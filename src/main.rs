use enigo::{Enigo, Key, KeyboardControllable};
use itertools::{izip, Itertools};
use pointer_deref::main::{get_process, Module};
use pointer_deref::main::{get_system, OpenedProcess};
use std::cmp::{min, Ordering};
use std::env::split_paths;
use std::ops::BitOr;
use std::time::{Duration, Instant};
use sysinfo::ProcessExt;

#[derive(Debug)]
struct Wall {
    side: u32,
    distance: u32,
    depth: u32,
}

#[derive(Debug)]
struct Timer {
    seconds: usize,
    frames: usize,
}

#[derive(Debug)]
struct GameState {
    angle: usize,
    frames: u32,
    walls: [Wall; 64],
}

fn mod_sub(a: usize, b: usize, m: usize) -> usize {
    (a + m - b) % m
}

fn mod_diff(a: usize, b: usize, m: usize) -> usize {
    mod_sub(a, b, m).min(mod_sub(b, a, m))
}

fn handle_action(
    enigo: &mut Enigo,
    module: &Module,
    angle: usize,
    (target_angle, is_dir_up): (usize, bool),
) {
    if is_dir_up {
        enigo.key_down(Key::LeftArrow);
        loop {
            let angle = get_angle(module);
            if mod_sub(angle, target_angle, 360) < 10 {
                enigo.key_up(Key::LeftArrow);
                return;
            }
        }
    } else {
        enigo.key_down(Key::RightArrow);
        loop {
            let angle = get_angle(module);
            if mod_sub(target_angle, angle, 360) < 10 {
                enigo.key_up(Key::RightArrow);
                return;
            }
        }
    }
}

struct VectorizedGameState {
    rows: Vec<(usize, [bool; 6])>,
}

#[derive(Eq, PartialEq)]
struct WallEvent {
    start: bool,
    side: usize,
    point: usize,
}

impl PartialOrd for WallEvent {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for WallEvent {
    fn cmp(&self, other: &Self) -> Ordering {
        (self.point.cmp(&other.point)).then(self.start.cmp(&other.start))
    }
}

impl VectorizedGameState {
    fn vectorize(game: &GameState) -> Self {
        let mut rows = vec![];

        let mut time = 0;
        let mut state = [false; 6];

        for wall in game
            .walls
            .iter()
            .filter(|wall| {
                !(wall.depth == 0  || wall.side >= 6 || wall.depth > 15000 || wall.distance + wall.depth < 150)
            })
            .map(|wall| {
                [
                    WallEvent {
                        start: true,
                        side: wall.side as usize,
                        point: (wall.distance as usize).saturating_sub(150),
                    },
                    WallEvent {
                        start: false,
                        side: wall.side as usize,
                        point: (wall.distance as usize + wall.depth as usize).saturating_sub(150),
                    },
                ]
                .into_iter()
            })
            .flatten()
            .sorted()
        {
            if wall.point > time + 40 {
                rows.push((time, state));
                time = wall.point;
            }

            state[wall.side] = wall.start;
        }
        // println!();
        rows.push((time, state));

        Self { rows }
    }

    fn display(&self) {
        for row in &self.rows {
            println!(
                "{:04} - {:?}",
                row.0,
                row.1.iter().cloned().map(u8::from).format("")
            );
        }
    }

    fn solve(&self, angle: usize, modulo: usize) -> Option<(usize, bool)> {
        const SPLIT_FACTOR: usize = 15;

        let mut side = [None; 6 * SPLIT_FACTOR];
        let mut costs = vec![[usize::MAX; 6 * SPLIT_FACTOR]; self.rows.len()];
        *costs.last_mut().unwrap() = [0; 6 * SPLIT_FACTOR];

        let modulo = modulo * SPLIT_FACTOR;

        for (i, (d, row)) in self.rows.iter().enumerate().rev().skip(1) {
            let row: [bool; 6 * SPLIT_FACTOR] = to_array(row.iter().map(|&x| [x; SPLIT_FACTOR].into_iter()).flatten());

            let mut unreachable = row;
            side = [None; 6 * SPLIT_FACTOR];

            if row[..modulo].iter().all(|x| *x) {
                costs[i] = [0; 6 * SPLIT_FACTOR];
                continue;
            }

            for (j, (next_d, next_row)) in self.rows.iter().enumerate().skip(i + 1) {
                let next_row: [bool; 6 * SPLIT_FACTOR] = to_array(next_row.iter().map(|&x| [x; SPLIT_FACTOR].into_iter()).flatten());

                // no more places are reachable
                if unreachable[..modulo].iter().all(|x| *x) {
                    break;
                }

                // check if we can reach certain angles
                let reachable_diff = (next_d - d) * modulo / 1500;

                // For each cell in the current row
                for k in 0..modulo {
                    if unreachable[k] {
                        continue;
                    }

                    // Allow moving straight
                    costs[i][k] = min(costs[i][k], costs[j][k]);

                    // Move in positive dir
                    for l in 1..min(modulo, reachable_diff + 1) {
                        // Calculate absolute l
                        let l_abs = (k + l) % modulo;

                        // If we can't rotate to here, break (means we also can't rotate further)
                        if unreachable[l_abs] {
                            break;
                        }

                        let new_cost = costs[j][l_abs].saturating_add(1);

                        if new_cost < costs[i][k] {
                            costs[i][k] = new_cost;
                            side[k] = Some((true, l_abs));
                        }
                    }

                    // Move in negative dir
                    for l in 1..min(modulo, reachable_diff + 1) {
                        let l_abs = (k + modulo - l) % modulo;

                        if unreachable[l_abs] {
                            break;
                        }

                        let new_cost = costs[j][l_abs].saturating_add(1);

                        if new_cost < costs[i][k] {
                            costs[i][k] = new_cost;
                            side[k] = Some((false, l_abs));
                        }
                    }
                }

                // determine next `unreachable`
                unreachable = to_array(unreachable.iter().zip(next_row.iter()).map(|(x, y)| *x || *y));
            }
        }

        // for costs in costs {
        //     println!("{:?}", costs.map(|c| (c == usize::MAX) as usize));
        // }
        //
        // println!("{}", modulo);
        // println!("{:?}", side);
        // println!("{:?}", angle_to_index(angle, modulo));
        // println!("{:?}", side[angle_to_index(angle, modulo)]);
        // println!("{}", index_to_angle(side[angle_to_index(angle, modulo)].0, modulo))

        side[angle_to_index(angle, modulo)].map(|(s, t)| (index_to_angle(t, modulo), s))
    }
}

fn index_to_angle(i: usize, modulo: usize) -> usize {
    (360 / modulo / 2 + i * 360 / modulo)
}

fn angle_to_index(angle: usize, modulo: usize) -> usize {
    modulo * angle / 360
}

fn to_array<const N: usize, T: Default + Clone + Copy>(mut i: impl Iterator<Item = T>) -> [T; N] {
    let mut arr = [T::default(); N];
    for (i, v) in i.enumerate() {
        arr[i] = v;
    }
    arr
}

fn main() {
    let mut system = get_system();

    let process =
        get_process(&mut system, "SuperHexagon.exe").expect("Super Hexagon should be running.");
    let process = OpenedProcess::new(process.pid());

    let module = process
        .list_modules()
        .find(|p| p.get_name().to_bytes() == b"superhexagon.exe")
        .unwrap();

    // sleep(Duration::from_secs(2));

    let mut enigo = Enigo::new();

    let mut last_print = Instant::now();
    let mut last_target_angle = None;
    loop {
        let angle = get_angle(&module);
        let frames: u32 = unsafe { module.read_deref::<f32>(0x15E8EC, [0x2924]).unwrap() } as u32;

        let walls = get_walls(&module);
        let modulo = get_modulo(&module);

        let game_state = GameState {
            angle,
            frames,
            walls,
        };

        let vectorized = VectorizedGameState::vectorize(&game_state);
        let target_angle = vectorized.solve(angle, modulo);

        if Instant::now() - last_print > Duration::from_millis(100)
            || last_target_angle != target_angle
        {
            last_print = Instant::now();
            last_target_angle = target_angle;
            vectorized.display();
            println!("{} -> {:?}", angle, target_angle);
            println!("{modulo}");
            // println!("{:?}", &game_state.walls);
            println!();
        }

        if let Some(target_angle) = target_angle {
            handle_action(&mut enigo, &module, angle, target_angle);
        }
        // thread::sleep(Duration::from_millis(250));
    }
}

fn get_walls(module: &Module) -> [Wall; 64] {
    izip!(get_sides(module), get_distances(module), get_depths(module))
        .map(|(side, distance, depth)| Wall {
            side,
            distance,
            depth,
        })
        .collect::<Vec<_>>()
        .try_into()
        .unwrap()
}

fn get_depths<'a>(module: &'a Module) -> impl Iterator<Item = u32> + 'a {
    (0x0218..)
        .step_by(0x14)
        .take(64)
        .map(|offset| unsafe { module.read_deref(0x15E8EC, [offset]).unwrap() })
}

fn get_distances<'a>(module: &'a Module) -> impl Iterator<Item = u32> + 'a {
    (0x0214..)
        .step_by(0x14)
        .take(64)
        .map(|offset| unsafe { module.read_deref(0x15E8EC, [offset]).unwrap() })
}

fn get_sides<'a>(module: &'a Module) -> impl Iterator<Item = u32> + 'a {
    (0x0210..)
        .step_by(0x14)
        .take(64)
        .map(|offset| unsafe { module.read_deref(0x15E8EC, [offset]).unwrap() })
}

fn get_angle(module: &Module) -> usize {
    unsafe { module.read_deref::<u32>(0x15E8EC, [0x297C]).unwrap() as usize }
}

fn get_modulo(module: &Module) -> usize {
    unsafe { module.read_deref::<u32>(0x15E8EC, [0x0198]).unwrap() as usize }
}

#[cfg(test)]
mod tests {
    use crate::VectorizedGameState;

    const F: bool = false;
    const T: bool = true;

    #[test]
    fn test1() {
        let gs: VectorizedGameState = VectorizedGameState {
            rows: vec![
                (0000, [F, F, F, F, F, F]),
                (1400, [F, T, T, T, T, T]),
                (1600, [F, F, F, F, F, F]),
                (2600, [T, F, T, T, F, T]),
                (2800, [F, F, F, F, F, F]),
                (3200, [F, T, T, F, T, T]),
                (3400, [F, F, F, F, F, F]),
            ],
        };
        let angle = 153;
        let res = gs.solve(angle, 6);
        assert!(res.is_some());
    }

    #[test]
    fn test2() {
        let gs: VectorizedGameState = VectorizedGameState {
            rows: vec![
                (0000, [F, F, F, F, F, F]),
                (1010, [T, T, T, T, T, F]),
                (1210, [F, F, F, F, F, F]),
                (2614, [T, T, T, T, T, F]),
                (2814, [F, T, T, T, T, F]),
                (3014, [F, F, T, T, T, F]),
                (3214, [F, F, F, T, T, F]),
                (3414, [F, F, F, F, T, T]),
                (3614, [F, F, F, F, F, T]),
                (3714, [F, F, F, F, F, F]),
            ],
        };
        let angle = 152;
        let res = gs.solve(angle, 6);
        assert!(res.is_some());
    }

    #[test]
    fn test3() {
        let gs: VectorizedGameState = VectorizedGameState {
            rows: vec![
                (0000, [F,F,F,F,F,F]),
                (2050, [F,T,T,T,T,T]),
                (2250, [F,F,T,T,T,T]),
                (2450, [F,F,F,T,T,T]),
                (2650, [F,F,F,F,T,T]),
                (2850, [T,F,F,F,F,T]),
                (3050, [T,T,F,F,F,F]),
                (3150, [F,T,F,F,F,F]),
                (3250, [F,T,T,F,F,F]),
                (3350, [F,F,T,F,F,F]),
                (3450, [F,F,T,T,F,F]),
                (3550, [F,F,F,T,F,F]),
                (3650, [F,F,F,T,T,F]),
                (3750, [F,F,F,F,T,F]),
                (3850, [F,F,F,F,T,T]),
                (3950, [F,F,F,F,F,T]),
                (4050, [T,F,F,F,F,T]),

                (4150, [T,F,F,F,F,F]),
                (4225, [T,T,F,F,F,F]),
                (4350, [F,T,F,F,F,F]),
                (4425, [F,T,T,F,F,F]),
                (4625, [F,T,T,T,F,F]),
                (4825, [F,T,T,T,T,F]),
                (5025, [F,T,T,T,T,T]),
                (5325, [F,F,F,F,F,F]),
            ],
        };
        let angle = 347;
        let res = gs.solve(angle, 6);
        assert!(res.is_some());
    }

    #[test]
    fn test4() {
        let gs: VectorizedGameState = VectorizedGameState {
            rows: vec![
                (0000, [F,F,F,F,F,F]),
                (0642, [T,F,T,T,T,F]),
                (0842, [F,F,F,F,F,F])
            ],
        };
        let angle = 308;
        let res = gs.solve(angle, 5);
        assert!(res.is_some());
    }

    #[test]
    fn test5() {
        let gs: VectorizedGameState = VectorizedGameState {
            rows: vec![
                (0000, [F,F,F,F,F,F]),
                (0546, [T,T,T,F,T,F]),
                (0746, [F,F,F,F,F,F]),
            ],
        };
        let angle = 184;
        let res = gs.solve(angle, 5);
        let Some((res, _)) = res else {
            panic!()
        };
        assert!(res >= 210 && res <= 282)
    }
}

// 0000 - 100000
// 0050 - 110000
// 0225 - 011000
// 0350 - 001000
// 0450 - 001100
// 0550 - 000100
// 0650 - 000110
// 0750 - 000010
// 0850 - 000011
// 0950 - 000001
// 1050 - 100001
// 1150 - 100000
// 1250 - 110000
// 1350 - 010000
// 1450 - 011000
// 1550 - 001000
// 1650 - 001100
// 1850 - 001110
// 2050 - 001111
// 2250 - 101111
// 2550 - 000000
// 251 -> Some((326, true))
// 6
//
// 0000 - 111000
// 0050 - 101000
// 0175 - 101100
// 0275 - 100100
// 0375 - 100110
// 0475 - 100010
// 0575 - 100011
// 0675 - 100001
// 0775 - 100001
// 0875 - 100000
// 0975 - 110000
// 1075 - 010000
// 1175 - 011000
// 1275 - 001000
// 1375 - 001100
// 1575 - 001110
// 1775 - 001111
// 1975 - 101111
// 2275 - 000000
// 328 -> None
// 6