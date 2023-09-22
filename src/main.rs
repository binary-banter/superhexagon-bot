use enigo::{Enigo, Key, KeyboardControllable};
use itertools::{izip, Itertools};
use pointer_deref::main::{get_process, Module};
use pointer_deref::main::{get_system, OpenedProcess};
use std::cmp::{min, Ordering};
use std::time::{Duration, Instant};
use sysinfo::{ProcessExt};

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
    if mod_diff(angle, target_angle, 360) < 10 {
        return;
    }

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
                !(wall.depth == 0 || wall.distance > 3500 || wall.side >= 6 || wall.depth > 15000)
            })
            .map(|wall| {
                [
                    WallEvent {
                        start: true,
                        side: wall.side as usize,
                        point: wall.distance as usize,
                    },
                    WallEvent {
                        start: false,
                        side: wall.side as usize,
                        point: wall.distance as usize + wall.depth as usize,
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
        let mut costs = [0usize; 6];
        let mut side = [None; 6];
        let mut last_d = self.rows.last().unwrap().0;

        for &(d, row) in self.rows.iter().rev().skip(1) {
            let delta_d = last_d - d;
            let reachable_diff = delta_d * 6 / 1500;

            let mut new_costs = [0; 6];
            for i in 0..6 {
                if row[i] {
                    new_costs[i] = 1000000000;
                    continue;
                };
                new_costs[i] = costs[i];
                side[i] = None;

                for j in 1..min(6, reachable_diff+1) {
                    let j_abs = (i + j) % 6;
                    if row[j_abs] { break; }
                    let new_cost = costs[j_abs] + 1;

                    if new_cost < new_costs[i] {
                        new_costs[i] = new_cost;
                        side[i] = Some((true, j_abs));
                    }
                }

                for j in 1..min(6, reachable_diff+1) {
                    let j_abs = (i + 6 - j) % 6;
                    if row[j_abs] { break; }
                    let new_cost = costs[j_abs] + 1;

                    if new_cost < new_costs[i] {
                        new_costs[i] = new_cost;
                        side[i] = Some((false, j_abs));
                    }
                }
            }
            // println!("{d} {reachable_diff} {row:?} - {new_costs:?}");

            costs = new_costs;
            last_d = d;
        }

        side[angle_to_index(angle, 6)].map(|(s, t)| (index_to_angle(t, 6), s))
    }
}

fn index_to_angle(i: usize, modulo: usize) -> usize{
    let angle = 360 / modulo;
    (angle/2 + angle * i)
}

fn angle_to_index(angle: usize, modulo: usize) -> usize{
    angle / (360 / modulo)
}

fn to_array<const N: usize, T: Default + Clone + Copy>(mut i: impl Iterator<Item=T>) -> [T; N] {
    let mut arr = [T::default(); N];
    for (i, v) in i.enumerate(){
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

    #[test]
    fn test() {
        const F: bool = false;
        const T: bool = true;
        let gs: VectorizedGameState = VectorizedGameState {
            rows: vec![
                (0000, [F,F,F,F,F,F]),
                (1400, [F,T,T,T,T,T]),
                (1600, [F,F,F,F,F,F]),
                (2600, [T,F,T,T,F,T]),
                (2800, [F,F,F,F,F,F]),
                (3200, [F,T,T,F,T,T]),
                (3400, [F,F,F,F,F,F]),
            ],
        };
        let angle = 153;
        let res = gs.solve(angle, 6);
        assert!(res.is_some());
    }
}