use enigo::{Enigo, Key, KeyboardControllable};
use itertools::{izip, Itertools};
use pointer_deref::main::{get_process, Module};
use pointer_deref::main::{get_system, OpenedProcess};
use std::cmp::Ordering;
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
                "{} - {:?}",
                row.0,
                row.1.iter().cloned().map(u8::from).format("")
            );
        }
    }

    fn solve(&self, angle: usize, modulo: usize) -> Option<(usize, bool)> {
        if self.rows.len() < 2 {
            return None;
        }

        let (t0, row0) = self.rows[0];
        assert_eq!(t0, 0);
        let (t1, row1) = self.rows[1];

        let cur_i = angle / (360 / modulo);

        let set = match modulo {
            6 => vec![30, 90, 150, 210, 270, 330],
            5 => vec![36, 108, 180, 252, 324],
            4 => vec![45, 135, 225, 315],
            _ => panic!("wut"),
        }.into_iter().enumerate(); //.sorted_by_key(|(_, x)|mod_diff(angle, *x as usize, 360));

        for (test_i, test_angle) in set {
            let reachable_diff = t1 * 360 / 1500;

            if row1[test_i] {
                continue;
            }

            // If we go via increasing, can we make it?
            if mod_sub(test_angle, angle, 360) < reachable_diff {
                if (0..=mod_sub(test_i, cur_i, modulo))
                    .map(|a| (a + cur_i) % modulo)
                    .all(|i| !row0[i])
                {
                    return Some((test_angle, true));
                }
            }

            // If we go via decreasing, can we make it?
            if mod_sub(angle, test_angle, 360) < reachable_diff {
                if (0..=mod_sub(cur_i, test_i, modulo))
                    .map(|a| (a + test_i) % modulo)
                    .all(|i| !row0[i])
                {
                    return Some((test_angle, false));
                }
            }
        }

        None
    }
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
