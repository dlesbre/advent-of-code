// ==== Puzzle 20 : https://adventofcode.com/2023/day/20 ====

use std::collections::{HashMap, VecDeque};
use std::ops::AddAssign;

#[derive(Clone)]
enum Module {
    FlipFlop(bool),
    Conjunction(HashMap<usize, bool>),
    Broadcaster,
}

#[derive(Clone)]
struct Signal {
    from: usize,
    to: usize,
    is_high: bool,
}

struct PulseCount {
    low: usize,
    high: usize,
}

impl AddAssign for PulseCount {
    fn add_assign(&mut self, rhs: Self) {
        self.low += rhs.low;
        self.high += rhs.high;
    }
}
impl PulseCount {
    fn make(count: usize, is_high: bool) -> PulseCount {
        if is_high {
            PulseCount {
                low: 0,
                high: count,
            }
        } else {
            PulseCount {
                low: count,
                high: 0,
            }
        }
    }
}

const ZERO: PulseCount = PulseCount { low: 0, high: 0 };

impl Module {
    fn receive(
        &mut self,
        children: &Vec<usize>,
        signal: Signal,
        worklist: &mut VecDeque<Signal>,
    ) -> PulseCount {
        match self {
            Module::FlipFlop(pulse) => {
                if !signal.is_high {
                    *pulse = !*pulse;
                    worklist.extend(children.iter().map(|&to| Signal {
                        from: signal.to,
                        to,
                        is_high: *pulse,
                    }));
                    return PulseCount::make(children.len(), *pulse);
                };
                return ZERO;
            }
            Module::Conjunction(map) => {
                map.insert(signal.from, signal.is_high);
                if map.values().all(|x| *x) {
                    worklist.extend(children.iter().map(|&to| Signal {
                        from: signal.to,
                        to,
                        is_high: false,
                    }));
                    return PulseCount::make(children.len(), false);
                } else {
                    worklist.extend(children.iter().map(|&to| Signal {
                        from: signal.to,
                        to,
                        is_high: true,
                    }));
                    return PulseCount::make(children.len(), true);
                };
            }
            Module::Broadcaster => {
                worklist.extend(children.iter().map(|&to| Signal {
                    from: signal.to,
                    to,
                    is_high: signal.is_high,
                }));
                return PulseCount::make(children.len(), signal.is_high);
            }
        }
    }
}

const ID_BROADCASTER: usize = 1;

type Input = HashMap<usize, (Module, Vec<usize>)>;

/// Resolve a single button press, updating the state and counting pulses
fn resolve_one(input: &mut Input) -> PulseCount {
    let mut worklist = VecDeque::new();
    worklist.push_back(Signal {
        from: 0,
        to: ID_BROADCASTER,
        is_high: false,
    });
    let mut signals = PulseCount { low: 1, high: 0 };
    while let Some(signal) = worklist.pop_front() {
        match input.get_mut(&signal.to) {
            None => (),
            Some((module, children)) => signals += module.receive(children, signal, &mut worklist),
        }
    }
    signals
}

/// Number of button press needed to send signal that satisfies f
fn presses_to_signal<F: Fn(&Signal) -> bool>(input: &mut Input, f: F) -> usize {
    let mut count = 1;
    loop {
        let mut worklist = VecDeque::new();
        worklist.push_back(Signal {
            from: 0,
            to: ID_BROADCASTER,
            is_high: false,
        });
        while let Some(signal) = worklist.pop_front() {
            if f(&signal) {
                return count;
            }
            match input.get_mut(&signal.to) {
                None => (),
                Some((module, children)) => {
                    module.receive(children, signal, &mut worklist);
                }
            }
        }
        count += 1;
    }
}

fn read_lines() -> Vec<String> {
    std::io::stdin().lines().filter_map(|x| x.ok()).collect()
}

fn find_number<'a>(nb: &mut usize, map: &mut HashMap<&'a str, usize>, string: &'a str) -> usize {
    match map.get(string) {
        Some(n) => *n,
        None => {
            let x = *nb;
            *nb = x + 1;
            map.insert(string, x);
            x
        }
    }
}

fn parents(input: &Input, id: usize) -> Vec<usize> {
    let mut parents = Vec::new();
    for (keys, (_, children)) in input {
        if children.contains(&id) {
            parents.push(*keys);
        }
    }
    parents
}

fn parse(lines: Vec<String>) -> (Input, Option<usize>) {
    let mut number = ID_BROADCASTER + 1;
    let mut strings = HashMap::new();
    let mut ret = HashMap::new();
    for line in &lines {
        let mut s = line.split(" -> ");
        let name = s.next().unwrap();
        let (id, module) = if name.starts_with("%") {
            (
                find_number(&mut number, &mut strings, &name[1..]),
                Module::FlipFlop(false),
            )
        } else if name.starts_with("&") {
            (
                find_number(&mut number, &mut strings, &name[1..]),
                Module::Conjunction(HashMap::new()),
            )
        } else {
            assert!(name == "broadcaster");
            (ID_BROADCASTER, Module::Broadcaster)
        };

        let mut outgoing = Vec::new();
        for child in s.next().unwrap().split(", ") {
            let nb = find_number(&mut number, &mut strings, child);
            outgoing.push(nb);
        }
        ret.insert(id, (module, outgoing));
    }
    for (key, (_, values)) in ret.clone() {
        for val in values {
            match ret.get_mut(&val) {
                Some((Module::Conjunction(map), _)) => {
                    map.insert(key, false);
                }
                _ => (),
            }
        }
    }
    (ret, strings.get("rx").map(|x| *x))
}

fn gcd(a: usize, b: usize) -> usize {
    if b == 0 {
        return a;
    }
    gcd(b, a % b)
}

fn lcm(a: usize, b: usize) -> usize {
    (a / gcd(a, b)) * b
}

fn main() {
    let lines = read_lines();
    let (input, rx_id) = parse(lines);
    let mut input1 = input.clone();
    let mut p1 = ZERO;
    for _ in 0..1000 {
        p1 += resolve_one(&mut input1);
    }
    println!("Part 1 : {}", p1.low * p1.high);
    if rx_id == None {
        return;
    }
    let rx_id = rx_id.unwrap();
    // my input only has on parent of rx
    // which is a conjuction
    let x = parents(&input, rx_id).pop().unwrap();
    let mut time_to_high = 1;
    for y in parents(&input, x) {
        time_to_high = lcm(
            time_to_high,
            presses_to_signal(&mut input.clone(), |x| x.from == y && x.is_high),
        );
    }
    println!("Part 2 : {}", time_to_high);
}
