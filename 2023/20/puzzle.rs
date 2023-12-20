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

fn resolve_one(input: &mut Input, rx_id: usize) -> (PulseCount, bool) {
    let mut worklist = VecDeque::new();
    worklist.push_back(Signal {
        from: 0,
        to: ID_BROADCASTER,
        is_high: false,
    });
    let mut signals = PulseCount { low: 1, high: 0 };
    let mut signaled_rx = false;
    while let Some(signal) = worklist.pop_front() {
        if signal.to == rx_id && !signal.is_high {
            signaled_rx = true;
        }
        match input.get_mut(&signal.to) {
            None => (),
            Some((module, children)) => signals += module.receive(children, signal, &mut worklist),
        }
    }
    (signals, signaled_rx)
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

fn parse(lines: Vec<String>) -> (Input, usize) {
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
    (ret, *strings.get("rx").unwrap())
}

fn main() {
    let lines = read_lines();
    let (mut input, rx_id) = parse(lines);
    let mut p1 = ZERO;
    let mut found_rx = usize::MAX;
    for i in 0..1000 {
        let (signals, signaled_rx) = resolve_one(&mut input, rx_id);
        p1 += signals;
        if signaled_rx && found_rx == usize::MAX {
            found_rx = i + 1;
        }
    }
    println!("Part 1 : {}", p1.low * p1.high);
    if found_rx == usize::MAX {
        let mut i = 1000;
        let mut found = false;
        while !found {
            let (_, signaled_rx) = resolve_one(&mut input, rx_id);
            found = signaled_rx;
            i += 1;
        }
        found_rx = i;
    }
    println!("Part 2 : {}", found_rx);
}
