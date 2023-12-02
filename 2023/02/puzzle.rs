// ==== Puzzle 02 : https://adventofcode.com/2023/day/2 ====

extern crate regex;

use regex::Regex;
use std::{cmp::Ordering, fmt};

fn read_lines() -> Vec<String> {
    let mut file_lines: Vec<String> = Vec::new();
    for line in std::io::stdin().lines() {
        match line {
            Ok(l) => file_lines.push(l),
            Err(_) => panic!(),
        }
    }
    return file_lines;
}

#[derive(Eq, PartialEq, Debug)]
struct Sample {
    red: i32,
    green: i32,
    blue: i32,
}

struct Game {
    id: i32,
    samples: Vec<Sample>,
}

fn parse_sample(line: &str) -> Vec<Sample> {
    let re = Regex::new(r"([0-9]+) (red|green|blue)").unwrap();
    let mut samples = Vec::new();
    for sample in line.split("; ") {
        let mut blue = 0;
        let mut red = 0;
        let mut green = 0;
        for capture in re.captures_iter(sample) {
            match capture.extract() {
                (_, [n, "red"]) => red += n.parse::<i32>().unwrap(),
                (_, [n, "green"]) => green += n.parse::<i32>().unwrap(),
                (_, [n, "blue"]) => blue += n.parse::<i32>().unwrap(),
                (_, _) => panic!("Unreachable"),
            }
        }
        samples.push(Sample { red, green, blue })
    }
    return samples;
}

fn parse_lines(lines: Vec<String>) -> Vec<Game> {
    let re = Regex::new(r"Game ([0-9]+): (.+)$").unwrap();
    let mut games = Vec::new();
    for line in lines {
        let capture = re.captures(&line).unwrap();
        let id = capture.get(1).unwrap().as_str().parse().unwrap();
        let game = Game {
            id,
            samples: parse_sample(capture.get(2).unwrap().as_str()),
        };
        games.push(game);
    }
    return games;
}

impl fmt::Display for Sample {
    // This trait requires `fmt` with this exact signature.
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // Write strictly the first element into the supplied output
        // stream: `f`. Returns `fmt::Result` which indicates whether the
        // operation succeeded or failed. Note that `write!` uses syntax which
        // is very similar to `println!`.
        write!(
            f,
            "{{red:{}, green:{}, blue:{}}}",
            self.red, self.green, self.blue
        )
    }
}

fn and_ordering(l: Option<Ordering>, r: Option<Ordering>) -> Option<Ordering> {
    match (l, r) {
        (Some(Ordering::Less), Some(Ordering::Greater))
        | (Some(Ordering::Greater), Some(Ordering::Less))
        | (None, _)
        | (_, None) => None,
        (Some(Ordering::Less), _) | (_, Some(Ordering::Less)) => Some(Ordering::Less),
        (Some(Ordering::Greater), _) | (_, Some(Ordering::Greater)) => Some(Ordering::Greater),
        (Some(Ordering::Equal), Some(Ordering::Equal)) => Some(Ordering::Equal),
    }
}

impl PartialOrd for Sample {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        // println!("Comparing {} and {}", self, other);
        and_ordering(
            PartialOrd::partial_cmp(&self.red, &other.red),
            and_ordering(
                PartialOrd::partial_cmp(&self.green, &other.green),
                PartialOrd::partial_cmp(&self.blue, &other.blue),
            ),
        )
    }
}

fn power(game: &Vec<Sample>) -> i32 {
    game.iter().map(|c| c.red).max().unwrap()
        * game.iter().map(|c| c.green).max().unwrap()
        * game.iter().map(|c| c.blue).max().unwrap()
}

fn main() {
    let lines = read_lines();
    let games = parse_lines(lines);
    let reference = Sample {
        red: 12,
        green: 13,
        blue: 14,
    };
    let mut sum = 0;
    let mut pow = 0;
    for game in games {
        if game.samples.iter().all(|x| x <= &reference) {
            sum += game.id;
        }
        pow += power(&game.samples);
    }
    println!("Sum: {}", sum);
    println!("Power: {}", pow);
}
