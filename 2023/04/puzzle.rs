// ==== Puzzle 04 : https://adventofcode.com/2023/day/4 ====

// use std::collections::HashMap;
use regex::Regex;
use std::cmp::min;

struct Game {
    _number: usize,
    winning_numbers: Vec<i32>,
    present_numbers: Vec<i32>,
}

fn nb_matches(game: &Game) -> usize {
    return game.present_numbers.iter().filter(|c| game.winning_numbers.contains(c)).count();
}

fn parse_list(list: &str) -> Vec<i32> {
    let mut res = Vec::new();
    let re = Regex::new(r"\d+").unwrap();
    for capture in re.captures_iter(list) {
        res.push(capture.get(0).unwrap().as_str().parse().unwrap())
    }
    return res;
}

fn parse_game(line: &str) -> Game {
    let re = Regex::new(r"Card\s+(\d+):\s+(.*?)\s+\|\s+(.*?)$").unwrap();
    let capture = re.captures(line).unwrap();
    return Game {
        _number: capture.get(1).unwrap().as_str().parse().unwrap(),
        winning_numbers: parse_list(capture.get(2).unwrap().as_str()),
        present_numbers: parse_list(capture.get(3).unwrap().as_str())
    };
}

fn read_lines() -> Vec<Game> {
    let mut file_lines = Vec::new();
    for line in std::io::stdin().lines() {
        match line {
            Ok(l) => file_lines.push(parse_game(&l)),
            Err(_) => panic!(),
        }
    }
    return file_lines;
}

// fn get_amount(map: &HashMap<i32,i32>, nb: i32) -> i32 {
//     match map.get(&nb) {
//         Some(n) => *n,
//         None => 1
//     }
// }

// fn add_amount(map: &mut HashMap<i32,i32>, nb: i32, amount: i32) {
//     let n = get_amount(map, nb);
//     map.insert(nb, n+amount);
// }

fn main() {
    let lines = read_lines();
    let mut sum = 0;
    let mut sum2 = 0;

    // for game in lines {
    //     sum += (1 << nb_matches(&game)) >> 1;
    // }
    let mut amounts: Vec<i32> = lines.iter().map(|_| 1).collect();
    let len = lines.len();
    for (i, game) in lines.iter().enumerate() {
        let matches = nb_matches(&game);
        sum += (1 << matches) >> 1;
        sum2 += amounts[i];
        // println!("{} : {} : {}", game._number, matches, amounts[i]);
        for j in (i+1)..(min(i+1+matches, len)) {
            amounts[j] += amounts[i]*1;
        }
    }

    println!("Part 1: {}", sum);
    println!("Part 2: {}", sum2);
}
