// ==== Puzzle 03 : https://adventofcode.com/2023/day/3 ====

use std::cmp::min;

use regex::Regex;

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

struct SchemaPart {
    object: Option<i32>, // Some for numbers, None for gears
    line: usize,         // starts at 0
    column_start: usize, // starts at 0
    column_end: usize,
}

fn all_numbers(lines: &Vec<String>) -> Vec<SchemaPart> {
    let nb = Regex::new(r"(\d+|\*)").unwrap();
    let mut found = Vec::with_capacity(lines.len() * 10);
    for (y, line) in lines.iter().enumerate() {
        for capture in nb.captures_iter(line) {
            let matc = capture.get(0).unwrap();
            let str = matc.as_str();
            let object = if str == "*" {
                None
            } else {
                Some(str.parse().unwrap())
            };
            found.push(SchemaPart {
                object,
                line: y,
                column_start: matc.start(),
                column_end: matc.end(),
            })
        }
    }
    return found;
}

fn check_neighbors(lines: &Vec<String>, number: &SchemaPart) -> bool {
    let x_start = usize::saturating_sub(number.column_start, 1);
    let x_end = min(number.column_end + 1, lines[0].len() - 1);

    let y_start = usize::saturating_sub(number.line, 1);
    let y_end = min(number.line + 1, lines.len() - 1) + 1;
    // println!(
    //     "Checking: {} at {}--{} {}--{}",
    //     number.number, x_start, x_end, y_start, y_end
    // );
    for line in y_start..y_end {
        if lines[line]
            .get(x_start..x_end)
            .unwrap()
            .chars()
            .any(|chr| chr != '.' && !(chr.is_digit(10)))
        {
            return true;
        }
    }
    return false;
}

fn gear_ratio(gear: &SchemaPart, parts: &Vec<SchemaPart>) -> i32 {
    let mut found = 0;
    let mut prod = 1;
    for part in parts {
        match part.object {
            None => (),
            Some(n) => {
                let x_start = usize::saturating_sub(part.column_start, 1);
                let x_end = part.column_end + 1;

                let y_start = usize::saturating_sub(part.line, 1);
                let y_end = part.line + 2;
                // println!(
                //     "Checking with: {} at {}--{} {}--{}",
                //     n, x_start, x_end, y_start, y_end
                // );
                if (x_start..x_end).contains(&gear.column_start)
                    && (y_start..y_end).contains(&gear.line)
                {
                    prod *= n;
                    found += 1;
                }
            }
        }
    }
    if found == 2 {
        return prod;
    }
    return 0;
}
fn main() {
    let lines = read_lines();
    let parts = all_numbers(&lines);
    let mut sum = 0;
    let mut ratio = 0;
    for part in &parts {
        match part.object {
            Some(n) => {
                if check_neighbors(&lines, &part) {
                    sum += n;
                }
            }
            None => ratio += gear_ratio(&part, &parts),
        }
    }
    println!("Sum: {}", sum);
    println!("Ratio: {}", ratio);
}
