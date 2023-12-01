// Puzzle 01 : https://adventofcode.com/2023/day/1

const PART_TWO: bool = true;

fn parse_file_lines() -> Vec<String> {
    let mut file_lines: Vec<String> = Vec::new();
    for line in std::io::stdin().lines() {
        match line {
            Ok(l) => file_lines.push(l),
            Err(_) => panic!(),
        }
    }
    return file_lines;
}

fn char_first(str: &String) -> char {
    for c in str.chars() {
        if c.is_digit(10) {
            return c;
        }
    }
    panic!("No number on line");
}

fn char_last(str: &String) -> char {
    for c in str.chars().rev() {
        if c.is_digit(10) {
            return c;
        }
    }
    panic!("No number on line");
}

fn main() {
    let lines = parse_file_lines();
    let mut sum = 0;
    for mut line in lines {
        if PART_TWO {
            // there can be overlap, like oneight
            line = line.replace("one", "o1e");
            line = line.replace("two", "t2o");
            line = line.replace("three", "t3e");
            line = line.replace("four", "f4r");
            line = line.replace("five", "f5e");
            line = line.replace("six", "s6x");
            line = line.replace("seven", "s7n");
            line = line.replace("eight", "e8t");
            line = line.replace("nine", "n9e");
        }
        match char_first(&line).to_digit(10) {
            Some(digit) => sum += 10 * digit,
            None => panic!("Not a digit"),
        };
        match char_last(&line).to_digit(10) {
            Some(digit) => sum += digit,
            None => panic!("Not a digit"),
        };
    }
    println!("{}", sum);
}
