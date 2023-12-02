// Puzzle 01 : https://adventofcode.com/2023/day/1

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

fn char_first(str: &str) -> u32 {
    str.chars()
        .find(|c| c.is_digit(10))
        .unwrap()
        .to_digit(10)
        .unwrap()
}

fn char_last(str: &str) -> u32 {
    str.chars()
        .rev()
        .find(|c| c.is_digit(10))
        .unwrap()
        .to_digit(10)
        .unwrap()
}

fn main() {
    let lines = parse_file_lines();
    let mut sum = 0;
    for mut line in lines {
        if cfg!(part = "2") {
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
        sum += 10 * char_first(&line) + char_last(&line);
    }
    println!("{}", sum);
}
