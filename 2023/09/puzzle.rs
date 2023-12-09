// ==== Puzzle 09 : https://adventofcode.com/2023/day/9 ====

fn parse_line(line: String) -> Vec<i64> {
    line.split(" ").map(|n| n.parse().unwrap()).collect()
}

fn read_lines() -> Vec<Vec<i64>> {
    std::io::stdin()
        .lines()
        .filter_map(|x| x.ok())
        .map(parse_line)
        .collect()
}

fn differences(line: &Vec<i64>) -> Vec<i64> {
    (0..(line.len() - 1))
        .map(|i| line[i + 1] - line[i])
        .collect()
}

fn extrapolate_last(line: &Vec<i64>) -> i64 {
    let diff = differences(line);
    if diff.iter().all(|x| *x == 0) {
        *line.last().unwrap()
    } else {
        let i = extrapolate_last(&diff);
        line.last().unwrap() + i
    }
}

fn extrapolate_first(line: &Vec<i64>) -> i64 {
    let diff = differences(line);
    if diff.iter().all(|x| *x == 0) {
        line[0]
    } else {
        let i = extrapolate_first(&diff);
        line[0] - i
    }
}

fn main() {
    let lines = read_lines();
    let part1: i64 = lines.iter().map(extrapolate_last).sum();
    println!("Part 1 : {}", part1);
    let part2: i64 = lines.iter().map(extrapolate_first).sum();
    println!("Part 1 : {}", part2);
}
