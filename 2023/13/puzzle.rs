// ==== Puzzle 13 : https://adventofcode.com/2023/day/13 ====

use std::cmp::min;

fn read_lines() -> Vec<String> {
    std::io::stdin().lines().filter_map(|x| x.ok()).collect()
}

// Split lines on empty strings
fn split_grids(input: Vec<String>) -> Vec<Vec<String>> {
    let mut total = Vec::new();
    let mut partial = Vec::new();
    for line in input {
        if line == "" {
            total.push(partial);
            partial = Vec::new();
        } else {
            partial.push(line);
        }
    }
    total.push(partial);
    return total;
}

fn is_power_of_two(n: usize) -> bool {
    n != 0 && (n & (n - 1) == 0)
}

fn check_symmetry(ints: Vec<usize>, part2: &bool) -> Option<usize> {
    for i in 0..(ints.len() - 1) {
        let ip = i + 1;
        // 0 <= j < i+1 => 0 <= i - j <= i
        // 0 <= j < len-i-1 => i+1 <= i+1+j < len
        let mut valid = true;
        let mut flipped = *part2;
        for j in 0..(min(ip, ints.len() - ip)) {
            if ints[i - j] == ints[ip + j] {
                continue;
            }
            if flipped && is_power_of_two(ints[i - j] ^ ints[ip + j]) {
                flipped = !flipped;
                continue;
            }
            valid = false;
            break;
        }
        if valid && (!part2 || *part2 == !flipped) {
            return Some(ip);
        }
    }
    return None;
}

fn parse_int<'a, T: IntoIterator<Item = &'a u8>>(x: T) -> usize {
    let mut nb = 0;
    for c in x {
        nb <<= 1;
        if *c == ('#' as u8) {
            nb |= 1;
        }
    }
    return nb;
}

fn find_symmetry(v: &Vec<String>, part2: &bool) -> usize {
    let ints: Vec<usize> = v.iter().map(|c| parse_int(c.as_bytes().iter())).collect();
    if let Some(x) = check_symmetry(ints, part2) {
        return 100 * x;
    }

    let v_bytes: Vec<&[u8]> = v.iter().map(|c| c.as_bytes()).collect();
    let ints: Vec<usize> = (0..(v[0].len()))
        .map(|i| parse_int(v_bytes.iter().map(|v| &v[i])))
        .collect();
    match check_symmetry(ints, part2) {
        Some(x) => x,
        None => panic!("No symmetry found!"),
    }
}

fn main() {
    let input = split_grids(read_lines());
    let p1: usize = input.iter().map(|v| find_symmetry(v, &false)).sum();
    println!("Part 1 : {}", p1);
    let p2: usize = input.iter().map(|v| find_symmetry(v, &true)).sum();
    println!("Part 1 : {}", p2);
}
