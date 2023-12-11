// ==== Puzzle 11 : https://adventofcode.com/2023/day/11 ====

use std::collections::HashSet;

#[derive(Clone, Debug, PartialEq)]
struct P2 {
    x: usize,
    y: usize,
}

fn absolute_diff(x: usize, y: usize) -> usize {
    if x > y {
        x - y
    } else {
        y - x
    }
}

impl P2 {
    fn dist_manhattan(&self, other: &Self) -> usize {
        absolute_diff(self.x, other.x) + absolute_diff(self.y, other.y)
    }
}

fn list_galaxies(input: &Vec<Vec<bool>>) -> Vec<P2> {
    input
        .iter()
        .enumerate()
        .flat_map(|(y, vec)| {
            vec.iter()
                .enumerate()
                .filter(|(_, b)| **b)
                .map(move |(x, _)| P2 { x, y })
        })
        .collect()
}

fn shift_galaxy(galaxy: &P2, rows: &HashSet<usize>, columns: &HashSet<usize>, mult: usize) -> P2 {
    P2 {
        x: galaxy.x + mult * rows.iter().filter(|p| p < &&galaxy.x).count(),
        y: galaxy.y + mult * columns.iter().filter(|p| p < &&galaxy.y).count(),
    }
}

fn all_distances(galaxies: Vec<P2>) -> usize {
    let mut total = 0;
    for (i, gal) in galaxies.iter().enumerate() {
        for j in 0..i {
            total += gal.dist_manhattan(&galaxies[j]);
        }
    }
    total
}

fn parse_line(line: String) -> Vec<bool> {
    line.chars().map(|c| c == '#').collect()
}

fn read_lines() -> Vec<Vec<bool>> {
    std::io::stdin()
        .lines()
        .filter_map(|x| x.ok())
        .map(parse_line)
        .collect()
}

fn main() {
    let input = read_lines();
    let galaxies = list_galaxies(&input);
    let columns: HashSet<_> = galaxies.iter().map(|p| p.y).collect();
    let co_columns: HashSet<_> = (0..*columns.iter().max().unwrap())
        .filter(|c| !columns.contains(c))
        .collect();

    let rows: HashSet<_> = galaxies.iter().map(|p| p.x).collect();
    let co_rows: HashSet<_> = (0..*rows.iter().max().unwrap())
        .filter(|c| !rows.contains(c))
        .collect();

    let p2 = all_distances(
        galaxies
            .iter()
            .map(|galaxy| shift_galaxy(galaxy, &co_rows, &co_columns, 999999)) // mult is -1
            .collect(),
    );
    let p1 = all_distances(
        galaxies
            .iter()
            .map(|galaxy| shift_galaxy(galaxy, &co_rows, &co_columns, 1))
            .collect(),
    );

    println!("Part 1 : {}", p1);
    println!("Part 2 : {}", p2);
}
