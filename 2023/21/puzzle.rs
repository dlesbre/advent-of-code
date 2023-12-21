// ==== Puzzle 21 : https://adventofcode.com/2023/day/21 ====

#[derive(PartialEq, Eq, Clone)]
enum Tile {
    Rock,
    GardenUnreached,
    GardenReached,
}

use std::usize;

use Tile::*;

fn parse_tile(c: char) -> Tile {
    match c {
        '.' => GardenUnreached,
        'S' => GardenReached,
        '#' => Rock,
        _ => panic!("Unrecognized tile"),
    }
}

fn parse(s: String) -> Vec<Tile> {
    s.chars().map(parse_tile).collect()
}

fn read_lines() -> Vec<Vec<Tile>> {
    std::io::stdin()
        .lines()
        .filter_map(|x| x.ok())
        .map(parse)
        .collect()
}

fn find_start(lines: &Vec<Vec<Tile>>) -> (usize, usize) {
    for (y, line) in lines.iter().enumerate() {
        for (x, s) in line.iter().enumerate() {
            if *s == GardenReached {
                return (x, y);
            }
        }
    }
    panic!("No start tile");
}

// fn modulo(a: isize, b: usize) -> isize {
//     let b = b as isize;
//     let m = a % b;
//     if m < 0 {
//         m + b
//     } else {
//         m
//     }
// }

fn step(map: &mut Vec<Vec<Tile>>, reached: Vec<(usize, usize)>) -> Vec<(usize, usize)> {
    for (x, y) in &reached {
        map[*y][*x] = GardenUnreached;
    }
    let mut next = Vec::with_capacity(reached.len() * 4);
    for (x, y) in reached {
        if x > 0 && map[y][x - 1] == GardenUnreached {
            map[y][x - 1] = GardenReached;
            next.push((x - 1, y));
        }
        if y > 0 && map[y - 1][x] == GardenUnreached {
            map[y - 1][x] = GardenReached;
            next.push((x, y - 1));
        }
        if x + 1 < map[0].len() && map[y][x + 1] == GardenUnreached {
            map[y][x + 1] = GardenReached;
            next.push((x + 1, y));
        }
        if y + 1 < map.len() && map[y + 1][x] == GardenUnreached {
            map[y + 1][x] = GardenReached;
            next.push((x, y + 1));
        }
    }
    return next;
}

fn do_n_steps(input: &Vec<Vec<Tile>>, x: usize, y: usize, nb: usize) -> usize {
    let mut reached = vec![(x, y)];
    let mut input = (*input).clone();
    for _ in 0..nb {
        reached = step(&mut input, reached);
    }
    return reached.len();
}

fn main() {
    let mut input = read_lines();
    let (start_x, start_y) = find_start(&input);
    input[start_y][start_x] = GardenUnreached;

    println!("Part 1 : {}", do_n_steps(&input, start_x, start_y, 64));

    const STEPS: usize = 26501365;

    //                  N
    //                C | C
    //              T - S - T
    //            C |   |   | C
    //          T - S - F - S - T
    //        C |   |   |   |   | C
    //      W - S - F - S - F - S - E
    //        C |   |   |   |   | C
    //          T - S - F - S - T
    //            C |   |   | C
    //              T - S - T
    //                C | C
    //                  S
    //
    // Where: N, S, E, W is north/south/east/west square
    // S is square with same parity as Start, F is square with flipped parity
    // T is triangular filled square (4 different type)
    // C is triangular truncation (removed from a full odd or even square)
    // also 4 different types
    //
    // Let D be number of dashes between center and any cardinal
    // Nb(C) = 4 * D        (so there are D instances of each subplot)
    // Nb(T) = 4 * (D - 1)  (so there are D-1 instances of each subplot)
    // Notice that the 1s fill a square with side Nb(T) / 4
    // And the Ss fill a square with side Nb(C) / 4
    // The result actually depends on parity of D:
    // Nb(F) = D * D  if D even, (D-1) * (D-1) otherwise
    // Nb(S) = D * D  if D odd, (D-1) * (D-1) otherwise

    assert!(input.len() == input[0].len());
    let input_size = input.len();

    // D is (steps - step_in_center) / input_size
    let diamond_width = (STEPS - input_size / 2) / input_size; // D

    let nb_triangular = diamond_width; // Nb(T) / 4
    let nb_truncated = usize::saturating_sub(diamond_width, 1); // Nb(C) / 4
    let (nb_same_parity, nb_flipped_parity) = if diamond_width % 2 == 1 {
        // Nb(S) and Nb(F)
        (diamond_width * diamond_width, nb_truncated * nb_truncated)
    } else {
        (nb_truncated * nb_truncated, diamond_width * diamond_width)
    };

    let same_parity_plots = do_n_steps(&input, start_x, start_y, input_size * 2 + STEPS % 2);
    let flipped_parity_plots = do_n_steps(&input, start_x, start_y, input_size * 2 + 1 - STEPS % 2);

    // Nb of steps after last boundary (into cardinal squares and T squares)
    let boundary_steps = (STEPS - input_size / 2 - 1) % input_size;
    let north_plots = do_n_steps(&input, start_x, input_size - 1, boundary_steps);
    let south_plots = do_n_steps(&input, start_x, 0, boundary_steps);
    let east_plots = do_n_steps(&input, 0, start_y, boundary_steps);
    let west_plots = do_n_steps(&input, input_size - 1, start_y, boundary_steps);

    // Nb of steps into triangular zone
    let triangular_steps = (STEPS - 2 * (input_size / 2 + 1)) % input_size; //boundary_steps - input_size / 2;
    let ne_t_plots = do_n_steps(&input, 0, input_size - 1, triangular_steps);
    let nw_t_plots = do_n_steps(&input, input_size - 1, input_size - 1, triangular_steps);
    let se_t_plots = do_n_steps(&input, 0, 0, triangular_steps);
    let sw_t_plots = do_n_steps(&input, input_size - 1, 0, triangular_steps);

    // Nb of steps into truncated zone
    let truncated_steps = triangular_steps + input_size;
    let ne_c_plots = do_n_steps(&input, 0, input_size - 1, truncated_steps);
    let nw_c_plots = do_n_steps(&input, input_size - 1, input_size - 1, truncated_steps);
    let se_c_plots = do_n_steps(&input, 0, 0, truncated_steps);
    let sw_c_plots = do_n_steps(&input, input_size - 1, 0, truncated_steps);

    let p2 = nb_same_parity * same_parity_plots
        + nb_flipped_parity * flipped_parity_plots
        + north_plots
        + south_plots
        + east_plots
        + west_plots
        + nb_triangular * (ne_t_plots + nw_t_plots + se_t_plots + sw_t_plots)
        + nb_truncated * (ne_c_plots + nw_c_plots + se_c_plots + sw_c_plots);
    println!("Part 2 : {}", p2)
}
