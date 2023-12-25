// ==== Puzzle 24 : https://adventofcode.com/2023/day/24 ====

struct P3 {
    x: i64,
    y: i64,
    z: i64,
}

extern crate fraction;
type F = fraction::BigFraction;

// impl P3 {
//     fn cross(&self, other: &Self) -> P3 {
//         P3 {
//             x: self.y * other.z - self.z * other.y,
//             y: self.z * other.x - self.x * other.z,
//             z: self.x * other.y - self.y * other.x,
//         }
//     }

//     fn sub(&self, other: &Self) -> P3 {
//         P3 {
//             x: self.x - other.x,
//             y: self.y - other.y,
//             z: self.z - other.z,
//         }
//     }
// }

struct Stone {
    position: P3,
    velocity: P3,
}

// impl Stone {
//     fn at(&self, t: i64) -> P3 {
//         P3 {
//             x: self.position.x + t * self.velocity.x,
//             y: self.position.y + t * self.velocity.y,
//             z: self.position.z + t * self.velocity.z,
//         }
//     }

//     fn from_two(a: P3, t_a: i64, b: P3, t_b: i64) -> Option<Stone> {
//         let dt = t_a - t_b;
//         let vx = a.x - b.x;
//         if (vx % dt != 0) {
//             return None;
//         }
//         let vx = vx / dt;
//         let vy = a.y - b.y;
//         if (vy % dt != 0) {
//             return None;
//         }
//         let vy = vy / dt;
//         let vz = a.z - b.z;
//         if (vz % dt != 0) {
//             return None;
//         }
//         let vz = vz / dt;
//         Some(Stone {
//             velocity: P3 {
//                 x: vx,
//                 y: vy,
//                 z: vz,
//             },
//             position: P3 {
//                 x: a.x - t_a * vx,
//                 y: a.y - t_a * vy,
//                 z: a.z - t_a * vz,
//             },
//         })
//     }
// }

fn parse_p3(string: &str) -> P3 {
    let mut iter = string.split(", ");
    let x = iter.next().unwrap().trim().parse().unwrap();
    let y = iter.next().unwrap().trim().parse().unwrap();
    let z = iter.next().unwrap().trim().parse().unwrap();
    P3 { x, y, z }
}

fn parse_line(string: String) -> Stone {
    let mut iter = string.split(" @ ");
    let position = parse_p3(iter.next().unwrap());
    let velocity = parse_p3(iter.next().unwrap());
    Stone { position, velocity }
}

fn read_lines() -> Vec<Stone> {
    std::io::stdin()
        .lines()
        .filter_map(|x| x.ok())
        .map(parse_line)
        .collect()
}

fn equation(s: &Stone) -> (f64, f64) {
    let a = (s.velocity.y as f64) / (s.velocity.x as f64);
    let b = -a * (s.position.x as f64) + (s.position.y as f64);
    (a, b)
}

fn intersects(s: &Stone, t: &Stone) -> (f64, f64) {
    // y = as x + bs (equation for stone s)
    // y = at x + bt (equation for stone t)
    // 0 = (as - at)x + (bs - bt)
    // x = (bt - bs) / (as - at)
    //
    // as = s.velocity.y / s.velocity.x
    // bs = (s.position.x - s.velocity.x) *
    let (a_s, b_s) = equation(s);
    let (a_t, b_t) = equation(t);
    let x = (b_t - b_s) / (a_s - a_t);
    let y = a_s * x + b_s;
    (x, y)
}

fn sign(x: f64) -> i32 {
    if x < 0. {
        -1
    } else {
        1
    }
}

fn sign_u(x: i64) -> i32 {
    if x < 0 {
        -1
    } else {
        1
    }
}

fn in_future(x: f64, stone: &Stone) -> bool {
    sign(x - (stone.position.x as f64)) == sign_u(stone.velocity.x)
}

// For all i: V_r * t[i] + P_r = V[i]*t[i] + P[i]
// so (V_r - V[i]) * t[i] = (P[i] - P_r)
// Equal vectors:
//  => (V_r - V[i]) and (P[i] - P_r) colinear
//  => (V_r - V[i]) cross product (P[i] - P_r) = 0
// Yay! we got rid of t !
//
// However, it still isn't linear.
// But the non-linear term does not depend on i.
// So substracting two equations will remove it:
// 0 = (V_r - V[0]) cross product (P[0] - P_r).x - (V_r - V[1]) cross product (P[1] - P_r).x
//   =   (V_r.y - V[0].y)(P[0].z - P_r.z) - (V_r.z - V[0].z)(P[0].y - P_r.y)
//     - (V_r.y - V[1].y)(P[1].z - P_r.z) + (V_r.z - V[1].z)(P[1].y - P_r.y)
//
//   =    V_r.y P[0].z - V_r.y P_r.z - V[0].y P[0].z + V[0].y P_r.z
//     - (V_r.z P[0].y - V_r.z P_r.y - V[0].z P[0].y + V[0].z P_r.y)  (replace z <-> y)
//     - (V_r.y P[1].z - V_r.y P_r.z - V[1].y P[1].z + V[1].y P_r.z)  (replace 0 <-> 1)
//     + (V_r.z P[1].y - V_r.z P_r.y - V[1].z P[1].y + V[1].z P_r.y)
//
//   = [0, V[1].z - V[0].z, V[0].y - V[1].y, 0, P[0].z - P[1].z, -P[0].y-P[1].y ]
//     * [P_r.x, P_r.y, P_r.z, V_r.x, V_r.y, V_r.z]
//     + (-V[0].y P[0].z + V[0].z P[0].y + V[1].y P[1].z - V[1].z P[1].y) (so flip signs)
//
// For the y and z equations, they are mostly the same after substitution
//    x y z
// so y z x
// so z x y
fn equation_system(v0: &Stone, v1: &Stone) -> (Vec<Vec<i64>>, Vec<i64>) {
    (
        vec![
            vec![
                0,
                v1.velocity.z - v0.velocity.z,
                v0.velocity.y - v1.velocity.y,
                0,
                v0.position.z - v1.position.z,
                v1.position.y - v0.position.y,
            ],
            vec![
                v0.velocity.z - v1.velocity.z,
                0,
                v1.velocity.x - v0.velocity.x,
                v1.position.z - v0.position.z,
                0,
                v0.position.x - v1.position.x,
            ],
            vec![
                v1.velocity.y - v0.velocity.y,
                v0.velocity.x - v1.velocity.x,
                0,
                v0.position.y - v1.position.y,
                v1.position.x - v0.position.x,
                0,
            ],
        ],
        vec![
            v0.velocity.y * v0.position.z - v0.velocity.z * v0.position.y
                + v1.velocity.z * v1.position.y
                - v1.velocity.y * v1.position.z,
            v0.velocity.z * v0.position.x - v0.velocity.x * v0.position.z
                + v1.velocity.x * v1.position.z
                - v1.velocity.z * v1.position.x,
            v0.velocity.x * v0.position.y - v0.velocity.y * v0.position.x
                + v1.velocity.y * v1.position.x
                - v1.velocity.x * v1.position.y,
        ],
    )
}

// fn print_matrix(matrix: &Vec<Vec<f64>>, equals: &Vec<f64>) {
//     for i in 0..matrix.len() {
//         print!("[");
//         for &x in &matrix[i] {
//             print!("{:>8}", (x * 100.).round() / 100.);
//         }
//         println!("]  [{:>8}]", (equals[i] * 100.).round() / 100.)
//     }
// }

fn solve_system(mut matrix: Vec<Vec<F>>, mut equals: Vec<F>) -> Vec<F> {
    for pos in 0..matrix.len() {
        let mut found_non_null = false;
        if !found_non_null {
            for j in (pos)..matrix.len() {
                if matrix[j][pos] != F::from(0) {
                    matrix.swap(j, pos);
                    equals.swap(j, pos);
                    found_non_null = true;
                    break;
                }
            }
            if !found_non_null {
                panic!("Non solvable system");
            }
        }
        let coeff = matrix[pos][pos].clone();
        for j in (pos + 1)..matrix.len() {
            let scale = matrix[j][pos].clone() / coeff.clone();
            for i in pos..(matrix[j].len()) {
                let diff = scale.clone() * matrix[pos][i].clone();
                matrix[j][i] -= diff;
            }
            let diff = scale * equals[pos].clone();
            equals[j] -= diff;
        }
    }
    let mut result = Vec::new();
    for _ in &matrix {
        result.push(F::from(0));
    }
    for pos in (0..matrix.len()).rev() {
        // The solution should be an integer
        let res = equals[pos].clone() / matrix[pos][pos].clone();
        //assert!(res.round() == res);
        for j in 0..pos {
            equals[j] -= res.clone() * matrix[j][pos].clone();
        }
        result[pos] = res;
    }
    return result;
}

fn main() {
    let input = read_lines();
    let mut nb_intersections = 0;
    let test_min = 200000000000000.; //7.;
    let test_max = 400000000000000.; // 27.;
    for i in 0..input.len() {
        for j in 0..i {
            let (x, y) = intersects(&input[i], &input[j]);
            if test_min <= x
                && x <= test_max
                && test_min <= y
                && y <= test_max
                && in_future(x, &input[i])
                && in_future(x, &input[j])
            {
                nb_intersections += 1;
            }
        }
    }
    println!("Part 1 : {}", nb_intersections);

    let (mut matrix, mut equal) = equation_system(&input[0], &input[2]);
    let (matrix2, equal2) = equation_system(&input[0], &input[1]);
    matrix.extend(matrix2);
    equal.extend(equal2);
    let system: Vec<Vec<F>> = matrix
        .iter()
        .map(|v| v.iter().map(|c| F::from(*c)).collect())
        .collect();
    let equals: Vec<F> = equal.iter().map(|c| F::from(*c)).collect();
    let res = solve_system(system, equals);
    println!(
        "Part 2 : {}",
        res[0].clone() + res[1].clone() + res[2].clone()
    );
}
