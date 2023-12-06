// ==== Puzzle 06 : https://adventofcode.com/2023/day/6 ====

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

fn parse_line(line: &str) -> Vec<usize> {
    line[10..]
        .trim()
        .split(" ")
        .filter_map(|c| {
            if c.trim().is_empty() {
                None
            } else {
                Some(c.parse::<usize>().unwrap())
            }
        })
        .collect()
}

// In theory, this can be solved fast:
// Waiting p seconds gives distance D(t, p) = (t-p) * p
// So D(t, p) > d
//    <=> -p^2 + tp - d > 0
//    <=> -p^2 + tp - d > 0
//    <=> t in roots of (-X^2 + tX - d)
//    <=> t in (t-sqrt(delta))/2 .. (t + sqrt(delta))/2
//    so t in an interval of width t
//
// You could also use the monotonicity of the result
// i.e. if p_1 < p_2 are valid then all p_1 .. p_2 will be two
// However, the brute force naive method runs in < 3s so I can't be bothered...
fn dist(time: usize, wait: usize) -> usize {
    (time - wait) * wait
}

fn main() {
    // For part 2, change inputs to remove spaces...
    let lines = read_lines();
    let times = parse_line(&lines[0]);
    let dists = parse_line(&lines[1]);
    let mut prod = 1;

    for i in 0..times.len() {
        let mut nb = 0;
        for j in 1..times[i] {
            if dist(times[i], j) > dists[i] {
                // println!("Dist: {} {} {}", times[i], j, dists[i]);
                nb += 1;
            }
        }
        // println!("Nb: {}", nb);
        prod *= nb;
    }
    println!("{}", prod);
}
