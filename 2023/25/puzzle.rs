// ==== Puzzle 25 : https://adventofcode.com/2023/day/25 ====

use std::collections::{HashMap, HashSet, VecDeque};

extern crate rand;
// import commonly used items from the prelude:
use rand::prelude::*;

// Graph as list of neighbors
// i.e. graph[i] contains j IFF (i,j) is an edge
type Graph = Vec<Vec<usize>>;

fn get_id<'a>(node: &'a str, ids: &mut HashMap<&'a str, usize>, graph: &mut Graph) -> usize {
    match ids.get(node) {
        Some(id) => *id,
        None => {
            let id = ids.len();
            ids.insert(node, id);
            // let mut vec = Vec::with_capacity(capacity);
            // for _ in 0..graph.len() {
            //     vec.push(false);
            // }
            // graph.push(vec);
            // for x in graph.iter_mut() {
            //     x.push(false);
            // }
            graph.push(Vec::new());
            id
        }
    }
}

fn read_lines() -> Vec<String> {
    std::io::stdin().lines().filter_map(|x| x.ok()).collect()
}

fn make_graph(lines: Vec<String>) -> Graph {
    let mut ids = HashMap::new();
    let capacity = lines.len();
    let mut graph = Vec::with_capacity(capacity);
    for i in 0..capacity {
        let source = get_id(&lines[i][0..3], &mut ids, &mut graph);
        for x in lines[i][5..].split(" ") {
            let target = get_id(x, &mut ids, &mut graph);
            graph[source].push(target);
            graph[target].push(source);
        }
    }
    graph
}

fn rm_edge_single(graph: &mut Graph, node_a: usize, node_b: usize) {
    let mut pos = usize::MAX;
    for (i, x) in graph[node_a].iter().enumerate() {
        if *x == node_b {
            pos = i;
            break;
        }
    }
    graph[node_a].remove(pos);
}

fn rm_edge(graph: &mut Graph, node_a: usize, node_b: usize) {
    rm_edge_single(graph, node_a, node_b);
    rm_edge_single(graph, node_b, node_a);
}

fn connex_component(graph: &Graph, start: usize) -> HashSet<usize> {
    let mut seen = HashSet::new();
    let mut worklist = VecDeque::new();
    seen.insert(start);
    worklist.push_back(start);
    while let Some(pos) = worklist.pop_front() {
        for x in &graph[pos] {
            if !seen.contains(x) {
                seen.insert(*x);
                worklist.push_back(*x);
            }
        }
    }
    seen
}

fn component_sizes(graph: &Graph) -> Option<(usize, usize)> {
    let mut components = Vec::new();
    let mut seen = HashSet::new();
    for x in 0..graph.len() {
        if seen.contains(&x) {
            continue;
        }
        let comp = connex_component(graph, x);
        components.push(comp.len());
        seen.extend(comp);
    }
    if components.len() == 2 {
        return Some((components[0], components[1]));
    }
    return None;
}

type EdgeCount = HashMap<(usize, usize), usize>;

fn add_edge(map: &mut EdgeCount, start: usize, end: usize) {
    let key = if start < end {
        (start, end)
    } else {
        (end, start)
    };
    match map.get_mut(&key) {
        Some(n) => {
            *n += 1;
            return;
        }
        None => (),
    }
    map.insert(key, 1);
}

fn dijkstra(graph: &Graph, start: usize, end: usize, edges: &mut EdgeCount) {
    let mut worklist = VecDeque::new();
    let mut parents = Vec::with_capacity(graph.len());
    for _ in graph {
        parents.push(None);
    }
    worklist.push_back(start);
    while let Some(pos) = worklist.pop_front() {
        if pos == end {
            break;
        }
        for x in &graph[pos] {
            if parents[*x] == None && *x != start {
                parents[*x] = Some(pos);
                worklist.push_back(*x);
            }
        }
    }
    let mut end = end;
    while let Some(start) = parents[end] {
        add_edge(edges, start, end);
        end = start;
    }
}

fn main() {
    let lines = read_lines();
    let graph = make_graph(lines);

    loop {
        let mut graph = graph.clone();
        for _ in 0..3 {
            let mut edges = HashMap::new();
            for _ in 0..500 {
                let a = random::<usize>() % graph.len();
                let b = random::<usize>() % graph.len();
                dijkstra(&graph, a, b, &mut edges);
            }
            let max = edges.iter().max_by_key(|entry| entry.1).unwrap().0;
            rm_edge(&mut graph, max.0, max.1);
        }
        match component_sizes(&graph) {
            None => (),
            Some((a, b)) => {
                println!("Part 1 : {} = {} * {}", a * b, a, b);
                return;
            }
        }
    }
    // for x in graph {
    //     println!("{}", x.iter().filter(|x| **x).count());
    // }
}
