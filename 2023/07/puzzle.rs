// ==== Puzzle 07 : https://adventofcode.com/2023/day/7 ====

use std::collections::HashMap;
use std::cmp::Ordering;

const PART_TWO: bool = true;

// =================================================
// ========= Types =================================
// =================================================

#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
struct Card(u32);

#[derive(PartialEq, Eq)]
struct Hand {
  hand: [Card; 5],
  bet: usize
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug)]
enum Rank {
  HighCard = 0,
  OnePair = 1,
  TwoPair = 2,
  ThreeOfAKind = 3,
  FullHouse = 4,
  FourOfAKind = 5,
  FiveOfAKind = 6
}

// =================================================
// ========= Parsing ===============================
// =================================================

fn parse_line(line: &str) -> Hand {
  let mut it = line.split(" ");
  let mut hand: [Card; 5] = [Card(0), Card(0), Card(0), Card(0), Card(0)];
  let x = it.next().unwrap().as_bytes();
  for i in 0..5 {
    hand[i] = match x[i] {
      b'K' => Card(14),
      b'Q' => Card(13),
      b'J' => if PART_TWO { Card(0) } else { Card(12)},
      b'T' => Card(11),
      n => Card((n - b'0' + 1) as u32)
    }
  }
  return Hand{hand, bet:it.next().unwrap().parse().unwrap()}

}

fn read_lines() -> Vec<Hand> {
  let mut file_lines = Vec::new();
  for line in std::io::stdin().lines() {
      match line {
          Ok(l) => file_lines.push(parse_line(&l)),
          Err(_) => panic!(),
      }
  }
  return file_lines;
}

// =================================================
// ========= Solving ===============================
// =================================================

fn hand_rank(hand: &Hand) -> Rank{
  let mut map = HashMap::new();
  for card in &hand.hand {
    map.insert(card, match map.get(&card) {
      None => 1,
      Some(n) => *n + 1
    });
  }
  if PART_TWO {
    match &map.get(&Card(0)) {
      Some(&n) => {
        map.remove(&Card(0));
        for (_, val) in map.iter_mut() {
            *val += n;
        }},
      _ => ()
    }
  }
  match map.len() {
   0 => Rank::FiveOfAKind,
   1 => Rank::FiveOfAKind,
   2 => match map.into_values().max() {
          Some(4) => { Rank::FourOfAKind},
          _ => {Rank::FullHouse}
        },
   3 => match map.into_values().max() {
          Some(3) => { Rank::ThreeOfAKind},
          _ => {Rank::TwoPair}
        },
   4 => Rank::OnePair,
   5 => Rank::HighCard,
   _ => panic!("Too many cards")
  }
}

impl PartialOrd for Hand {
  fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
      match hand_rank(&self).cmp(&hand_rank(&other)) {
        Ordering::Equal => Some(self.hand.cmp(&other.hand)),
        x => Some(x)
      }
  }
}
impl Ord for Hand {
  fn cmp(&self, other: &Self) -> Ordering {
      match hand_rank(&self).cmp(&hand_rank(&other)) {
        Ordering::Equal => self.hand.cmp(&other.hand),
        x => x
      }
  }
}

fn main() {
    let mut input = read_lines();
    input.sort();
    let mut total: usize = 0;
    for (i,x) in input.iter().enumerate() {
        total += (i+1) * x.bet;
    }
    println!("{}", total);

}
