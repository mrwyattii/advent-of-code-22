use std::collections::HashMap;

fn read_file_lines(filename: &str) -> Vec<String> {
    let mut result = Vec::new();

    for line in std::fs::read_to_string(filename)
        .expect("Unable to read file")
        .lines()
    {
        result.push(line.to_string())
    }

    result
}

struct Hand {
    cards: Vec<char>,
    bid: usize,
}

impl Hand {
    fn new(line: &String) -> Hand {
        let (cards, bid) = line.split_once(" ").expect("Input did not match pattern!");
        Hand {
            cards: cards.chars().collect(),
            bid: bid.parse().expect("Number not found!"),
        }
    }

    fn get_type(&self, jokers: bool) -> usize {
        if !jokers || !self.cards.contains(&'J') {
            self.get_max_type()
        } else {
            let joker_idx = self.cards.iter().position(|&c| c == 'J').unwrap();
            let mut max_type: usize = 0;
            for c in [
                'A', 'K', 'Q', 'T', '9', '8', '7', '6', '5', '4', '3', '2', '1',
            ] {
                let mut new_cards = self.cards.to_vec();
                new_cards[joker_idx] = c;
                let new_hand = Hand {
                    cards: new_cards,
                    bid: self.bid,
                };
                let this_type = new_hand.get_type(true);
                max_type = max_type.max(this_type);
            }
            max_type
        }
    }

    fn get_max_type(&self) -> usize {
        let mut card_counts: HashMap<char, usize> = HashMap::new();
        for c in self.cards.iter() {
            *card_counts.entry(*c).or_insert(0) += 1;
        }
        let unique_cards = card_counts.keys().len();
        match unique_cards {
            5 => 1, // High card
            4 => 2, // One pair
            3 => {
                if *card_counts.values().max().unwrap() == 2 {
                    3
                }
                // Two pair
                else {
                    4
                } // Three of a kind
            }
            2 => {
                if *card_counts.values().max().unwrap() == 3 {
                    5
                }
                // Full house
                else {
                    6
                } // Four of a kind
            }
            _ => 7, // Five of a kind
        }
    }
}

fn card_to_rank(c: char, joker: bool) -> usize {
    match c {
        'A' => 14,
        'K' => 13,
        'Q' => 12,
        'J' => {
            if joker {
                1
            } else {
                11
            }
        }
        'T' => 10,
        _ => c as usize - '0' as usize, // really??
    }
}

fn rank_same_hands(a: Vec<char>, b: Vec<char>, idx: usize, joker: bool) -> std::cmp::Ordering {
    let a_rank = card_to_rank(a[idx], joker);
    let b_rank = card_to_rank(b[idx], joker);
    if a_rank == b_rank {
        rank_same_hands(a, b, idx + 1, joker)
    } else {
        a_rank.cmp(&b_rank)
    }
}

fn main() {
    let lines = read_file_lines("./input.txt");
    let mut hands_p1: HashMap<usize, Vec<Hand>> = HashMap::new();
    let mut hands_p2: HashMap<usize, Vec<Hand>> = HashMap::new();

    for line in lines.iter() {
        let hand = Hand::new(line);
        let hand_type = hand.get_type(false);
        hands_p1.entry(hand_type).or_insert(Vec::new()).push(hand);
        let hand = Hand::new(line);
        let hand_type = hand.get_type(true);
        hands_p2.entry(hand_type).or_insert(Vec::new()).push(hand);
    }

    for (_, v) in hands_p1.iter_mut() {
        v.sort_by(|a, b| rank_same_hands(a.cards.clone(), b.cards.clone(), 0, false));
    }

    for (_, v) in hands_p2.iter_mut() {
        v.sort_by(|a, b| rank_same_hands(a.cards.clone(), b.cards.clone(), 0, true));
    }

    let mut rank: usize = 1;
    let mut sum_p1: usize = 0;
    for k in 1..8 {
        if !hands_p1.contains_key(&k) {
            continue;
        }
        for h in hands_p1.get(&k).unwrap() {
            sum_p1 += rank * h.bid;
            rank += 1;
        }
    }

    let mut rank: usize = 1;
    let mut sum_p2: usize = 0;
    for k in 1..8 {
        if !hands_p2.contains_key(&k) {
            continue;
        }
        for h in hands_p2.get(&k).unwrap() {
            sum_p2 += rank * h.bid;
            rank += 1;
        }
    }

    println!("Part 1 sum: {sum_p1}");
    println!("Part 2 sum: {sum_p2}");
}
