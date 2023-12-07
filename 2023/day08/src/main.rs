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

fn parse_input(filename: &str) -> (Vec<char>, HashMap<String, Node>) {
    let lines = read_file_lines(filename);
    let directions: Vec<char> = lines[0].chars().collect();
    let mut map: HashMap<String, Node> = HashMap::new();

    // Build the map
    for line in lines {
        match line {
            line if line.contains("=") => {
                if let Some((key, val)) = line.split_once(" ") {
                    let key_str = key.to_string();
                    let val_str = val.to_string();
                    map.insert(key_str, Node::new(val_str));
                }
            }
            _ => {}
        }
    }

    (directions, map)
}

struct Node {
    left: String,
    right: String,
}

impl Node {
    fn new(line: String) -> Node {
        let (left, right) = line[line.find("(").unwrap() + 1..line.len()]
            .strip_suffix(")")
            .unwrap()
            .split_once(", ")
            .unwrap();
        Node {
            left: left.to_string(),
            right: right.to_string(),
        }
    }
}

fn compute_steps(
    map: &HashMap<String, Node>,
    guide: &Vec<char>,
    start_loc: String,
    end_cond: fn(&String) -> bool,
) -> usize {
    let mut steps: usize = 0;
    let mut current_loc = start_loc;
    let guide_len = guide.len();
    loop {
        if end_cond(&current_loc) {
            break;
        }
        let dir = guide[steps % guide_len];
        let node = map.get(&current_loc).expect("Missing key!");
        match dir {
            'R' => {
                current_loc = node.right.clone();
            }
            'L' => {
                current_loc = node.left.clone();
            }
            _ => {
                panic!("NONONONO");
            }
        }
        steps += 1;
    }
    steps
}

fn main() {
    // Get parsed input
    let (directions, map) = parse_input("./input.txt");

    // Part 1
    let steps_p1 = compute_steps(&map, &directions, String::from("AAA"), |s| s == "ZZZ");

    // Part 2
    let starting_locs: Vec<String> = map
        .keys()
        .filter(|k| k.ends_with("A"))
        .map(|k| k.clone())
        .collect();
    let mut steps_p2: Vec<usize> = Vec::new();
    for start_loc in starting_locs {
        steps_p2.push(compute_steps(&map, &directions, start_loc, |s| {
            s.ends_with("Z")
        }));
    }
    let steps_p2 = steps_p2.into_iter().reduce(num::integer::lcm).unwrap();

    println!("Part 1: {steps_p1}");
    println!("Part 2: {steps_p2}");
}
