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

fn line_to_num(s: &str) -> Vec<usize> {
    s.split_whitespace()
        .filter_map(|s: &str| s.parse().ok())
        .collect()
}

struct MapIndex {
    dst_idx: usize,
    src_idx: usize,
    length: usize,
}

struct Map {
    from: String,
    to: String,
    indicies: Vec<MapIndex>,
}

impl Map {
    fn new(line: String) -> Map {
        let (maps_from, maps_to) = line
            .strip_suffix(" map:")
            .expect("Line did not match expectations!")
            .split_once("-to-")
            .expect("Line did not match expectations!");
        let indicies = Vec::new();
        Map {
            from: maps_from.to_string(),
            to: maps_to.to_string(),
            indicies: indicies,
        }
    }

    fn add_index(&mut self, line: &str) {
        let dst_src_len = line_to_num(line);
        let index = MapIndex {
            dst_idx: dst_src_len[0],
            src_idx: dst_src_len[1],
            length: dst_src_len[2],
        };
        self.indicies.push(index);
    }

    fn get_next_val(&self, src_idx: usize) -> usize {
        let mut dst_idx = src_idx;
        for index in self.indicies.iter() {
            if index.src_idx <= src_idx && (index.src_idx + index.length) > src_idx {
                dst_idx = index.dst_idx + (src_idx - index.src_idx);
            }
        }
        dst_idx
    }
}

fn main() {
    let lines = read_file_lines("./input.txt");
    let mut maps = std::collections::HashMap::new();
    let mut seeds = Vec::new();
    let mut current_map_idx = String::new();
    for line in lines {
        match line {
            line if line.starts_with("seeds:") => {
                seeds = line_to_num(
                    line.strip_prefix("seeds:")
                        .expect("Line did not match expectations!"),
                );
            }
            line if line.ends_with("map:") => {
                let map = Map::new(line);
                current_map_idx = map.from.clone();
                maps.insert(map.from.clone(), map);
            }
            line if line.is_empty() => {}
            _ => {
                maps.get_mut(&current_map_idx)
                    .expect("Map value is missing!")
                    .add_index(line.as_str());
            }
        }
    }

    // Part 1
    let mut min_loc_1 = usize::max_value();
    for seed in seeds.iter() {
        let mut current_val = *seed;
        let mut current_map_idx = String::from("seed");
        loop {
            if current_map_idx == "location" {
                min_loc_1 = min_loc_1.min(current_val);
                break;
            }
            current_val = maps
                .get(&current_map_idx)
                .expect("Map value is missing!")
                .get_next_val(current_val);
            current_map_idx = maps
                .get(&current_map_idx)
                .expect("Map value is missing!")
                .to
                .clone();
        }
    }

    // Part 2
    let mut min_loc_2 = usize::max_value();
    let print_step: usize = 1000000;
    let mut i: usize = 0;
    let mut total_size: usize = 0;
    for chunk in seeds.chunks(2) {
        total_size += chunk[1];
    }
    for chunk in seeds.chunks(2) {
        let start_idx = chunk[0];
        let length = chunk[1];
        for seed in start_idx..=start_idx + length - 1 {
            if i % print_step == 0 {
                println!("{}/{}", i, total_size);
            }
            i += 1;
            let mut current_val = seed;
            let mut current_map_idx = String::from("seed");
            loop {
                if current_map_idx == "location" {
                    min_loc_2 = min_loc_2.min(current_val);
                    break;
                }
                current_val = maps
                    .get(&current_map_idx)
                    .expect("Map value is missing!")
                    .get_next_val(current_val);
                current_map_idx = maps
                    .get(&current_map_idx)
                    .expect("Map value is missing!")
                    .to
                    .clone();
            }
        }
    }

    println!("Part 1 min location: {min_loc_1}");
    println!("Part 2 min location: {min_loc_2}");
}
