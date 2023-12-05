fn read_file_lines(filename: &str) -> Vec<String> {
    let mut result = Vec::new();

    for line in std::fs::read_to_string(filename).unwrap().lines() {
        result.push(line.to_string())
    }

    result
}

fn part1(lines: Vec<String>) {
    let width = lines[0].len();

    // Get a list of all the index values on or adjacent to a symbol
    // Here I'm representing the location as an integer value:
    // 0 1 2
    // 3 4 5
    // 6 7 8
    let mut symbols = Vec::new();
    for (i, line) in lines.iter().enumerate() {
        for (j, char) in line.chars().enumerate() {
            if char != '.' && !char.is_numeric() {
                for y in 0..3 {
                    for x in 0..3 {
                        // Have to be careful around the edges of lines
                        if (i + y) < 1 {
                            continue;
                        }
                        if (j + x) < 1 || (j + x - 1) > width {
                            continue;
                        }
                        let symbol_idx = (i + y - 1) * width + (j + x - 1);
                        symbols.push(symbol_idx);
                    }
                }
            }
        }
    }

    let mut add_number = false;
    let mut number_string = String::from("");
    let mut sum = 0;
    for (i, line) in lines.iter().enumerate() {
        for (j, char) in line.chars().enumerate() {
            if char.is_numeric() {
                // When we a number character, cache that and check if it's next to a symbol
                number_string.push(char);
                if symbols.contains(&(i * width + j)) {
                    add_number = true;
                }
            } else {
                // If we found a number next to a symbol, add it to our sum
                if add_number {
                    sum += number_string.parse::<usize>().unwrap();
                }
                add_number = false;
                number_string = String::from("");
            }
        }
    }

    println!("Part 1 sum: {}", sum);
}

struct Gear {
    gear_loc: Vec<usize>,
    numbers: Vec<usize>,
}

fn part2(lines: Vec<String>) {
    let width = lines[0].len();

    // Get a list of all the index values on or adjacent to a symbol
    // Here I'm representing the location as an integer value:
    // 0 1 2
    // 3 4 5
    // 6 7 8
    let mut gears: Vec<Gear> = Vec::new();
    for (i, line) in lines.iter().enumerate() {
        for (j, char) in line.chars().enumerate() {
            if char == '*' {
                let mut gear_loc = Vec::new();
                for y in 0..3 {
                    for x in 0..3 {
                        // Have to be careful around the edges of lines
                        if (i + y) < 1 {
                            continue;
                        }
                        if (j + x) < 1 || (j + x - 1) > width {
                            continue;
                        }
                        let symbol_idx = (i + y - 1) * width + (j + x - 1);
                        gear_loc.push(symbol_idx);
                    }
                }
                gears.push(Gear {
                    gear_loc: gear_loc,
                    numbers: Vec::new(),
                });
            }
        }
    }

    let mut add_number = false;
    let mut number_string = String::from("");
    let mut gear_id_to_add: usize = 0;
    for (i, line) in lines.iter().enumerate() {
        for (j, char) in line.chars().enumerate() {
            if char.is_numeric() {
                // When we a number character, cache that and check if it's next to a symbol
                number_string.push(char);
                for (gear_id, gear) in gears.iter().enumerate() {
                    if gear.gear_loc.contains(&(i * width + j)) {
                        add_number = true;
                        gear_id_to_add = gear_id;
                    }
                }
            } else {
                // If we found a number next to a symbol, add it to our sum
                if add_number {
                    gears[gear_id_to_add]
                        .numbers
                        .push(number_string.parse::<usize>().unwrap());
                }
                add_number = false;
                number_string = String::from("");
            }
        }
    }

    let mut sum: usize = 0;
    for gear in gears.iter() {
        if gear.numbers.len() > 1 {
            sum += gear.numbers.iter().fold(1, |acc, &x| acc * x);
        }
    }

    println!("Part 2 sum: {}", sum);
}

fn main() {
    let lines = read_file_lines("../input.txt");

    part1(lines.clone());
    part2(lines.clone());
}
