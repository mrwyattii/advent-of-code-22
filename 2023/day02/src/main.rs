fn read_file_lines(filename: &str) -> Vec<String> {
    let mut result = Vec::new();

    for line in std::fs::read_to_string(filename).unwrap().lines() {
        result.push(line.to_string())
    }

    result
}

#[derive(Default)]
struct Round {
    red: i32,
    green: i32,
    blue: i32,
}

#[derive(Default)]
struct Bag {
    id: i32,
    max_red: i32,
    max_green: i32,
    max_blue: i32,
}

impl Bag {
    fn string_to_round(&self, round_str: &str) -> Round {
        let mut red: i32 = 0;
        let mut green: i32 = 0;
        let mut blue: i32 = 0;
        for color_count in round_str.split(",") {
            if let Some((count, color)) = color_count.trim_start().split_once(" ") {
                match color {
                    "red" => {
                        red = count.parse().unwrap();
                    }
                    "green" => {
                        green = count.parse().unwrap();
                    }
                    "blue" => {
                        blue = count.parse().unwrap();
                    }
                    _ => {}
                }
            }
        }
        Round {
            red: red,
            green: green,
            blue: blue,
        }
    }

    fn process_round(&mut self, round_str: &str) {
        let round = self.string_to_round(round_str);
        if round.red > self.max_red {
            self.max_red = round.red;
        }
        if round.green > self.max_green {
            self.max_green = round.green;
        }
        if round.blue > self.max_blue {
            self.max_blue = round.blue;
        }
    }

    fn is_game_possible(&self, red: i32, green: i32, blue: i32) -> bool {
        if red < self.max_red {
            return false;
        }
        if green < self.max_green {
            return false;
        }
        if blue < self.max_blue {
            return false;
        }
        true
    }
}

fn main() {
    let lines = read_file_lines("../input.txt");
    let mut sum: i32 = 0;
    let mut power_sum: i32 = 0;
    for line in lines {
        if let Some((game_id, rounds)) = line.split_once(":") {
            let id = game_id.trim_start_matches("Game ").parse().unwrap();
            let mut bag = Bag {
                id: id,
                max_red: 0,
                max_green: 0,
                max_blue: 0,
            };

            for round in rounds.split(";") {
                bag.process_round(round);
            }

            // part 1
            if bag.is_game_possible(12, 13, 14) {
                sum += bag.id;
            }

            // part 2
            power_sum += bag.max_red * bag.max_green * bag.max_blue;
        }
    }
    println!("Part 1 sum: {}", sum);
    println!("Part 2 power sum: {}", power_sum);
}
