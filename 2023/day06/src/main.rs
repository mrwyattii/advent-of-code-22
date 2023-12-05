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

fn main() {
    let lines = read_file_lines("./input.txt");
    let mut times_p1: Vec<usize> = Vec::new();
    let mut dists_p1: Vec<usize> = Vec::new();
    let mut time_p2: usize = 0;
    let mut dist_p2: usize = 0;

    // Parse the input
    for line in lines {
        match line {
            line if line.starts_with("Time:") => {
                times_p1.extend(line_to_num(
                    line.strip_prefix("Time:")
                        .expect("Line did not match pattern"),
                ));
                time_p2 += line_to_num(
                    line.strip_prefix("Time:")
                        .expect("Line did not match pattern")
                        .replace(" ", "")
                        .as_str(),
                )[0];
            }
            line if line.starts_with("Distance:") => {
                dists_p1.extend(line_to_num(
                    line.strip_prefix("Distance:")
                        .expect("Line did not match pattern"),
                ));
                dist_p2 += line_to_num(
                    line.strip_prefix("Distance:")
                        .expect("Line did not match pattern")
                        .replace(" ", "")
                        .as_str(),
                )[0];
            }
            _ => {}
        }
    }

    // Part 1: naive
    let time_dist_pairs = times_p1.iter().zip(dists_p1.iter());
    let mut counts: Vec<usize> = Vec::new();
    for (t, d) in time_dist_pairs {
        // Iterate over pairs and solve
        let mut count: usize = 0;
        for hold_time in 0..=t - 1 {
            if (*t) * hold_time - hold_time * hold_time > (*d) {
                count += 1;
            }
        }
        counts.push(count);
    }

    let sum_p1 = counts.into_iter().reduce(|a, b| a * b).unwrap();

    // Part 2: less naive... binary search
    // We only need to search one direction since the maximum distance will
    // always be at T/2
    let mut guess = time_p2 / 2;
    let mut move_size = time_p2 / 4;
    loop {
        let this_dist = time_p2 * guess - guess * guess;
        if this_dist > dist_p2 {
            if move_size == 1 {
                break;
            }
            guess -= move_size;
            move_size = 1.max(move_size / 2);
        } else if this_dist < dist_p2 {
            guess += move_size;
            move_size = 1.max(move_size / 2);
        } else {
            break;
        }
    }

    println!("Part 1: {sum_p1}");
    println!("Part 2: {}", 2 * (time_p2 / 2 - guess) + 1);
}
