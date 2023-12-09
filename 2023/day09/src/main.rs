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

fn parse_input(filename: &str) -> Vec<Vec<i32>> {
    let lines = read_file_lines(filename);
    let mut numbers = Vec::new();
    for line in lines.iter() {
        numbers.push(line.split_whitespace().filter_map(|s| s.parse().ok()).collect());
    }
    numbers
}

fn get_next_number(sequence: Vec<i32>) -> i32 {
    let mut diffs = Vec::new();
    for i in 1..sequence.len() {
        diffs.push(sequence[i] - sequence[i-1]);
    }
    if diffs.iter().min() == diffs.iter().max() {
        // All values are the same
        return sequence.last().unwrap() + diffs[0];
    } else {
        // Values are different
        return sequence.last().unwrap() + get_next_number(diffs);
    }
}

fn main() {
    // Part 1
    let numbers = parse_input("./input.txt");
    let mut sum_p1 = 0;
    for sequence in numbers {
        sum_p1 += get_next_number(sequence);
    }

    // Part 2
    let numbers = parse_input("./input.txt");
    let mut sum_p2 = 0;
    for mut sequence in numbers {
        sequence.reverse();
        sum_p2 += get_next_number(sequence);
    }

    println!("Part 1 sum: {sum_p1}");
    println!("Part 2 sum: {sum_p2}");
}
