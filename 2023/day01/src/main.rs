use std::fs;

fn part1() {
    let data = fs::read_to_string("../input.txt").expect("Unable to read file");
    let lines = data.split("\n");
    let mut sum: i32 = 0;
    for line in lines {
        let mut first_digit_seen = false;
        let mut first_digit: char = ' ';
        let mut last_digit: char = ' ';
        for char in line.chars() {
            if char.is_numeric() {
                if first_digit_seen {
                    last_digit = char;
                } else {
                    first_digit_seen = true;
                    first_digit = char;
                    last_digit = char;
                }
            }
        }
        let number: i32 = format!("{}{}", first_digit, last_digit).parse().unwrap();
        sum += number;
    }
    println!("Part 1 Calibration value is {}", sum);
}

fn part2() {
    let digit_words = [
        ["one", "1"],
        ["two", "2"],
        ["three", "3"],
        ["four", "4"],
        ["five", "5"],
        ["six", "6"],
        ["seven", "7"],
        ["eight", "8"],
        ["nine", "9"],
    ];
    let data = fs::read_to_string("../input.txt").expect("Unable to read file");
    let lines = data.split("\n");
    let mut sum: i32 = 0;
    for line in lines {
        let mut modified_line = line.to_string();
        for [word, digit] in digit_words {
            let replace_str = format!("{}{}{}", word, digit, word);
            modified_line = modified_line.replace(word, &replace_str);
        }

        let mut first_digit_seen = false;
        let mut first_digit: char = ' ';
        let mut last_digit: char = ' ';
        for char in modified_line.chars() {
            if char.is_numeric() {
                if first_digit_seen {
                    last_digit = char;
                } else {
                    first_digit_seen = true;
                    first_digit = char;
                    last_digit = char;
                }
            }
        }
        let number: i32 = format!("{}{}", first_digit, last_digit).parse().unwrap();
        sum += number;
    }
    println!("Part 2 Calibration value is {}", sum);
}

fn main() {
    part1();
    part2();
}
