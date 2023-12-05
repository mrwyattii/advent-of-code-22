fn read_file_lines(filename: &str) -> Vec<String> {
    let mut result = Vec::new();

    for line in std::fs::read_to_string(filename).unwrap().lines() {
        result.push(line.to_string())
    }

    result
}

struct Game {
    score: usize,
    copies: usize,
    matching: usize,
}

impl Game {
    fn new(winning_numbers: Vec<usize>, numbers: Vec<usize>) -> Game {
        // Calculate the score of the game and number of matches
        let mut score: usize = 0;
        let mut matching: usize = 0;
        for num in numbers.iter() {
            if winning_numbers.contains(num) {
                matching += 1;
                if score == 0 {
                    score = 1;
                } else {
                    score *= 2;
                }
            }
        }
        Game{score: score, copies: 1, matching: matching}
    }

    fn add_copies(&mut self, num_copies: usize) {
        self.copies += num_copies;
    }
}

fn split_into_numbers(s: &str) -> Vec<usize> {
    s.split_whitespace().filter_map(|s: &str| s.parse().ok()).collect()
}

fn main() {
    let filename: &str = "../input.txt";
    let lines: Vec<String> = read_file_lines(filename);

    // Create a vector of Game objects
    let mut games: Vec<Game> = Vec::new();
    for line in lines.iter() {
        let parts: Vec<&str> = line.splitn(2, ":").last().unwrap().splitn(2, "|").collect();
        games.push(
            Game::new(
                split_into_numbers(parts[0]),
                split_into_numbers(parts[1])
            )
        );
    }

    // Add copies for number of matching (part 2)
    for i in 0..=(games.len()-1) {
        let copies = games[i].copies;
        let matching = games[i].matching;
        if matching == 0 {continue;}
        for j in (i+1)..=(i+matching) {
            games[j].add_copies(copies);
        }
    }

    let mut sum_part1: usize = 0;
    let mut sum_part2: usize = 0;
    for i in 0..=(games.len()-1) {
        sum_part1 += games[i].score;
        println!("{}, {}", i, games[i].copies);
        sum_part2 += games[i].copies;
    }

    println!("Part 1 sum: {}", sum_part1);
    println!("Part 2 sum: {}", sum_part2);

}