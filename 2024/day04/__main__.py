from itertools import product
from typing import Dict, List, Tuple

from common.aoc_day import AoCDay


class Day04(AoCDay):
    def process_input(self, raw_input: List[str]) -> Dict[Tuple[int, int], str]:
        return {
            (x, y): char
            for x, line in enumerate(raw_input)
            for y, char in enumerate(line.strip())
        }

    @staticmethod
    def word_in_direction(
        grid: Dict[Tuple[int, int], str],
        word: str,
        start: Tuple[int, int],
        direction: Tuple[int, int],
    ):
        for char in word:
            if grid.get(start, "") != char:
                return False
            start = (start[0] + direction[0], start[1] + direction[1])
        return True

    def part1(self, input: Dict[Tuple[int, int], str]):
        word_count = 0
        directions = list(product([1, 0, -1], repeat=2))
        for start in input.keys():
            for d in directions:
                if self.word_in_direction(input, "XMAS", start, d):
                    word_count += 1
        print(word_count)

    @staticmethod
    def x_mas_in_position(grid: Dict[Tuple[int, int], str], start: Tuple[int, int]):
        if grid.get(start) != "A":
            return False
        x, y = start
        mas_chars = {"M", "S"}
        forward_slash = {grid.get((x - 1, y - 1), ""), grid.get((x + 1, y + 1), "")}
        backward_slash = {grid.get((x - 1, y + 1), ""), grid.get((x + 1, y - 1), "")}
        return (forward_slash == mas_chars) and (backward_slash == mas_chars)

    def part2(self, input: Dict[Tuple[int, int], str]):
        word_count = 0
        for start in input.keys():
            if self.x_mas_in_position(input, start):
                word_count += 1
        print(word_count)


if __name__ == "__main__":
    Day04()()
