from itertools import product
from typing import List

from common.aoc_day import AoCDay


class Day04(AoCDay):
    def process_input(self, raw_input: List[str]):
        return [[char for char in line.strip()] for line in raw_input]

    @staticmethod
    def word_in_direction(grid, word, start, direction):
        x, y = start
        dx, dy = direction
        for char in word:
            if (
                x < 0
                or y < 0
                or x >= len(grid)
                or y >= len(grid[0])
                or grid[x][y] != char
            ):
                return False
            x += dx
            y += dy
        return True

    def part1(self, input: List[List[str]]):
        word_count = 0
        directions = list(product([1, 0, -1], repeat=2))
        print(directions)
        for i in range(len(input)):
            for j in range(len(input[i])):
                for d in directions:
                    if self.word_in_direction(input, "XMAS", (i, j), d):
                        word_count += 1
        print(word_count)

    @staticmethod
    def x_mas_in_position(grid, x, y):
        if grid[x][y] != "A":
            return False
        if x < 1 or y < 1 or x >= len(grid) - 1 or y >= len(grid[0]) - 1:
            return False
        mas_chars = {"M", "S"}
        forward_slash = set((grid[x - 1][y - 1], grid[x + 1][y + 1]))
        backward_slash = set((grid[x - 1][y + 1], grid[x + 1][y - 1]))
        return (forward_slash == mas_chars) and (backward_slash == mas_chars)

    def part2(self, input: List[List[str]]):
        word_count = 0
        for i in range(len(input)):
            for j in range(len(input[i])):
                if self.x_mas_in_position(input, i, j):
                    word_count += 1
        print(word_count)


if __name__ == "__main__":
    Day04()()
