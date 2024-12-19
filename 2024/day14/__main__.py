import re
import sys
from dataclasses import dataclass
from typing import List

from common.aoc_day import AoCDay


@dataclass
class Robot:
    x: int
    y: int
    dx: int
    dy: int
    x_max: int
    y_max: int
    x_min: int = 0
    y_min: int = 0

    def move(self):
        x = self.x + self.dx
        y = self.y + self.dy
        if x >= self.x_max:
            self.x = x % self.x_max
        elif x < self.x_min:
            self.x = self.x_max + x
        else:
            self.x = x
        if y >= self.y_max:
            self.y = y % self.y_max
        elif y < self.y_min:
            self.y = self.y_max + y
        else:
            self.y = y

    def qudrant_scores(self):
        if self.x < self.x_max // 2:
            if self.y < self.y_max // 2:
                return [1, 0, 0, 0]
            elif self.y > self.y_max // 2:
                return [0, 1, 0, 0]
        if self.x > self.x_max // 2:
            if self.y < self.y_max // 2:
                return [0, 0, 1, 0]
            elif self.y > self.y_max // 2:
                return [0, 0, 0, 1]
        return [0, 0, 0, 0]


class Day14(AoCDay):
    def process_input(self, raw_input: List[str]) -> List[Robot]:
        int_re = re.compile(r"(-?\d+)")
        robots = []
        for line in raw_input:
            px, py, dx, dy = map(int, int_re.findall(line))
            robots.append(Robot(x=px, y=py, dx=dx, dy=dy, x_max=101, y_max=103))
        return robots

    def part1(self, robots: List[Robot]) -> int:
        quad_scores = [0, 0, 0, 0]
        for r in robots:
            for _ in range(100):
                r.move()
            quad_scores = [a + b for a, b in zip(quad_scores, r.qudrant_scores())]
        return quad_scores[0] * quad_scores[1] * quad_scores[2] * quad_scores[3]

    @staticmethod
    def print_grid(r_locs, y_max, x_max):
        for y in range(y_max):
            for x in range(x_max):
                if (x, y) in r_locs:
                    print("R", end="")
                else:
                    print(".", end="")
            print()

    def part2(self, robots) -> int:
        count = 0
        max_count = 1000
        min_iter = 0
        min_quad_score = sys.maxsize
        r_locs = set()
        while count < max_count:
            count += 1
            quad_scores = [0, 0, 0, 0]
            for r in robots:
                r.move()
                quad_scores = [a + b for a, b in zip(quad_scores, r.qudrant_scores())]
            this_quad_score = (
                quad_scores[0] * quad_scores[1] * quad_scores[2] * quad_scores[3]
            )
            if this_quad_score < min_quad_score:
                min_quad_score = this_quad_score
                min_iter = count
                r_locs = set((r.x, r.y) for r in robots)
            if count == max_count:
                self.print_grid(r_locs, robots[0].x_max, robots[0].y_max)
                if input("Is Christmas? (y/[n]): ") == "y":
                    break
                else:
                    max_count += 1000

        return min_iter


if __name__ == "__main__":
    Day14()()
