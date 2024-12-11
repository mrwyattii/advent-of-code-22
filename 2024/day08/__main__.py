from collections import defaultdict
from functools import total_ordering
from itertools import combinations
from typing import Dict, List, Set

from common.aoc_day import AoCDay


@total_ordering
class Coord2D:
    def __init__(self, x: int, y: int):
        self.x = x
        self.y = y

    def __hash__(self) -> int:
        return hash((self.x, self.y))

    def __add__(self, other: "Coord2D") -> "Coord2D":
        return Coord2D(self.x + other.x, self.y + other.y)

    def __sub__(self, other: "Coord2D") -> "Coord2D":
        return Coord2D(self.x - other.x, self.y - other.y)

    def __eq__(self, other: "Coord2D") -> bool:
        return self.x == other.x and self.y == other.y

    def __lt__(self, other: "Coord2D") -> bool:
        return self.x <= other.x and self.y <= other.y

    def __repr__(self) -> str:
        return f"(x={self.x}, y={self.y})"


class Map:
    def __init__(self, antennas: Dict[str, Coord2D], size: Coord2D):
        self.antennas = antennas
        self.min_loc = Coord2D(0, 0)
        self.max_loc = size
        self.antinodes = set()

    def in_bounds(self, loc: Coord2D) -> bool:
        return self.min_loc < loc < self.max_loc

    def find_antinodes(
        self, loc1: Coord2D, loc2: Coord2D, resonant_freq: bool = False
    ) -> Set[Coord2D]:
        vec = loc1 - loc2
        if resonant_freq:  # Part 2
            self.antinodes.add(loc1)
            an = loc1 + vec
            while self.in_bounds(an):
                self.antinodes.add(an)
                an = an + vec
            an = loc1 - vec
            while self.in_bounds(an):
                self.antinodes.add(an)
                an = an - vec
        else:  # Part 1
            an = loc1 + vec
            if self.in_bounds(an):
                self.antinodes.add(an)
            an = loc2 - vec
            if self.in_bounds(an):
                self.antinodes.add(an)

    def get_antinodes(self, resonant_freq: bool = False) -> Set[Coord2D]:
        for _, coords in self.antennas.items():
            for loc1, loc2 in combinations(coords, 2):
                self.find_antinodes(loc1, loc2, resonant_freq)
        return self.antinodes


class Day08(AoCDay):
    def process_input(self, raw_input: List[str]) -> Map:
        size = Coord2D(len(raw_input[0].strip()) - 1, len(raw_input) - 1)
        antennas = defaultdict(set)
        for y, line in enumerate(raw_input):
            for x, char in enumerate(line.strip()):
                if char == ".":
                    continue
                antennas[char].add(Coord2D(x, y))

        return Map(antennas, size)

    def part1(self, puzzle_map: Map) -> int:
        return len(puzzle_map.get_antinodes())

    def part2(self, puzzle_map: Map) -> int:
        return len(puzzle_map.get_antinodes(resonant_freq=True))


if __name__ == "__main__":
    Day08()()
