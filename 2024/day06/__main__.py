from collections import defaultdict
from typing import Dict, List, Tuple

from common.aoc_day import AoCDay
from tqdm import tqdm


class CycleError(Exception):
    pass


class GuardWalker:
    def __init__(
        self,
        grid_map: Dict[Tuple[int, int], str],
        location: Tuple[int, int],
        direction: Tuple[int, int],
    ):
        self.grid_map = grid_map
        self.location = location
        self.direction = direction
        self.history = []

        self.original_location = location
        self.original_direction = direction

    def reset(self) -> None:
        self.location = self.original_location
        self.direction = self.original_direction
        self.history = []

    def turn_right(self) -> None:
        self.direction = (-self.direction[1], self.direction[0])

    def next_location(self) -> Tuple[int, int]:
        return (
            self.location[0] + self.direction[0],
            self.location[1] + self.direction[1],
        )

    def walk(self) -> None:
        while self.grid_map[self.location] != "":
            if (self.location, self.direction) in self.history:
                raise CycleError("Cycle detected")
            self.history.append((self.location, self.direction))

            while self.grid_map[self.next_location()] == "#":
                self.turn_right()
            self.location = self.next_location()


class Day06(AoCDay):
    def process_input(self, raw_input: List[str]) -> GuardWalker:
        grid_map = defaultdict(str)
        for y, row in enumerate(raw_input):
            for x, cell in enumerate(row.strip()):
                if cell == "^":
                    start = (x, y)
                grid_map[(x, y)] = cell
        return GuardWalker(grid_map=grid_map, location=start, direction=(0, -1))

    def part1(self, input: GuardWalker) -> None:
        input.walk()
        print(len({loc for loc, _ in input.history}))

    def part2(self, input: GuardWalker) -> None:
        input.walk()
        path = {loc for loc, _ in input.history[1:]}  # Skip first location
        add_obstacle_locations = set()
        for loc in tqdm(path):
            input.reset()
            input.grid_map[loc] = "#"
            try:
                input.walk()
            except CycleError:
                add_obstacle_locations.add(loc)
            input.grid_map[loc] = "."
        print(len(add_obstacle_locations))


if __name__ == "__main__":
    Day06(0)()
