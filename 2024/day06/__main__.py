from typing import List, Set, Tuple

from common.aoc_day import AoCDay
from tqdm import tqdm


class CycleError(Exception):
    pass


class GuardWalker:
    def __init__(
        self,
        grid_size: Tuple[int, int],
        obstacles: Set[Tuple[int, int]],
        location: Tuple[int, int],
        direction: Tuple[int, int],
    ):
        self.grid_size = grid_size
        self.obstacles = obstacles
        self.location = location
        self.direction = direction
        self.history = set()

        self.original_location = location
        self.original_direction = direction

    def reset(self) -> None:
        self.location = self.original_location
        self.direction = self.original_direction
        self.history = set()

    def turn_right(self) -> None:
        self.direction = (-self.direction[1], self.direction[0])

    def next_location(self) -> Tuple[int, int]:
        return (
            self.location[0] + self.direction[0],
            self.location[1] + self.direction[1],
        )

    def outside_grid(self) -> bool:
        return (
            self.location[0] < 0
            or self.location[0] >= self.grid_size[0]
            or self.location[1] < 0
            or self.location[1] >= self.grid_size[1]
        )

    def walk(self) -> None:
        while not self.outside_grid():
            if (self.location, self.direction) in self.history:
                raise CycleError("Cycle detected")
            self.history.add((self.location, self.direction))

            while self.next_location() in self.obstacles:
                self.turn_right()
            self.location = self.next_location()


class Day06(AoCDay):
    def process_input(self, raw_input: List[str]) -> GuardWalker:
        obstacles = set()
        grid_size = (len(raw_input[0].strip()), len(raw_input))
        for y, row in enumerate(raw_input):
            for x, cell in enumerate(row.strip()):
                if cell == "^":
                    start = (x, y)
                if cell == "#":
                    obstacles.add((x, y))
        return GuardWalker(
            grid_size=grid_size, obstacles=obstacles, location=start, direction=(0, -1)
        )

    def part1(self, input: GuardWalker) -> None:
        input.walk()
        print(len({loc for loc, _ in input.history}))

    def part2(self, input: GuardWalker) -> None:
        input.walk()
        path = {loc for loc, _ in input.history}
        path.remove(input.original_location)  # Skip first location
        add_obstacle_locations = set()
        for loc in tqdm(path):
            input.reset()
            input.obstacles.add(loc)
            try:
                input.walk()
            except CycleError:
                add_obstacle_locations.add(loc)
            input.obstacles.remove(loc)
        print(len(add_obstacle_locations))


if __name__ == "__main__":
    Day06(0)()
