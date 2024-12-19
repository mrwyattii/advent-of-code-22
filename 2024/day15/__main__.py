from typing import List, Tuple

from common.aoc_day import AoCDay
from tqdm import tqdm


class Obj:
    def __init__(self, symbol: str, loc: complex, width: int, height: int):
        self.symbol = symbol
        self.loc = loc
        self.width = width
        self.height = height

    @property
    def locs(self):
        return [
            self.loc + complex(x, y)
            for y in range(self.height)
            for x in range(self.width)
        ]

    @property
    def is_robot(self):
        return self.symbol == "@"

    @property
    def is_goods(self):
        return self.symbol == "O"

    @property
    def is_wall(self):
        return self.symbol == "#"


class GPS:
    def __init__(self, objects: List[Obj] = None):
        if objects is not None:
            self.objects = objects
        else:
            self.objects = []

    def add_object(self, symbol: str, loc: complex, width: int = 1, height: int = 1):
        obj = Obj(symbol, loc, width, height)
        self.objects.append(obj)

    def find_robot(self):
        for obj in self.objects:
            if obj.is_robot:
                return obj

    def can_move(self, d: complex, loc: complex) -> bool:
        for obj in self.objects:
            if loc in obj.locs:
                break
        else:
            return True

        if obj.is_wall:
            return False

        for obj_loc in obj.locs:
            if obj_loc + d in obj.locs:
                continue
            elif not self.can_move(d, obj_loc + d):
                return False

        return True

    def move(self, d: complex, loc: complex) -> bool:
        if not self.can_move(d, loc):
            return loc

        for obj in self.objects:
            if loc in obj.locs:
                break
        else:
            return loc

        for obj_loc in obj.locs:
            if obj_loc + d in obj.locs:
                continue
            self.move(d, obj_loc + d)
        obj.loc += d
        return loc + d

    def print(self):
        max_x = max([int(l.real) for obj in self.objects for l in obj.locs])
        max_y = max([int(l.imag) for obj in self.objects for l in obj.locs])
        grid = [["." for _ in range(max_x + 1)] for _ in range(max_y + 1)]
        for obj in self.objects:
            for l in obj.locs:
                grid[int(l.imag)][int(l.real)] = obj.symbol
        print("\n".join(["".join(l) for l in grid]))


class Day15(AoCDay):
    def process_input(
        self, raw_input: List[str]
    ) -> Tuple[List[Tuple[str, complex]], List[complex]]:
        objects = []
        move_list = []
        move_map = {"<": -1, ">": 1, "^": -1j, "v": 1j}
        for y, line in enumerate(raw_input):
            for x, c in enumerate(line.strip()):
                if c in move_map:
                    move_list.append(move_map[c])
                elif c == ".":
                    pass
                else:
                    objects.append((c, complex(x, y)))

        return objects, move_list

    def part1(
        self, puzzle_input: Tuple[List[Tuple[str, complex]], List[complex]]
    ) -> int:
        objects, moves = puzzle_input
        gps = GPS()
        for symbol, loc in objects:
            gps.add_object(symbol, loc)

        robot_loc = gps.find_robot().loc

        for m in tqdm(moves):
            robot_loc = gps.move(m, robot_loc)

        sum_value = 0
        for obj in gps.objects:
            if obj.is_goods:
                sum_value += (obj.loc.imag) * 100 + (obj.loc.real)

        return int(sum_value)

    def part2(self, puzzle_input) -> int:
        objects, moves = puzzle_input
        gps = GPS()
        for symbol, loc in objects:
            if symbol == "@":
                gps.add_object(symbol, complex(loc.real * 2, loc.imag))
            else:
                gps.add_object(symbol, complex(loc.real * 2, loc.imag), width=2)

        robot_loc = gps.find_robot().loc
        for m in tqdm(moves):
            robot_loc = gps.move(m, robot_loc)

        sum_value = 0
        for obj in gps.objects:
            if obj.is_goods:
                sum_value += (obj.loc.imag) * 100 + (obj.loc.real)

        return int(sum_value)


if __name__ == "__main__":
    Day15()()
