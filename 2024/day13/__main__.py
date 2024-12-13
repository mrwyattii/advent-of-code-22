import functools
import re
import sys
from typing import List

from common.aoc_day import AoCDay
from tqdm import tqdm

sys.setrecursionlimit(1500)


class Machine:
    def __init__(self, A: complex, B: complex, P: complex):
        self.A_cost = 3
        self.B_cost = 1
        self.A = A
        self.B = B
        self.P = P

    # Leaving this in for posterity, but it's too slow for P2
    @functools.cache
    def solve(self, loc: complex = 0, cost: int = 0) -> int:
        if loc == self.P:
            return cost
        if loc.real > self.P.real or loc.imag > self.P.imag:
            return sys.maxsize
        return min(
            self.solve(loc + self.A, cost + self.A_cost),
            self.solve(loc + self.B, cost + self.B_cost),
        )

    def math_solve(self) -> int:
        # I think this may ignore some edge cases, but it seems to work!
        # Solve following system of eqs for a and b:
        # Ax*A+Bx*B = Px
        # Ay*A+By*B = Py
        Ax, Ay = self.A.real, self.A.imag
        Bx, By = self.B.real, self.B.imag
        Px, Py = self.P.real, self.P.imag
        A = (Px * By - Py * Bx) / (Ax * By - Ay * Bx)
        B = (Py * Ax - Px * Ay) / (Ax * By - Ay * Bx)
        if A.is_integer() and B.is_integer():
            return int(self.A_cost * A + self.B_cost * B)
        return sys.maxsize


class Day13(AoCDay):
    def process_input(self, raw_input: List[str]) -> List[Machine]:
        number_re = re.compile(r"(\d+),.*?(\d+)")
        vals = []
        for line in raw_input:
            val1, val2 = list(map(int, number_re.search(line).groups()))
            vals.append(complex(val1, val2))

        return [Machine(A, B, P) for A, B, P in zip(vals[::3], vals[1::3], vals[2::3])]

    def part1(self, machines: List[Machine]) -> int:
        total_tokens = 0
        for m in tqdm(machines):
            soln = m.solve()
            if soln != sys.maxsize:
                total_tokens += soln
        return total_tokens

    def part2(self, machines: List[Machine]) -> int:
        total_tokens = 0
        for m in machines:
            m.P += complex(1e13, 1e13)
            soln = m.math_solve()
            if soln != sys.maxsize:
                total_tokens += soln
        return total_tokens


if __name__ == "__main__":
    Day13()()
