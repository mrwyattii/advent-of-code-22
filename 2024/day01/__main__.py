from collections import Counter

from common.aoc_day import AoCDay


class Day01(AoCDay):
    def process_input(self, raw_input):
        return list(map(lambda x: tuple(map(int, x.split())), raw_input))

    def part1(self, input):
        left, right = zip(*input)
        left, right = sorted(left), sorted(right)
        print(sum(abs(x - y) for x, y in zip(left, right)))

    def part2(self, input):
        left, right = zip(*input)
        counter = Counter(right)
        print(sum(x * counter[x] for x in left))


if __name__ == "__main__":
    Day01()()
