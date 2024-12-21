from functools import cache
from typing import List, Tuple

from common.aoc_day import AoCDay


class Day19(AoCDay):
    def process_input(self, raw_input: List[str]) -> Tuple[Tuple[str], Tuple[str]]:
        towel_vocab = tuple(s.strip() for s in raw_input[0].strip().split(","))
        towel_words = tuple(s.strip() for s in raw_input[1:])
        return towel_vocab, towel_words

    @cache
    def is_valid_word(self, word_remainder: str, vocab: Tuple[str]) -> int:
        if not word_remainder:
            return 1
        result = 0
        for v in vocab:
            if word_remainder.startswith(v):
                result += self.is_valid_word(word_remainder[len(v) :], vocab)
        return result

    def part1(self, puzzle_input: Tuple[Tuple[str], Tuple[str]]) -> int:
        towel_vocab, towel_words = puzzle_input
        return sum(bool(self.is_valid_word(w, towel_vocab)) for w in towel_words)

    def part2(self, puzzle_input: Tuple[Tuple[str], Tuple[str]]) -> int:
        towel_vocab, towel_words = puzzle_input
        return sum(self.is_valid_word(w, towel_vocab) for w in towel_words)


if __name__ == "__main__":
    Day19()()
