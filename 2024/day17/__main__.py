import re
from typing import Dict, List, Tuple

from common.aoc_day import AoCDay


class Day17(AoCDay):
    def process_input(self, raw_input: List[str]) -> Tuple[Dict[str, int], List[int]]:
        registers = {}
        register_re = re.compile(r".*([A|B|C]):\s(\d+)")
        for line in raw_input:
            if "Register" in line:
                reg, value = register_re.match(line).groups()
                registers[reg] = int(value)
            elif "Program" in line:
                code = list(map(int, line.split(" ")[1].split(",")))

        return registers, code

    def run_program(self, registers: Dict[str, int], code: List[int]) -> List[int]:
        ADV, BXL, BST, JNZ, BXC, OUT, BDV, CDV = range(8)
        A, B, C = registers["A"], registers["B"], registers["C"]

        idx = 0
        output = []
        while idx < len(code):
            next_idx = idx + 2
            opcode, operand = code[idx], code[idx + 1]

            if opcode in [ADV, BST, OUT, BDV, CDV]:
                if operand == 4:
                    operand = A
                elif operand == 5:
                    operand = B
                elif operand == 6:
                    operand = C

            if opcode == ADV:
                A = A // (2**operand)
            elif opcode == BXL:
                B = B ^ operand
            elif opcode == BST:
                B = operand % 8
            elif opcode == JNZ and A:
                next_idx = operand
            elif opcode == BXC:
                B = B ^ C
            elif opcode == OUT:
                output.append(operand % 8)
            elif opcode == BDV:
                B = A // (2**operand)
            elif opcode == CDV:
                C = A // (2**operand)

            idx = next_idx

        return output

    def part1(self, puzzle_input: Tuple[Dict[str, int], List[int]]) -> str:
        registers, code = puzzle_input
        output = self.run_program(registers, code)
        return ",".join(map(str, output))

    def part2(self, puzzle_input: Tuple[Dict[str, int], List[int]]) -> int:
        _, code = puzzle_input
        jump_size = len(code) - 1
        reg_a = 8 ** (jump_size)
        while True:
            registers = {"A": reg_a, "B": 0, "C": 0}
            output = self.run_program(registers, code)
            if output == code:
                return reg_a
            for i in range(-1, -jump_size - 1, -1):
                if output[i] != code[i]:
                    break
            reg_a += 8 ** (jump_size + i)


if __name__ == "__main__":
    Day17()()
