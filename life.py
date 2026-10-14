"""Pure rules and challenge definitions for the terminal Game of Life."""

from dataclasses import dataclass

Cell = tuple[int, int]


@dataclass(frozen=True)
class Challenge:
    name: str
    kind: str
    budget: int
    target: int
    limit: int
    goal: str
    score_unit: str
    hint: str


CHALLENGES = (
    Challenge("Выживание", "survive", 4, 12, 12, "выжить 12 поколений", "поколений", "Подсказка: квадрат 2×2 живёт вечно."),
    Challenge("Вспышка", "population", 5, 20, 30, "вырастить 20 клеток", "клеток", "Подсказка: попробуйте R-пентамино (4)."),
    Challenge("Пульс", "oscillator", 3, 6, 8, "создать колебание до 6-го шага", "поколений", "Подсказка: мигалка (2) меняет форму."),
    Challenge("Улей", "still_life", 6, 10, 10, "6 неподвижных клеток к 10-му шагу", "поколений", "Подсказка: улей (5) — устойчивая фигура."),
    Challenge("Путешествие", "travel", 5, 6, 16, "сдвинуть фигуру на 6 клеток", "клеток", "Подсказка: планер (3) движется по диагонали."),
    Challenge("Буря", "population", 5, 30, 40, "вырастить 30 клеток", "клеток", "Подсказка: R-пентамино (4) растёт долго."),
)

PATTERNS: dict[str, tuple[str, tuple[Cell, ...]]] = {
    "1": ("Блок", ((0, 0), (1, 0), (0, 1), (1, 1))),
    "2": ("Мигалка", ((0, 0), (1, 0), (2, 0))),
    "3": ("Планер", ((1, 0), (2, 1), (0, 2), (1, 2), (2, 2))),
    "4": ("R-пентамино", ((1, 0), (2, 0), (0, 1), (1, 1), (1, 2))),
    "5": ("Улей", ((1, 0), (2, 0), (0, 1), (3, 1), (1, 2), (2, 2))),
}


def translation_distance(initial: set[Cell], current: set[Cell]) -> int:
    """Return Manhattan distance only when the entire shape was translated."""
    if not initial or len(initial) != len(current):
        return 0
    dx = min(x for x, _ in current) - min(x for x, _ in initial)
    dy = min(y for _, y in current) - min(y for _, y in initial)
    return abs(dx) + abs(dy) if current == {(x + dx, y + dy) for x, y in initial} else 0


def challenge_won(
    challenge: Challenge,
    generation: int,
    alive: set[Cell],
    previous: set[Cell],
    two_back: set[Cell] | None,
    initial: set[Cell],
) -> bool:
    if not alive:
        return False
    if challenge.kind == "survive":
        return generation >= challenge.target
    if challenge.kind == "population":
        return len(alive) >= challenge.target
    if challenge.kind == "oscillator":
        return generation >= challenge.target and two_back is not None and alive == two_back and alive != previous
    if challenge.kind == "still_life":
        return generation >= challenge.target and len(alive) >= challenge.budget and alive == previous
    if challenge.kind == "travel":
        return translation_distance(initial, alive) >= challenge.target
    raise ValueError(f"Unknown challenge kind: {challenge.kind}")


def next_generation(alive: set[Cell], width: int, height: int, wrap: bool = False) -> set[Cell]:
    """Apply Conway's B3/S23 rules to a finite board."""
    counts: dict[Cell, int] = {}
    for x, y in alive:
        for dy in (-1, 0, 1):
            for dx in (-1, 0, 1):
                if dx == dy == 0:
                    continue
                nx, ny = x + dx, y + dy
                if wrap:
                    nx, ny = nx % width, ny % height
                elif not (0 <= nx < width and 0 <= ny < height):
                    continue
                counts[(nx, ny)] = counts.get((nx, ny), 0) + 1
    return {cell for cell, count in counts.items() if count == 3 or (count == 2 and cell in alive)}


def rotated(cells: tuple[Cell, ...], turns: int = 1) -> tuple[Cell, ...]:
    """Rotate a pattern clockwise around its bounding box."""
    result = cells
    for _ in range(turns % 4):
        height = max(y for _, y in result) + 1
        result = tuple((height - 1 - y, x) for x, y in result)
    return result


def pattern_at(cells: tuple[Cell, ...], x: int, y: int, width: int, height: int) -> set[Cell] | None:
    placed = {(x + dx, y + dy) for dx, dy in cells}
    if all(0 <= px < width and 0 <= py < height for px, py in placed):
        return placed
    return None
