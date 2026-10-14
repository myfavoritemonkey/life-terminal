#!/usr/bin/env python3
"""Interactive terminal Game of Life. Run with: python3 game.py"""

import curses
import json
import locale
import random
import time
from pathlib import Path

from life import CHALLENGES, PATTERNS, challenge_won, next_generation, pattern_at, rotated, translation_distance

SCORES_FILE = Path(__file__).with_name("scores.json")
MIN_COLS, MIN_LINES = 48, 15


def load_scores() -> dict[str, int]:
    try:
        data = json.loads(SCORES_FILE.read_text(encoding="utf-8"))
        if not isinstance(data, dict):
            return {}
        return {str(key): int(value) for key, value in data.items() if isinstance(value, int)}
    except (OSError, ValueError, TypeError):
        return {}


class Game:
    def __init__(self, screen):
        self.screen = screen
        lines, cols = screen.getmaxyx()
        self.width = max(40, min(72, cols - 2))
        self.height = max(12, min(30, lines - 7))
        self.scores = load_scores()
        self.mode = 0
        self.speed = 5
        self.wrap = False
        self.pattern = "3"
        self.rotation = 0
        self.help = False
        self.message = "Стрелки/WASD — курсор; пробел — клетка; Enter — запуск. H — помощь."
        self.reset()

    @property
    def challenge(self):
        return CHALLENGES[self.mode - 1] if self.mode else None

    def reset(self):
        self.alive: set[tuple[int, int]] = set()
        self.cursor_x, self.cursor_y = self.width // 2, self.height // 2
        self.generation = 0
        self.peak = 0
        self.farthest = 0
        self.initial: set[tuple[int, int]] = set()
        self.previous: set[tuple[int, int]] = set()
        self.two_back: set[tuple[int, int]] | None = None
        self.running = False
        self.result = ""
        self.last_tick = time.monotonic()

    def save_score(self):
        key = str(self.mode)
        kind = self.challenge.kind
        score = self.peak if kind == "population" else self.farthest if kind == "travel" else self.generation
        if score > self.scores.get(key, 0):
            self.scores[key] = score
            try:
                SCORES_FILE.write_text(json.dumps(self.scores, ensure_ascii=False, indent=2) + "\n", encoding="utf-8")
            except OSError:
                self.message = "Рекорд достигнут, но сохранить scores.json не удалось."

    def tick(self):
        if not self.generation:
            self.initial = self.alive.copy()
        self.two_back = self.previous if self.generation else None
        self.previous = self.alive.copy()
        self.alive = next_generation(self.alive, self.width, self.height, self.wrap)
        self.generation += 1
        self.peak = max(self.peak, len(self.alive))
        challenge = self.challenge
        if challenge:
            if challenge.kind == "travel":
                self.farthest = max(self.farthest, translation_distance(self.initial, self.alive))
            if challenge_won(challenge, self.generation, self.alive, self.previous, self.two_back, self.initial):
                self.result = f"ПОБЕДА! {challenge.name} пройдено. T — следующее испытание."
            elif not self.alive or self.generation >= challenge.limit:
                self.result = "Поражение. C — начать испытание заново."
            if self.result:
                self.running = False
                self.save_score()
        elif not self.alive:
            self.running = False
            self.message = "Поле пусто. Добавьте клетки и попробуйте снова."

    def edit_allowed(self):
        if self.challenge and self.generation:
            self.message = "После запуска поле закрыто для редактирования. C — начать заново."
            return False
        return True

    def toggle(self):
        if not self.edit_allowed():
            return
        cell = (self.cursor_x, self.cursor_y)
        if cell in self.alive:
            self.alive.remove(cell)
        elif self.challenge and len(self.alive) >= self.challenge.budget:
            self.message = f"Лимит стартовых клеток: {self.challenge.budget}."
            return
        else:
            self.alive.add(cell)
        self.peak = max(self.peak, len(self.alive))

    def stamp(self):
        if not self.edit_allowed():
            return
        cells = rotated(PATTERNS[self.pattern][1], self.rotation)
        placed = pattern_at(cells, self.cursor_x, self.cursor_y, self.width, self.height)
        if placed is None:
            self.message = "Фигура выходит за границы поля."
        elif self.challenge and len(self.alive | placed) > self.challenge.budget:
            self.message = f"Лимит стартовых клеток: {self.challenge.budget}."
        else:
            self.alive.update(placed)
            self.peak = max(self.peak, len(self.alive))
            self.message = f"Поставлена фигура: {PATTERNS[self.pattern][0]}."

    def handle(self, key):
        if key in (ord("q"), ord("Q"), 27):
            return False
        if key in (curses.KEY_UP, ord("w"), ord("W")):
            self.cursor_y = max(0, self.cursor_y - 1)
        elif key in (curses.KEY_DOWN, ord("s"), ord("S")):
            self.cursor_y = min(self.height - 1, self.cursor_y + 1)
        elif key in (curses.KEY_LEFT, ord("a"), ord("A")):
            self.cursor_x = max(0, self.cursor_x - 1)
        elif key in (curses.KEY_RIGHT, ord("d"), ord("D")):
            self.cursor_x = min(self.width - 1, self.cursor_x + 1)
        elif key == ord(" "):
            self.toggle()
        elif key in (10, 13, curses.KEY_ENTER):
            if not self.result and self.alive:
                self.running = not self.running
                self.last_tick = time.monotonic()
            elif not self.alive:
                self.message = "Сначала добавьте клетки."
        elif key in (ord("n"), ord("N")):
            if not self.running and not self.result:
                self.tick()
        elif key in (ord("c"), ord("C")):
            self.reset()
            self.message = "Поле очищено."
        elif key in (ord("r"), ord("R")):
            if self.challenge:
                self.message = "Случайное заполнение доступно в песочнице."
            else:
                self.reset()
                self.alive = {(x, y) for y in range(self.height) for x in range(self.width) if random.random() < 0.22}
                self.peak = len(self.alive)
                self.message = "Случайное поле создано."
        elif key in (ord("t"), ord("T")):
            self.mode = (self.mode + 1) % (len(CHALLENGES) + 1)
            self.wrap = False
            self.reset()
            self.message = f"{self.challenge.name}. {self.challenge.hint}" if self.challenge else "Режим: Песочница."
        elif key in (ord("b"), ord("B")):
            if self.challenge:
                self.message = "Сквозные границы доступны в песочнице."
            else:
                self.wrap = not self.wrap
                self.message = "Границы: сквозные." if self.wrap else "Границы: обычные."
        elif key in (ord("h"), ord("H"), ord("?")):
            self.help = not self.help
        elif key in (ord("p"), ord("P")):
            self.stamp()
        elif key in (ord("x"), ord("X")):
            self.rotation = (self.rotation + 1) % 4
            self.message = f"Поворот: {self.rotation * 90}°."
        elif 0 <= key < 256 and chr(key) in PATTERNS:
            self.pattern = chr(key)
            self.message = f"Фигура: {PATTERNS[self.pattern][0]}. P — поставить."
        elif key in (ord("+"), ord("=")):
            self.speed = min(3000, self.speed + 1)
        elif key == ord("-"):
            self.speed = max(1, self.speed - 1)
        elif key == curses.KEY_MOUSE:
            try:
                _, mx, my, _, state = curses.getmouse()
                left = (state & curses.BUTTON1_CLICKED) or (state & curses.BUTTON1_PRESSED)
                lines, cols = self.screen.getmaxyx()
                view_w = min(self.width, cols - 2)
                view_h = min(self.height, lines - 7)
                view_left = min(max(0, self.cursor_x - view_w // 2), self.width - view_w)
                view_top = min(max(0, self.cursor_y - view_h // 2), self.height - view_h)
                if left and 1 <= my <= view_h and 1 <= mx <= view_w:
                    self.cursor_x, self.cursor_y = view_left + mx - 1, view_top + my - 1
                    self.toggle()
            except curses.error:
                pass
        return True

    def put(self, y, x, value, attr=0):
        lines, cols = self.screen.getmaxyx()
        if 0 <= y < lines and 0 <= x < cols:
            try:
                self.screen.addnstr(y, x, value, cols - x - 1, attr)
            except curses.error:
                pass

    def draw(self):
        screen = self.screen
        screen.erase()
        lines, cols = screen.getmaxyx()
        if lines < MIN_LINES or cols < MIN_COLS:
            self.put(0, 0, f"Увеличьте терминал до {MIN_COLS}×{MIN_LINES}. Сейчас {cols}×{lines}.")
            screen.refresh()
            return

        view_w = min(self.width, cols - 2)
        view_h = min(self.height, lines - 7)
        left = min(max(0, self.cursor_x - view_w // 2), self.width - view_w)
        top = min(max(0, self.cursor_y - view_h // 2), self.height - view_h)
        title = " ЖИЗНЬ │ " + (self.challenge.name if self.challenge else "Песочница")
        self.put(0, 0, title, curses.A_BOLD)
        for sy in range(view_h):
            y = top + sy
            row = ["█" if (x, y) in self.alive else "·" for x in range(left, left + view_w)]
            self.put(sy + 1, 1, "".join(row))
        cx, cy = self.cursor_x - left + 1, self.cursor_y - top + 1
        self.put(cy, cx, "█" if (self.cursor_x, self.cursor_y) in self.alive else "·", curses.A_REVERSE | curses.A_BOLD)
        base = view_h + 1
        self.put(base, 0, "─" * min(view_w + 2, cols - 1))
        state = "Идёт" if self.running else "Пауза"
        if self.challenge:
            ch = self.challenge
            self.put(base + 1, 0, f"Шаг {self.generation}/{ch.limit}  Живых {len(self.alive)}  Старт {len(self.initial) if self.generation else len(self.alive)}/{ch.budget}  {state}")
            self.put(base + 2, 0, f"Цель: {ch.goal}  │  рекорд {self.scores.get(str(self.mode), 0)} {ch.score_unit}")
        else:
            self.put(base + 1, 0, f"Поколение {self.generation}  Живых {len(self.alive)}  Пик {self.peak}  {state}  {self.speed} шаг/с")
            self.put(base + 2, 0, f"Фигура {self.pattern}: {PATTERNS[self.pattern][0]} │ P поставить │ X повернуть │ границы: {'сквозные' if self.wrap else 'обычные'}")
        self.put(base + 3, 0, self.result or self.message, curses.A_BOLD if self.result else 0)
        self.put(base + 4, 0, "Enter старт/пауза │ N шаг │ T режим │ C заново │ H помощь │ Q выход")
        if self.help:
            tips = [
                " УПРАВЛЕНИЕ ",
                " Стрелки или WASD: перемещать курсор",
                " Пробел / мышь: добавить или убрать клетку",
                " Enter: пуск/пауза; N: одно поколение",
                " 1 блок  2 мигалка  3 планер  4 R-пентамино",
                " 5 улей",
                " P: поставить фигуру; X: повернуть",
                " +/-: скорость; B: граница; R: случайное поле",
                " T: сменить режим; C: начать заново",
                " В испытании клетки ставятся до первого шага.",
                " H: закрыть помощь",
            ]
            for i, tip in enumerate(tips[: max(0, lines - 2)]):
                self.put(i + 1, 1, tip.ljust(min(cols - 3, 52)), curses.A_REVERSE)
        screen.refresh()

    def run(self):
        self.screen.keypad(True)
        self.screen.timeout(50)
        try:
            curses.curs_set(0)
        except curses.error:
            pass
        try:
            curses.mousemask(curses.ALL_MOUSE_EVENTS)
        except curses.error:
            pass
        while True:
            if self.running and time.monotonic() - self.last_tick >= 1 / self.speed:
                self.tick()
                self.last_tick = time.monotonic()
            self.draw()
            key = self.screen.getch()
            if key != -1 and not self.handle(key):
                break


def main():
    locale.setlocale(locale.LC_ALL, "")
    curses.wrapper(lambda screen: Game(screen).run())


if __name__ == "__main__":
    main()
