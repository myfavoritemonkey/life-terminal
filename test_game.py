import unittest
from unittest.mock import patch

from game import Game
from life import CHALLENGES, PATTERNS, pattern_at


class DummyScreen:
    def getmaxyx(self):
        return 15, 48


class GameTests(unittest.TestCase):
    def test_every_challenge_can_be_won_in_game(self):
        for mode, key in enumerate(("1", "4", "2", "5", "3", "4"), start=1):
            with self.subTest(challenge=CHALLENGES[mode - 1].name), patch.object(Game, "save_score"):
                game = Game(DummyScreen())
                game.mode = mode
                game.alive = pattern_at(PATTERNS[key][1], 20, 6, game.width, game.height)
                for _ in range(game.challenge.limit):
                    game.tick()
                    if game.result:
                        break
                self.assertIn("ПОБЕДА", game.result)


if __name__ == "__main__":
    unittest.main()
