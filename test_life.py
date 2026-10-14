import unittest

from life import CHALLENGES, PATTERNS, challenge_won, next_generation, pattern_at, rotated, translation_distance


class LifeTests(unittest.TestCase):
    def test_block_stays_still(self):
        block = {(2, 2), (3, 2), (2, 3), (3, 3)}
        self.assertEqual(next_generation(block, 10, 10), block)

    def test_blinker_oscillates(self):
        horizontal = {(2, 3), (3, 3), (4, 3)}
        vertical = {(3, 2), (3, 3), (3, 4)}
        self.assertEqual(next_generation(horizontal, 10, 10), vertical)
        self.assertEqual(next_generation(vertical, 10, 10), horizontal)

    def test_glider_moves_diagonally(self):
        glider = pattern_at(PATTERNS["3"][1], 3, 3, 20, 20)
        state = glider
        for _ in range(4):
            state = next_generation(state, 20, 20)
        self.assertEqual(state, {(x + 1, y + 1) for x, y in glider})

    def test_wrap_connects_edges(self):
        cells = {(0, 0), (1, 0), (9, 0)}
        self.assertIn((0, 9), next_generation(cells, 10, 10, wrap=True))
        self.assertNotIn((0, 9), next_generation(cells, 10, 10))

    def test_challenges_are_winnable_with_supplied_patterns(self):
        for mode, key in enumerate(("1", "4", "2", "5", "3", "4")):
            challenge = CHALLENGES[mode]
            state = pattern_at(PATTERNS[key][1], 20, 6, 40, 12)
            self.assertLessEqual(len(state), challenge.budget)
            initial = state.copy()
            history = [state.copy()]
            reached = False
            for generation in range(1, challenge.limit + 1):
                state = next_generation(state, 40, 12)
                previous = history[-1]
                two_back = history[-2] if len(history) > 1 else None
                if challenge_won(challenge, generation, state, previous, two_back, initial):
                    reached = True
                    break
                history.append(state.copy())
            self.assertTrue(reached, challenge.name)

    def test_special_goals_reject_wrong_shapes(self):
        block = pattern_at(PATTERNS["1"][1], 10, 4, 40, 12)
        self.assertFalse(challenge_won(CHALLENGES[2], 6, block, block, block, block))
        self.assertFalse(challenge_won(CHALLENGES[3], 10, block, block, block, block))
        self.assertEqual(translation_distance(block, {(11, 4), (12, 4), (11, 5), (13, 5)}), 0)
        self.assertEqual(translation_distance(block, {(x + 3, y + 3) for x, y in block}), 6)

    def test_rotation_and_bounds(self):
        self.assertEqual(set(rotated(PATTERNS["2"][1])), {(0, 0), (0, 1), (0, 2)})
        self.assertIsNone(pattern_at(PATTERNS["1"][1], 9, 9, 10, 10))


if __name__ == "__main__":
    unittest.main()
