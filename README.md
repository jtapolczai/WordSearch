# WordSearch
Solver for that mobile word game where you have a 3x3 square of letters
and have to draw a word from at least 3 contiguous ones. I forgot what it's called.

This program offers the bare minimum of functionality and mainly exists to satisfy my urge to
create automated solvers for combinatorial games, the manual playing of which I consider an insult
to people's intelligence.

Usage
-----

Compile/load up WordSearch.hs in GHCI, enter the name of your words file (not supplied; has to be a newline-separated
list of words), the line-width of the letter grid (3 in that game) and the list of letters, going from left to right
(e.g. the grid

```
ABC
DEF
GHI
```

would be `abcdefghi`). The solver then spits out all words in the words-file which can be constructed by dragging
one's finger across the letters of the grid.

Technical details
-----------------

The algorithm is a standard BFS that limits exploring unnecessary letter combinations - i.e. combinations that
that aren't the prefix of any listed word - via a trie. It's nothing fancy, but it works very well in practice.
