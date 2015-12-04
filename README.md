# AdventCalendar2015
Probie's solutions to the Advent of Code problems

If you can't read them, install a font with decent unicode support.
It's 2015 (and nearly 2016), this should not be a problem these days.

To run one of them, either add a main like
`main = readFile "input.txt" >>= print . part1`
or just type something like ``part1 `fmap` readFile "input.txt"`` into GHCi

I currently use the following packages (and if you want to run my code,
you'll need them).

- split

- base-unicode-symbols

- puremd5