Off by one errors. Also writing some utility functions with loop. That's solution 1. I also misunderstood what the puzzle was asking for and had to retool a little.

After reading puzzle 2 I realized this is a tree problem. I thought about using a tree structure to track paths taken, but realized I can just rotate the grid and do a recursive search forward algorithm and count exit paths. I'm going to have to rewrite the solution. I'm trying not to use an array to figure out if I can do this with lists alone.

My initial solution was recursive, but not tail-optimized. The sample solved instantly, but the interpreter stalled on the actual input. I adjusted the solution to at least limit recursion to the depth of the tree. Ultimately it didn't complete, because the answer was in the trillions and recursively walking each path takes forever. The second solution was an incremental walker. I started to get lazy and make assumptions (there are no splitters along the 'wall' of the tree, nor next to each other). It ran much faster.

Also finally clauses in loops break return values, at least on recursive functions. I got stuck on NIL returns for a while until I figured that out.
