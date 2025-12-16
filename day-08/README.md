This one broke me. I rewrote solution 1 four times because I kept getting the question wrong. `Delete` was also not behaving as expected when the item was at the head of a list, so I set items to `nil` and cleaned them up afterward.

I ended up rewriting it a fifth time since I missed an edge case where a connection happens between two established circuits. This one worked.

The way I built up the list appears to be confusing the sorting algorithms. Both `sort <` and `nreverse` were destroying elements in the list. Using `sort >` and `reverse` got everything in the correct order for solution 2.
