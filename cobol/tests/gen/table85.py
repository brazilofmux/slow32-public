#!/usr/bin/env python3
"""Table handling as X3.23-1985 describes it, written out independently
of either compiler: a reference oracle for tests/gen (gen-table.py).

- SEARCH, format 1 (VI-124, general rule 2): from the index's current
  setting; at each occurrence the WHEN conditions in the order written;
  none satisfied, the index goes up by one; past the highest permissible
  occurrence, AT END, the index left there.  Satisfied, the index stays
  at the occurrence that satisfied it.
- SEARCH ALL (rule 4): the occurrence whose key equals; none, AT END
  (the index then unpredictable, so not shown).
- A variable-length group whose DEPENDING ON item is outside it, as a
  sending or receiving item, has its current length (VI-28 OCCURS, rule
  3a, as X3.23-1985 changed it: XVII-54); a MOVE into it is the
  alphanumeric MOVE (VI-104), the rest of the group's storage unchanged.
"""


def move_x(value, size):
    return (value + " " * size)[:size]


def search(start, whens, n):
    """whens: [(predicate(occurrence) -> bool, label)];
    returns (label or 'end', final index)"""
    i = start
    while i <= n:
        for pred, label in whens:
            if pred(i):
                return label, i
        i += 1
    return "end", i


def search_all(keys, target):
    for i, k in enumerate(keys, 1):
        if k == target:
            return "hit", i
    return "end", None


def odo_move_into(storage, length, value):
    """storage: the group's whole content; a MOVE at the current length"""
    return move_x(value, length) + storage[length:]
