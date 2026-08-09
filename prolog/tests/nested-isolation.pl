% Nested findall / \+ must not destroy remaining parent conjunction goals
% that sit deeper on the goal stack than a truncated 64-entry window.

age(a, 1).
age(b, 2).
age(c, 3).

% Long conjunction of true before findall: forces many remaining goals
% on the stack when findall runs. After findall, write must still run.
?- true, true, true, true, true, true, true, true, true, true,
   true, true, true, true, true, true, true, true, true, true,
   true, true, true, true, true, true, true, true, true, true,
   true, true, true, true, true, true, true, true, true, true,
   true, true, true, true, true, true, true, true, true, true,
   true, true, true, true, true, true, true, true, true, true,
   true, true, true, true, true, true, true, true, true, true,
   findall(X, age(X, _), L), write(L), nl.

% Same pattern with negation as failure.
?- true, true, true, true, true, true, true, true, true, true,
   true, true, true, true, true, true, true, true, true, true,
   true, true, true, true, true, true, true, true, true, true,
   true, true, true, true, true, true, true, true, true, true,
   true, true, true, true, true, true, true, true, true, true,
   true, true, true, true, true, true, true, true, true, true,
   true, true, true, true, true, true, true, true, true, true,
   \+ age(z, _), write(ok), nl.
