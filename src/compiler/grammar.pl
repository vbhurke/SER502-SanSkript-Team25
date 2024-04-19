:- discontiguous variable_declaration/3.

%:- use_rendering(svgtree).
identifier(I) --> i(I).

i(a) --> [a].
i(b) --> [b].
i(c) --> [c].
i(d) --> [d].
i(e) --> [e].
i(f) --> [f].
i(g) --> [g].
i(h) --> [h].
i(i) --> [i].
i(j) --> [j].
i(k) --> [k].
i(l) --> [l].
i(m) --> [m].
i(n) --> [n].
i(o) --> [o].
i(p) --> [p].
i(q) --> [q].
i(r) --> [r].
i(s) --> [s].
i(t) --> [t].
i(u) --> [u].
i(v) --> [v].
i(w) --> [w].
i(x) --> [x].
i(y) --> [y].
i(z) --> [z].

% Define numbers. Made easier for parsing in tree.
value(V) --> n(V).
n(0) --> [0].
n(1) --> [1].
n(2) --> [2].
n(3) --> [3].
n(4) --> [4].
n(5) --> [5].
n(6) --> [6].
n(7) --> [7].
n(8) --> [8].
n(9) --> [9].

%Variable Declaration and Assignment
variable_declaration(var_decl(T,I,=,V))--> type(T), identifier(I), [=], value(V),['||'].
%value to be replaced by expression 
assignment(assig(I,=,E))--> identifier(I), [=], value(E).

%Declaring datatypes

type(typ(int))--> [int].
type(typ(flt))--> [float].
type(typ(vrbl))--> [bool].
type(typ(str))--> [string].