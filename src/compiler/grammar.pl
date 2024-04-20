:- discontiguous variable_declaration/3, i/3, conditional_block/3.
:- discontiguous statement/3.
%:- use_rendering(svgtree).

%Program
program(prgm(B)) --> block(B).

%Block
block(blk(aarambh, D, '||', S, antah)) --> [aarambh], declarations(D), statements(S), [antah].

% Declarations
declaration(decl(V)) --> variable_declaration(V).
declarations(decl(V)) --> declaration(V).
declarations(decl(V,Vs)) --> declaration(V), declarations(Vs).

%different statement rule defined.
statements(stats(S)) --> statement(S),['||'].
statements(stats(S,Ss)) --> statement(S),['||'],statements(Ss).
statements(cond_blk(S)) --> conditional_block(S).

statement(stat(S)) --> assignment(S).

% Print Statement
statement(print_stmt(S)) --> print_statement(S).

% create loops for nested
loops(lps(S,Ss)) --> loop(S), loops(Ss).
loops(L) --> loop(L).
% create loop
loop(lp(S)) --> traditional_whileloop(S).

%conditional block
conditional_block(cond_blk(S)) --> if_then_block(S).
if_then_block(if_then_blk(Exp,S)) --> [if],['('],expression(Exp),[')'],[then],['('],statements(S),[')'].
conditional_block(cond_blk(S)) --> if_else_block(S).
if_else_block(if_else_blk(if,Exp,else,S)) --> [if],['('],expression(Exp),[')'],[else],['('],statements(S),[')'].

traditional_whileloop(trd_while_blk(while,I,Ri,E,Ss)) --> [while],['('],identifier(I),relational_identifier(Ri),expression(E),[')'],['('],statements(Ss),[')'].

%Increment Operations
increment_operation(incr_op(I,++)) --> identifier(I),[++].
increment_operation(incr_op(I,--)) --> identifier(I),[--].

%Relational operators
relational_identifier(<) --> [<].
relational_identifier(<=) --> [<=].
relational_identifier(>) --> [>].
relational_identifier(>=) --> [>=].
relational_identifier(==) --> [==].
% using =/= imstead of != as it is giving oprerator
relational_identifier(=/=) --> [=/=].

% Print Statements For evaluation of expressions and Strings.
print_statement(print_stmt(P)) --> [likhyam],['('],expression(P),[')'].
print_statement(print_stmt(W)) --> print_statement_word(W).
print_statement_word(print_stmt_Word(X)) --> [likhyam, '(', X, ')'].

%terms for identifiers and values
term(term(I)) --> i(I).
term(val(N)) --> n(N).
%Handling brackets in terms
term(A) --> ['('], expression(A), [')'].

%Expressions defined
expression(E) --> expr_add_sub(E).
%Expression for addition and subtraction
expr_add_sub(A) --> term(A).
expr_add_sub(store(I,=,E)) --> expr_mul_div(I), [=], expression(E).
expr_add_sub(sub(A, -, B)) --> expr_mul_div(A), [-], expression(B).
expr_add_sub(add(A, +, B)) --> expr_mul_div(A), [+], expression(B).
expr_add_sub(A) --> expr_mul_div(A).
% Giving priority to multiplication and division
expr_mul_div(A) --> term(A).
expr_mul_div(mul(A, *, B)) --> term(A), [*], expr_mul_div(B).
expr_mul_div(div(A, /, B)) --> term(A), [/], expr_mul_div(B).

% Declared initial to initialize
i(store(a,=,E)) --> [a],[=], expression(E).
i(store(b,=,E)) --> [b],[=], expression(E).
i(store(c,=,E)) --> [c],[=], expression(E).
i(store(d,=,E)) --> [d],[=], expression(E).
i(store(e,=,E)) --> [e],[=], expression(E).
i(store(f,=,E)) --> [f],[=], expression(E).
i(store(g,=,E)) --> [g],[=], expression(E).
i(store(h,=,E)) --> [h],[=], expression(E).
i(store(i,=,E)) --> [i],[=], expression(E).
i(store(j,=,E)) --> [j],[=], expression(E).
i(store(k,=,E)) --> [k],[=], expression(E).
i(store(l,=,E)) --> [l],[=], expression(E).
i(store(m,=,E)) --> [m],[=], expression(E).
i(store(n,=,E)) --> [n],[=], expression(E).
i(store(o,=,E)) --> [o],[=], expression(E).
i(store(p,=,E)) --> [p],[=], expression(E).
i(store(q,=,E)) --> [q],[=], expression(E).
i(store(r,=,E)) --> [r],[=], expression(E).
i(store(s,=,E)) --> [s],[=], expression(E).
i(store(t,=,E)) --> [t],[=], expression(E).
i(store(u,=,E)) --> [u],[=], expression(E).
i(store(v,=,E)) --> [v],[=], expression(E).
i(store(w,=,E)) --> [w],[=], expression(E).
i(store(x,=,E)) --> [x],[=], expression(E).
i(store(y,=,E)) --> [y],[=], expression(E).
i(store(z,=,E)) --> [z],[=], expression(E).

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
%assignment
assignment(assig(I,=,E))--> identifier(I), [=], expression(E).

%Declaring datatypes

type(typ(int))--> [int].
type(typ(flt))--> [float].
type(typ(vrbl))--> [bool].
type(typ(str))--> [string].