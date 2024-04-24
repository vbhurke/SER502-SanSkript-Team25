# Author - Milind Deshpande, Rushabh Gajab, Siddharth Sharma, Viresh Bhurke
# Purpose - Grammar
# Version - Final
# Date - 23 April 2024

:- discontiguous variable_declaration/3, i/3, conditional_block/3.
:- discontiguous statement/3, evaluator/3, value/3,print_statement/3.
%:- use_rendering(svgtree).

% Helper member predicate to check in list
mem(H,[H|_]).
mem(H,[_|T]):- mem(H,T).

%Program
program(prgm(B)) --> block(B).

%Block
block(blk(aarambh, D, S, antah)) --> [aarambh], declarations(D), statements(S), [antah].

% Declarations
declaration(decl(V)) --> variable_declaration(V).

declarations(decls(V)) --> declaration(V).
declarations(decls(V,Vs)) --> declaration(V), declarations(Vs).

%different statement rule defined.
statements(stats(S)) --> statement(S),['||'].
statements(stats(S,Ss)) --> statement(S),['||'],statements(Ss).
statement(cond_blk(S)) --> conditional_block(S).
%statements(stats(S,Ss)) --> conditional_block(S),statements(Ss).
%Added loops to statements.
statement(S) --> loops(S).
%statements(stats(S,Ss)) --> loops(S),statements(Ss).

statement(stat(S)) --> assignment(S).
statement(stat(S)) --> increment_operation(S).


% Print Statement
statement(print_stmt(S)) --> print_statement(S).

% create loops for nested
loops(lps(S,Ss)) --> loop(S), loops(Ss).
loops(L) --> loop(L).
% create loop
loop(lp(S)) --> traditional_whileloop(S).
%created for loop and for in range loop.
loop(lp(S)) --> traditional_forloop(S).
loop(lp(S)) --> range_forloop(S).

%conditional block
conditional_block(cond_blk(S)) --> if_then_block(S).
if_then_block(if_then_blk(Condition,S)) --> [if],['('],condition(Condition),[')'],[then],['('],statements(S),[')'].

%conditional block
conditional_block(cond_blk(S)) --> if_then_else_block(S).
if_then_else_block(if_then_else_blk(Condition,S1,S2)) --> [if],['('],condition(Condition),[')'],[then],['('],statements(S1),[')'],[else],['('],statements(S2),[')'].

traditional_whileloop(trd_while_blk(while,Condition,Ss)) --> [while],['('],condition(Condition),[')'],['('],statements(Ss),[')'].
% rules for traditional for loop and for in range loop.
traditional_forloop(trd_for_blk(for,I,V,Condition,I,Op,Ss)) --> [for],['('],identifier(I),[=],value(V),['||'],condition(Condition),['||'],increment_operation(Op),[')'],['('],statements(Ss),[')'].
range_forloop(rng_for_loop(for,I,in,range,N,->,M,Ss)) -->  [for],identifier(I),[in],[range],['('],value(N),[->],value(M),[')'],['('],statements(Ss),[')'].

%Ternary Operator:
conditional_block(cond_blk(S)) --> ternary_operator_block(S).
ternary_operator_block(tern_op_blk(Condition,?,S1,:,S2)) --> condition(Condition),['?'],statements(S1),[':'],statements(S2).

%Increment Operations
increment_operation(incr_op(I,++)) --> identifier(I),[++].
increment_operation(incr_op(I,--)) --> identifier(I),[--].

condition(cond(E1,Ri,E2))--> expression(E1),relational_identifier(Ri),expression(E2).
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
term(val(N)) --> value(N).
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

% Return answer for evaluation

% Evaluator for evaluating
evaluator(Expr, Substitutions, Ans) :-
    evaluation(Expr, Substitutions, Ans).

% Check tree for substitutions
evaluation(term(I), Substitutions, Variable) :-
    mem((_,I, Variable), Substitutions).
evaluation(val(Value), _, Value).

% Addition operation
evaluation(add(Exp1, +, Exp2), Substitutions, Ans) :-
    evaluation(Exp1, Substitutions, Exp1_Ans),
    evaluation(Exp2, Substitutions, Exp2_Ans),
    Ans is Exp1_Ans + Exp2_Ans.

% Subtraction operation
evaluation(sub(Exp1, -, Exp2), Substitutions, Ans) :-
    evaluation(Exp1, Substitutions, Exp1_Ans),
    evaluation(Exp2, Substitutions, Exp2_Ans),
    Ans is Exp1_Ans - Exp2_Ans.

% Multiplication Operation
evaluation(mul(Exp1, *, Exp2), Substitutions, Ans) :-
    evaluation(Exp1, Substitutions, Exp1_Ans),
    evaluation(Exp2, Substitutions, Exp2_Ans),
    Ans is Exp1_Ans * Exp2_Ans.

% Division Operation
evaluation(div(Exp1, /, Exp2), Substitutions, Ans) :-
    evaluation(Exp1, Substitutions, Exp1_Ans),
    evaluation(Exp2, Substitutions, Exp2_Ans),
    Ans is Exp1_Ans / Exp2_Ans.

eval_bool(cond(true), _, true). 
eval_bool(cond(false), _, false). 
 
eval_bool(cond(E1, ==, E2), Env, Result) :- 
    % Evaluate the expressions 
    evaluator(E1, Env, Val1), 
    evaluator(E2, Env, Val2), 
    % Check if the expressions are equal 
    (Val1 =:= Val2 -> Result = true ; Result = false).

eval_bool(cond(E1, >=, E2), Env, Result) :- 
    % Evaluate the expressions 
    evaluator(E1, Env, Val1), 
    evaluator(E2, Env, Val2), 
    % Check if the expressions are equal 
    (Val1 >= Val2 -> Result = true ; Result = false).

eval_bool(cond(E1, <=, E2), Env, Result) :- 
    % Evaluate the expressions 
    evaluator(E1, Env, Val1), 
    evaluator(E2, Env, Val2), 
    % Check if the expressions are equal 
    (Val1 =< Val2 -> Result = true ; Result = false).

eval_bool(cond(E1, >, E2), Env, Result) :- 
    % Evaluate the expressions 
    evaluator(E1, Env, Val1), 
    evaluator(E2, Env, Val2), 
    % Check if the expressions are equal 
    (Val1 > Val2 -> Result = true ; Result = false).

eval_bool(cond(E1, <, E2), Env, Result) :- 
    % Evaluate the expressions 
    evaluator(E1, Env, Val1), 
    evaluator(E2, Env, Val2), 
    % Check if the expressions are equal 
    (Val1 < Val2 -> Result = true ; Result = false).

eval_bool(cond(E1, =/=, E2), Env, Result) :- 
    % Evaluate the expressions 
    evaluator(E1, Env, Val1), 
    evaluator(E2, Env, Val2), 
    % Check if the expressions are equal 
    (Val1 \= Val2 -> Result = true ; Result = false).

    eval_var_declaration(var_decl(T, I, _, V), Env, NewEnv) :- 
        % Add constant to environment 
        NewEnv = [(T,I, V) | Env]. 
    
    eval_decl(decl(V), Env, NewEnv) :- 
        eval_var_declaration(V, Env, NewEnv).
    
    eval_decls(decls(V), Env, FinEnv) :- 
        eval_decl(V, Env, FinEnv).
    
    eval_decls(decls(V,Vs), Env, FinEnv) :- 
        eval_decl(V, Env, Env1),
        eval_decls(Vs, Env1, FinEnv).   
    
    eval_assignment(assig(I,=,E),Env,NewEnv):-
        evaluator(E, Env, R),
        update(_,I,R,Env,NewEnv).

eval_stat(stat(S), Env, FinEnv):- eval_assignment(S, Env, FinEnv).
eval_stat(stat(S), Env, FinEnv):- eval_increment_operation(S, Env, FinEnv).
eval_stats(stats(S), Env, FinEnv):- eval_stat(S, Env, FinEnv).
eval_stats(stats(S,Ss), Env, FinEnv):- eval_stat(S, Env, Env1), eval_stats(Ss, Env1, FinEnv).

eval_increment_operation(incr_op(I,++),Env,NewEnv) :- lookup(int,I,Env,V), K is V+1, 
    update(_,I,K,Env,NewEnv).

eval_increment_operation(incr_op(I,--),Env,NewEnv) :- lookup(int,I,Env,V), K is V-1, 
    update(_,I,K,Env,NewEnv).

%Update value in Environment
update(Typ,Id, Val, [], [(Typ,Id,Val)]). 
update(Typ,Id, Val, [(Typ,Id,_)|T], [(Typ,Id,Val)|T]). 
update(Typ,Id, Val, [H|T], [H|R]) :- H\=(Typ,Id,_),update(Typ,Id,Val,T,R).
%Lookup value in environment 
lookup(_,_,[],_). 
lookup(Typ,Id,[(Typ,Id,Val)|_],Val). 
lookup(Typ,Id,[_|T],Val):- lookup(Typ,Id,T,Val). 