# Author - Milind Deshpande, Rushabh Gajab, Siddharth Sharma, Viresh Bhurke
# Purpose - Grammar
# Version_2 - Final Project
# Date - 26 April 2024

:- discontiguous variable_declaration/3, i/3, conditional_block/3.
:- discontiguous statement/3, evaluator/3,condition/3.
%:- use_rendering(svgtree).

% Helper member predicate to check in list
mem(H,[H|_]).
mem(H,[_|T]):- mem(H,T).

%Program
program(prgm(B)) --> block(B).

%Block
block(blk(aarambh, D, '||', S, antah)) --> [aarambh], declarations(D), statements(S), [antah].

% Declarations
declaration(decl(V)) --> variable_declaration(V).
declaration(decl(S)) --> string_declaration(S).
declarations(decls(V)) --> declaration(V).
declarations(decls(V,Vs)) --> declaration(V), declarations(Vs).
%string 

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
if_then_block(if_then_blk(Bool,S)) --> [if],['('],bool(Bool),[')'],[then],['('],statements(S),[')'].

%conditional block
conditional_block(cond_blk(S)) --> if_then_else_block(S).
if_then_else_block(if_then_else_blk(Condition,S1,S2)) --> [if],['('],condition(Condition),[')'],[then],['('],statements(S1),[')'],[else],['('],statements(S2),[')'].
if_then_else_block(if_then_else_blk(Bool,S1,S2)) --> [if],['('],bool(Bool),[')'],[then],['('],statements(S1),[')'],[else],['('],statements(S2),[')'].

traditional_whileloop(trd_while_blk(while,Condition,Ss)) --> [while],['('],condition(Condition),[')'],['('],statements(Ss),[')'].
traditional_whileloop(trd_while_blk(while,Bool,Ss)) --> [while],['('],bool(Bool),[')'],['('],statements(Ss),[')'].
% rules for traditional for loop and for in range loop.
traditional_forloop(trd_for_blk(for,I,V,Condition,I,Op,Ss)) --> [for],['('],identifier(I),[=],value(V),['||'],condition(Condition),['||'],increment_operation(Op),[')'],['('],statements(Ss),[')'].
traditional_forloop(trd_for_blk(for,I,V,Bool,I,Op,Ss)) --> [for],['('],identifier(I),[=],value(V),['||'],bool(Bool),['||'],increment_operation(Op),[')'],['('],statements(Ss),[')'].

range_forloop(rng_for_loop(for,I,in,range,N,->,M,Ss)) -->  [for],identifier(I),[in],[range],['('],value(N),[->],value(M),[')'],['('],statements(Ss),[')'].


%Ternary Operator:
conditional_block(cond_blk(S)) --> ternary_operator_block(S).
ternary_operator_block(tern_op_blk(Condition,?,S1,:,S2)) --> condition(Condition),['?'],statements(S1),[':'],statements(S2).
ternary_operator_block(tern_op_blk(Bool,?,S1,:,S2)) --> bool(Bool),['?'],statements(S1),[':'],statements(S2).

%Increment Operations
increment_operation(incr_op(I,++)) --> identifier(I),[++].
increment_operation(incr_op(I,--)) --> identifier(I),[--].

condition(cond(E1,Ri,E2))--> expression(E1),relational_identifier(Ri),expression(E2).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
condition(cond(true)) --> [true].
condition(cond(false)) --> [false].
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Relational operators
relational_identifier(<) --> [<].
relational_identifier(<=) --> [<=].
relational_identifier(>) --> [>].
relational_identifier(>=) --> [>=].
relational_identifier(==) --> [==].
% using =/= imstead of != as it is giving oprerator
relational_identifier(=/=) --> [=/=].

%new
bool(bool_t(C1,and,C2))--> condition(C1),[and],condition(C2).
bool(bool_t(C1,or,C2))--> condition(C1),[or],condition(C2).
condition(cond(not,A))--> [not], condition(A).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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

word(S) --> [S], {atom(S)}.

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
n(N) --> [N], {integer(N)}.

%Variable Declaration and Assignment
variable_declaration(var_decl(T,I,=,V))--> type(T), identifier(I), [=], value(V),['||'].
%assignment
assignment(assig(I,=,E))--> identifier(I), [=], expression(E).
assignment(assig(I,=,W))--> identifier(I), [=], word(W).
%string 
string_declaration(str_decl(_,I,=,W))--> [string],identifier(I),[=],['('],word(W),[')'],['||'].

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
    mem((I, Variable), Substitutions).
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


eval_var_declaration(var_decl(_, I, _, V), Env, NewEnv) :- 
    % Add constant to environment 
    NewEnv = [(I, V) | Env]. 

eval_str_declaration(str_decl(_,I,_,W), Env, NewEnv) :- 
    % Add constant to environment 
    NewEnv = [(I, W) | Env]. 

eval_decl(decl(V), Env, NewEnv) :- 
    eval_var_declaration(V, Env, NewEnv).

eval_decl(decl(V), Env, NewEnv) :- 
    eval_str_declaration(V, Env, NewEnv).

eval_decls(decls(V), Env, FinEnv) :- 
    eval_decl(V, Env, FinEnv).

eval_decls(decls(V,Vs), Env, FinEnv) :- 
    eval_decl(V, Env, Env1),
    eval_decls(Vs, Env1, FinEnv).   

eval_assignment(assig(I,=,E),Env,NewEnv):-
    evaluator(E, Env, R),
    update(I,R,Env,NewEnv).

eval_assignment(assig(I,=,W),Env,NewEnv):-
    update(I,W,Env,NewEnv).

eval_stat(stat(S), Env, FinEnv):- eval_assignment(S, Env, FinEnv).
eval_stat(stat(S), Env, FinEnv):- eval_increment_operation(S, Env, FinEnv).
eval_stat(print_stmt(S),Env,Env):- eval_print_statement(S,Env).
eval_stat(S,Env, FinEnv):- eval_loops(S,Env,FinEnv).
eval_stat(cond_blk(S),Env,FinEnv):- eval_conditional_block(S,Env,FinEnv).

eval_stats(stats(S), Env, FinEnv):- eval_stat(S, Env, FinEnv).
eval_stats(stats(S,Ss), Env, FinEnv):- eval_stat(S, Env, Env1), eval_stats(Ss, Env1, FinEnv).

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

%%%%%Eval bool true
eval_bool(cond(true), _, Result) :- 
    % Check if the expressions are equal 
    (Result = true).

%%%%%Eval bool false
eval_bool(cond(false), _, Result) :- 
    % Check if the expressions are equal 
    (Result = false).

%%%%%Eval bool not
eval_bool(cond(not,A), Env, Result) :- 
    % Check if the expressions are equal 
    eval_bool(A,Env,R1),
    (R1 = true -> Result = false ; Result = true).

eval_bool(bool_t(C1,and,C2),Env,Result):-
    eval_bool(C1,Env,Res1),
    eval_bool(C2,Env,Res2),
    (Res1 = Res2 -> Result = Res1 ; Result = false).

eval_bool(bool_t(C1,or,C2),Env,Result):-
    eval_bool(C1,Env,Res1),
    eval_bool(C2,Env,Res2),
    (Res1 \= Res2 -> Result = true ; Result = Res1).
    
%bool(bool_t(E1,and,E2))--> condition(E1),[and],condition(E2).
%bool(bool_t(E1,or,E2))--> condition(E1),[or],condition(E2).

eval_range_for(rng_for_loop(for,_,in,range,M,->,M,_),Env,Env).
eval_range_for(rng_for_loop(for,I,in,range,N,->,M,Ss),Env,FinEnv):- update(I,N,Env,Env1), V is N, V < M,
     eval_stats(Ss, Env1, Env2), K is N+1,update(I,K,Env2,Env3),eval_range_for(rng_for_loop(for,I,in,range,K,->,M,Ss),Env3,FinEnv).

eval_while_loop(trd_while_blk(while,Condition,Ss),Env,NewEnv):-
    eval_bool(Condition,Env,true),
    eval_stats(Ss, Env, Env1),
    
    eval_while_loop(trd_while_blk(while,Condition,Ss),Env1,NewEnv).
eval_while_loop(trd_while_blk(while,Condition,_),Env,Env):-
    eval_bool(Condition,Env,false).

eval_increment_operation(incr_op(I,++),Env,NewEnv) :- lookup(I,Env,V), K is V+1, 
    update(I,K,Env,NewEnv).

eval_increment_operation(incr_op(I,--),Env,NewEnv) :- lookup(I,Env,V), K is V-1, 
    update(I,K,Env,NewEnv).

%traditional_forloop(trd_for_blk(for,I,V,Condition,I,Op,Ss)) --> [for],['('],identifier(I),[=],value(V),['||'],condition(Condition),['||'],increment_operation(Op),[')'],['('],statements(Ss),[')'].

eval_traditional_forloop(trd_for_blk(for,I,V,Condition,I,Op,Ss),Env, FinEnv):- update(I,V,Env,Env1), eval_bool(Condition,Env1,true), 
    eval_stats(Ss, Env1, Env2),eval_increment_operation(Op,Env2,Env3), lookup(I,Env3,V1), eval_traditional_forloop(trd_for_blk(for,I,V1,Condition,I,Op,Ss),Env3, FinEnv).
eval_traditional_forloop(trd_for_blk(for,I,V,Condition,I,_,_),Env, Env):- update(I,V,Env,Env1), eval_bool(Condition,Env1,false).

%loop(lp(S)) --> range_forloop(S).

eval_loop(lp(S),Env,NewEnv):- eval_traditional_forloop(S,Env,NewEnv).
eval_loop(lp(S),Env,NewEnv):- eval_while_loop(S,Env,NewEnv).
eval_loop(lp(S),Env,NewEnv):- eval_range_for(S,Env,NewEnv).

%loops(lps(S,Ss)) --> loop(S), loops(Ss).
%loops(L) --> loop(L).
eval_loops(S,Env,NewEnv):- eval_loop(S,Env,NewEnv).
eval_loops(lps(S,Ss),Env,NewEnv):- eval_loop(S,Env,Env1), eval_loops(Ss,Env1,NewEnv).

%conditional_block(cond_blk(S)) --> if_then_else_block(S).
%if_then_else_block(if_then_else_blk(Condition,S1,S2)) --> [if],['('],condition(Condition),[')'],[then],['('],statements(S1),[')'],[else],['('],statements(S2),[')'].
eval_if_then_else_block(if_then_else_blk(Condition,S1,_),Env,NewEnv):-
    eval_bool(Condition,Env,true), eval_stats(S1, Env, NewEnv).
eval_if_then_else_block(if_then_else_blk(Condition,_,S2),Env,NewEnv):-
    eval_bool(Condition,Env,false), eval_stats(S2, Env, NewEnv).

%conditional_block(cond_blk(S)) --> if_then_block(S).
%if_then_block(if_then_blk(Condition,S)) --> [if],['('],condition(Condition),[')'],[then],['('],statements(S),[')'].
eval_if_then_block(if_then_blk(Condition,S),Env,NewEnv):-
    eval_bool(Condition,Env,true), eval_stats(S, Env, NewEnv).
eval_if_then_block(if_then_blk(Condition,_),Env,Env):-
    eval_bool(Condition,Env,false).

eval_conditional_block(cond_blk(S),Env,NewEnv):- eval_if_then_block(S, Env, NewEnv).
eval_conditional_block(cond_blk(S),Env,NewEnv):- eval_if_then_else_block(S, Env, NewEnv).
eval_conditional_block(cond_blk(S),Env,NewEnv):- eval_tern_operator(S, Env, NewEnv).

eval_block(blk(aarambh, D, '||', S, antah),Env,FinEnv):- eval_decls(D, Env, Env1), eval_stats(S, Env1, FinEnv).

eval_program(prgm(B),Env,FinEnv):- eval_block(B,Env,FinEnv).

%evaluator for ternary operator conditional block:
eval_tern_operator(tern_op_blk(Condition,?,S1,:,_),Env,FinEnv):-
    eval_bool(Condition,Env,true),eval_stats(S1,Env,FinEnv).
eval_tern_operator(tern_op_blk(Condition,?,_,:,S2),Env,FinEnv):-
    eval_bool(Condition,Env,false),eval_stats(S2,Env,FinEnv).

eval_print_statement_word(print_stmt_Word(X),Env):- mem((X,_),Env),lookup(X,Env,V),write(V).
eval_print_statement_word(print_stmt_Word(X),Env):- \+ mem((X,_),Env),write(X). 
eval_print_statement(print_stmt(W),Env):- eval_print_statement_word(W,Env).
eval_print_statement(print_stmt(W),Env):- evaluator(W, Env, Ans), write(Ans),nl.


%Update value in Environment 
update(Id, Val, [], [(Id,Val)]). 
update(Id, Val, [(Id,_)|T], [(Id,Val)|T]). 
update(Id, Val, [H|T], [H|R]) :- H\=(Id,_),update(Id,Val,T,R). 
%Lookup value in environment 
lookup(_,[],_). 
lookup(Id,[(Id,Val)|_],Val). 
lookup(Id,[_|T],Val):- lookup(Id,T,Val). 
