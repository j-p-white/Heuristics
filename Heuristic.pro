% Heuristicall Programming 
% this code will use huristics to solve prolog problems 
% Langauge: Prolog
% Author: Jamie White 


%--------------------------------------------------------
% initulize the rules 
  rule(1,situation1,action1).
  rule(2,situation2,action2).
  rule(3,situation3,action3).
  rule(4,situation4,action4).
  rule(5,situation5,action5).
  rule(6,situation6,action6).
  rule(7,situation7,action7).
  rule(8,situation8,action8).

  solveProblemHeuristically :- 
  	rule(Number,Situation,Action),
	write('considering rule'),write(Number),write('...'),nl,
	Situation, 
	write('application of rule'),write(Number),write('produces'),
	Action.
 solveProblemHeuristically.

%---------------------------------------------------------
 %Heuristic 1 

  situation1 :- 
    problem(Numbers,Goal),
    Goal = goal(0),
    Numbers = numbers(N1,N2,N3,N4,N5), 
    member(0,[N1,N2,N3,N4,N5]).
  action1 :-
  	problem(Numbers,_),
	Numbers = numbers(N1,N2,N3,N4,N5),
	assert(solution(ex(N1,*,ex(N2,*,ex(N3,*,ex(N4,*,N5)))))).
%---------------------------------------------------------
%Heuristic 2 

situation2 :-
  problem(numbers(N1,N2,N3,N4,N5),goal(G)),
  member(G,[N1,N2,N3,N4,N5]),
  member(0,[N1,N2,N3,N4,N5]),
  not(G=0).
action2 :-
  problem(_,goal(G)),
  other_numbers(special(G),others(A,B,C,D)),
  assert(solution(ex(G,+,ex(A,*,ex(B,*,ex(C,*,D)))))).


%----------------------------------------------------------
%Heuristic 3

situation3 :-
 problem(_,goal(0)),
 doubleton.
action3 :-
  doubleton(doubleton(A,B),rest(C,D,E)),
  assert(solution(ex(ex(A,-,B),*,ex(C,*,ex(D,*,E))))).

%----------------------------------------------------------
%Heuristic 4 

situation4 :- 
problem(numbers(N1,N2,N3,N4,N5),goal(G)), 
 member(G,[N1,N2,N3,N4,N5]),
 member(1,[N1,N2,N3,N4,N5]),
 member(0,[N1,N2,N3,N4,N5]),
 not(G=0).
action4 :- 

