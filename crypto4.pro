%import the gv class 
:- consult('gv.pro').
:- consult('combosets.pro'). 
%----------------------------------------------------
%set up min max values for numbers 

minMaxCryptoNumbers :- 
      declare(lo,0),
      declare(hi,16). 

 
%make the random numbers 
randomNumber(R) :- 
    valueOf(lo,Lo), 
    valueOf(hi,Hi),
    random(Lo,Hi,R).

%make the problem 
generateRandomCryptoProblem :- 
     randomNumber(N1),
     randomNumber(N2),
     randomNumber(N3),
     randomNumber(N4),
     randomNumber(N5),
     randomNumber(G),
 addCryptoProblemToKnowleadgeBase(N1,N2,N3,N4,N5,G).


     %add numbers to data base 
     addCryptoProblemToKnowleadgeBase(N1,N2,N3,N4,N5,G):-
     retract(problem(_,_)), 
     assert(problem(number(N1,N2,N3,N4,N5),goal(G))),
     eraseOldSolutionsIfAny.	 

     %add problems to knowleadgebase 
     addCryptoProblemToKnowleadgeBase(N1,N2,N3,N4,N5,G) :-
        assert(problem(number(N1,N2,N3,N4,N5),goal(G))),
	eraseOldSolutionsIfAny.

eraseOldSolutionsIfAny :-
   retract(solution(_)).
eraseOldSolutionsIfAny.

%---------------------------------------------------------------

displayProblem :- 
         problem(number(N1,N2,N3,N4,N5),goal(G)), 
         write('problem: numbers = {'),
         write(N1), write(','),
         write(N2), write(','),
         write(N3), write(','),
         write(N4), write(','),
         write(N5), write('} the goal is: '),
         write(G), nl.

%--------------------------------------------------------
% initilize the rules 
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
%((4-4)*(5*(6*7)))
%--------------------------------------------------------------
%Heuristic 4 

situation4 :-
  problem(numbers(N1,N2,N3,N4,N5),goal(G)),
  member(G,[N1,N2,N3,N4,N5]),
  member(0,[N1,N2,N3,N4,N5]),
  same(G,5).
action4 :-
  problem(_,goal(G)),
  other_numbers(special(G),others(A,B,C,D)),
  assert(solution(ex(G,+,ex(A,*,ex(B,*,ex(C,*,D)))))).
%---------------------------------------------------------------
%Heuristic 5

situation5 :- 
  problem(numbers(N1,N2,N3,N4,N5),goal(G)), 
  member(0,[N1,N2,N3,N4,N5]),
  Numbers = [N1,N2,N3,N4,N5],
  not(N1=0),
  not(N2=0),
  Firstlast = [N1,N5],
  sum1(Firstlast,Result),
  same(Result,G).
action5 :-
	problem(numbers(N1,N2,N3,N4,N5),_),
  assert(solution(ex(ex(N1,+,N5),+,ex(N2,*,ex(N3,*,N4))))).
 
 % assert(solution(ex(ex(A,-,B),*,ex(C,*,ex(D,*,E))))).
%((4-4)*(5*(6*7)))
%---------------------------------------------------------------
%Heuristic 6

situation6 :- 
  problem(numbers(N1,N2,N3,N4,N5),goal(G)), 
  member(0,[N1,N2,N3,N4,N5]),
  Secondthird = [N2,N3],
  sum1(Secondthird,Result),
  same(Result,goal(G)).
action6 :-
	problem(number(N1,N2,N3,N4,N5),_),
  assert(solution(ex(ex(N2,+,N3),+,ex(N1,*,ex(N5,*,N4))))).
 
 % assert(solution(ex(ex(A,-,B),*,ex(C,*,ex(D,*,E))))).
%((4-4)*(5*(6*7)))
%-------------------------------------------------------------
%Heuristic 7

situation7 :- 
  problem(numbers(N1,N2,N3,N4,N5),goal(G)), 
  member(0,[N1,N2,N3,N4,N5]),
  Firstsecondthird = [N1,N2,N3],
  sum1(Firstsecondthird,Result),
  same(Result,goal(G)).
action7 :-
	problem(number(N1,N2,N3,N4,N5),_),
  assert(solution(ex(ex(N1,+,ex(N2,+,N3)),+,ex(N1,*,ex(N5,*,N4))))).
 
 % assert(solution(ex(ex(A,-,B),*,ex(C,*,ex(D,*,E))))).
%((4-4)*(5*(6*7)))
%-------------------------------------------------------------
%Heuristic 8

situation8 :- 
  problem(numbers(N1,N2,N3,N4,N5),goal(G)), 
  member(0,[N1,N2,N3,N4,N5]),
  All = [N1,N2,N3,N4,N5],
  sum1(All,Result),
  same(Result,goal(G)).
action8 :-
	problem(number(N1,N2,N3,N4,N5),_),
  assert(solution(ex(ex(N1,+,ex(N2,+,ex(N3,+,ex(N4,+,N5))))))).
 
 % assert(solution(ex(ex(A,-,B),*,ex(C,*,ex(D,*,E))))).
%((4-4)*(5*(6*7)))
%-------------------------------------------------------------

% display the solution --assuming that it has been solved

displaySolution :-
  solution(none),
  write('No solution to this one!'),nl.
displaySolution :-
  write('Solution: '),
  solution(S),
  displayResult(S),
  nl.

%---------------------------------------------------------------
%display the result -- assuming the problem the problem has been "solved"

displayResult(ex(A,O,B)) :-
  number(A),number(B), 
  write('( '),write(A),write(' '),write(O),write(' '),write(B),write(' )').
displayResult(ex(A,O,B)) :-
   number(A), B = ex(A1,O1,B1),
   write('( '),write(A),write(' '),write(O),write(' '),
   displayResult(ex(A1,O1,B1)),write(' )').
displayResult(ex(A,O,B)) :-
   number(B), A = ex(A1,O1,B1),
   write('( '),displayResult(ex(A1,O1,B1)),write(' '),write(O),write(' '),write(B),write(' )').
displayResult(ex(A,O,B)) :- 
  A = ex(A1,O1,B1),B = ex(A2,O2,B2),
  write('( '),displayResult(ex(A1,O1,B1)),write(' '),write(O),write(' '),displayResult(ex(A2,O2,B2)),write(' )').
%-------------------------------------------------------------------
%demo

demo :-
 generateRandomCryptoProblem,
 displayProblem,
 solveProblemHeuristically,
 displaySolution.

demo(0).
demo(N) :-
  demo,
  K is N-1,
  demo(K).

%-------------------------------------------------------------------

%crypto probelm solver for a specific problem 

solve(numbers(N1,N2,N3,N4,N5),goal(G)) :-
   establishCryptoProblem(numbers(N1,N2,N3,N4,N5),goal(G)),
   displayProblem, 
   solveProblemHeuristically,
   displaySolution.

 establishCryptoProblem(numbers(N1,N2,N3,N4,N5),goal(G)) :-
   addCryptoProblemToKnowleadgeBase(N1,N2,N3,N4,N5,G).

%-------------------------------------------------------------------
%initialization 

:- minMaxCryptoNumbers. 

%-------------------------------------------------------------------

%doubleton

doubleton :-
  problem(numbers(N1,N2,N3,N4,N5),_),
  List = [N1,N2,N3,N4,N5],
  check_pair(List,_).

%----------------------------------------------------------
%check_pair(index,list,output).

check_pair([H|T],H):-
  contains(H,T).
check_pair([_|T],NOut):-
  check_pair(T,NOut).

%----------------------------------------------------------
%contains

contains(X,[X|_]).
contains(X,[_|T]):-
  contains(X,T).

%----------------------------------------------------------
%search 

search(X,[X|_],X).
search(X,[_|T],A):-
  search(X,T,A).
%---------------------------------------------------------

%same 

same(E,E).

%----------------------------------------------------------

other_numbers(special(G),rest(N1,N2,N3,N4)):-
  problem(numbers(N1,N2,N3,N4,N5),goal(G)),
          same(G,N5).

other_numbers(special(G),rest(N2,N3,N4,N5)):-
  problem(numbers(N1,N2,N3,N4,N5),goal(G)),
  	same(G,N1).

other_numbers(special(G),rest(N1,N3,N4,N5)):-
  problem(numbers(N1,N2,N3,N4,N5),goal(G)),
  	same(G,N2).

other_numbers(special(G),rest(N1,N2,N4,N5)):-
  problem(numbers(N1,N2,N3,N4,N5),goal(G)),
  	same(G,N3).

other_numbers(special(G),rest(N1,N2,N3,N5)):-
  problem(numbers(N1,N2,N3,N4,N5),goal(G)),
  	same(G,N4).
%--------------------------------------------------------
%sum1 

 sum1([],0).
 sum1([H|T],Result):-
 	sum1(T,NewOut),
 	Result is H + NewOut.
