% vim: syntax=prolog:
act(A, K) :- 
 \+ gameStarted,
 assert(gameStarted),
 assert(myPos(((1,1), east))),
 assert(oldActions([])),
 (worldSize(X,Y) ; worldSize((X,Y))),
 assert(worldSize((X,Y))),
 assert(visited([])),
 assert(safeSpots([(1,1)])),
 act(A, K).

act(Action, K) :-
  gameStarted, 
  saveKnowledge(OldKnowledge),
  updateVisited(OldKnowledge, Knowledge),
  doAction(Action, Knowledge, Kp),
  updateMyPos(Action, Kp, K).

updateVisited(Ok, K) :-
  exists(myPos((V,_)), Ok),
  exists(visited(Vs), Ok),
  addIfNotMember(V, Vs, Vsp),
  my_retract(visited(_), Ok, Kp),
  my_assert(visited(Vsp), Kp, K).

addIfNotMember(A, B, C) :-
  member(A,B), !,
  C = B.

addIfNotMember(A,B,C) :-
  C = [A | B].

updateMyPos(turnLeft, Ok, K):-
  exists(myPos((V, D)), Ok),
  my_retract(myPos(_), Ok, Kp),
  turnDirLeft(D, Dp),
  my_assert(myPos((V,Dp)), Kp, K).

updateMyPos(turnRight, Ok, K):-
  exists(myPos((V, D)), Ok),
  my_retract(myPos(_), Ok, Kp),
  turnDirRight(D, Dp),
  my_assert(myPos((V,Dp)), Kp, K).

updateMyPos(moveForward, Ok, K):-
  exists(myPos((V, D)), Ok),
  my_retract(myPos(_), Ok, Kp),
  forwardVecPos(Ok, Vp),
  addVec(V, Vp, Vpp),
  my_assert(myPos((Vpp,D)), Kp, K).

updateMyPos(_, K, K).

forwardVecPos(Ok, V) :-
  exists(myPos((_, D)), Ok),
  forwardVec(D, V).

forwardVec(east, (1,0)).
forwardVec(south, (0,-1)).
forwardVec(north, (0, 1)).
forwardVec(west, (-1, 0)).

turnDirLeft(east, north).
turnDirLeft(south, east).
turnDirLeft(north, west).
turnDirLeft(west, south).

turnDirRight(east, south).
turnDirRight(north, east).
turnDirRight(south, west).
turnDirRight(west, north).

addVec((Ax, Ay), (Bx, By), (Cx, Cy)) :-
  Cx is Ax + Bx,
  Cy is Ay + By.

subVec((Ax, Ay), (Bx, By), (Cx, Cy)) :-
  Cx is Ax - Bx,
  Cy is Ay - By.

valuableKnowledge([oldActions, goVia, myPos, safeSpots, worldSize, visited]).

saveKnowledge(K) :-
  saveOldActions(X, [gameStarted]),
  saveGoBack(E, X),
  saveSafeSpots(W,E),
  saveMyPos(R, W),
  saveVisited(T, R),
  saveWorldSize(K, T).

saveVisited(K, A) :-
  visited(X), !,
  K = [visited(X) | A].

saveWorldSize(K, A) :-
  worldSize(X), !,
  K = [worldSize(X) | A].

saveOldActions(K, A) :-
  oldActions(X), !,
  K = [oldActions(X) | A].

saveGoBack(K, A) :-
  goVia(X), !,
  K = [goVia(X) | A ].

saveGoBack(K, K).

saveMyPos(K, A) :-
  myPos(X), !,
  K = [myPos(X) | A ].

saveSafeSpots(K, A) :-
  safeSpots(X), !,
  K = [safeSpots(X) | A ].

saveSafeSpots(K, K).

exists(A, B) :-
  member(A,B),
  !.

my_retract(A, B, C) :-
  my_delete(A, B, C).

my_delete(A, [], []).
my_delete(A, [H|T], Tp) :-
  \+ \+ A = H, !,
  my_delete(A, T, Tp).
my_delete(A, [H|T], [H|Tp]) :-
  my_delete(A, T, Tp).

my_assert(A, B, C) :-
  C = [A | B].

saveAction(A, Ok, K) :-
  (
    ( exists(oldActions(X), Ok), my_retract(oldActions(_), Ok, Kp))
    ; X = [], Kp = Ok),
  my_assert(oldActions([A|X]), Kp, K).

retraceAction(A, Ok, K) :-
  ( exists(oldActions([A|T]), Ok),
    my_retract(oldActions(_), Ok, Kp),
    my_assert(oldActions(T), Kp, K)
  )
; ( A = exit, Ok = K).

runAway(Ok, K) :-
  findPathTo((1,1), Ok, W),
  join(W, [exit], Wp),
  my_assert(goVia(Wp), Ok, K).

doAction(A, Ok, Kn) :-
  exists(goVia([A|T]), Ok),
  my_retract(goVia(_), Ok, Kp),
  ((T = [], !, K = Kp)
  ; my_assert(goVia(T), Kp, K)),
  updateIfSafe(K, Kn).

doAction(A, Ok, K) :-
  glitter, !,
  A = grab,
  runAway(Ok, K).

doAction(A, Ok, Kn) :-
  ((bump, retraceAction(_, Ok, Kpp), updateBumpedPos(Kpp, Kp))
  ;(breeze, Kp = Ok)
  ;(stench, Kp = Ok)), !,
  findNewDirection(Kp, K),
  doAction(A, K, Kn).

doAction(A, Ok, Kn) :-
  updateIfSafe(Ok,Kp),
  findNewDirection(Kp, K),
  doAction(A, K, Kn).

findNewDirection(Kp, K) :-
  ( goToUnexploredNode(Kp, K), ! )
  ; 
  ( runAway(Kp, K), !).

goToUnexploredNode(Kp, K) :-
  exists(safeSpots(SS), Kp),
  exists(visited(V), Kp),
  member(X, SS),
  \+ member(X, V),
  findPathTo(X, Kp, P), !,
  my_assert(goVia(P), Kp, K).

updateIfSafe(Ok, K) :-
  \+ breeze,
  \+ stench,
  !,
  updateSafeSpots(Ok,K).

updateIfSafe(K,K).

goForward(moveForward).

updateBumpedPos(Ok, K) :-
  forwardVecPos(Ok, V),
  exists(myPos((Vp, Dir)), Ok),
  subVec(Vp, V, Vpp),
  my_retract(myPos(_), Ok, Kp),
  my_assert(myPos((Vpp, Dir)), Kp, K).

updateSafeSpots(Ok, K) :-
  ((exists(safeSpots(L), Ok), my_retract(safeSpots(_), Ok, Kp)) 
  ; (L = [], Kp = Ok)),
  exists(myPos((P, _)), Kp),
  neighbours(P, Kp, N),
  join(N,L, Lp),
  my_assert(safeSpots(Lp), Kp, K).

join([], K, K).
join([H|T], L, [H|K]) :- join(T, L, K).

neighbours((X,Y), Ok, L) :-
  NY is Y + 1,
  SY is Y - 1,
  EX is X + 1,
  WX is X - 1,
  exists(worldSize((WoX, WoY)), Ok),
  addIfH(WX, 0, (WX,Y), [], LW),
  addIfH(SY, 0, (X,SY), LW, LS),
  addIfLE(NY, WoY, (X, NY), LS, LN),
  addIfLE(EX, WoX, (EX, Y), LN, L).

addIfLE(A,B,C,D,E):-
  A =< B, !,
  E = [ C | D].
addIfLE(A,B,C,D,D).

addIfH(A,B,C,D,E):-
  A > B, !,
  E = [ C | D].
addIfH(A,B,C,D,D).

reverseAction(moveForward, moveForward).
reverseAction(turnLeft, turnRight).
reverseAction(turnRight, turnLeft).
reverseAction(exit, exit).

findPathTo(PosB, Ok, Way) :-
  exists(safeSpots(SS), Ok),
  exists(myPos((V,Dir)), Ok),
  findWayTo(V, PosB, SS, W, Ok),
  wayToPath(W, Dir, Way).

wayToPath([H], _, []).
wayToPath([H1,H2|T], Dir, W) :- 
  subVec(H2, H1, Diff),
  diffToAct(Diff, Dir, Acts, Dirp),
  join(Acts, Wp, W),
  wayToPath([H2|T], Dirp, Wp).

diffToAct((-1, 0), east,  [ turnLeft, turnLeft, moveForward ], west).
diffToAct(( 0,-1), east,  [ turnRight, moveForward ], south).
diffToAct(( 0, 1), east,  [ turnLeft, moveForward ], north).
diffToAct(( 1, 0), east,  [ moveForward ], east).
diffToAct((-1, 0), west,  [ moveForward ], west).
diffToAct(( 0,-1), west,  [ turnLeft, moveForward ], south).
diffToAct(( 0, 1), west,  [ turnRight, moveForward ], north).
diffToAct(( 1, 0), west,  [ turnLeft, turnLeft, moveForward ], east).
diffToAct((-1, 0), north, [ turnLeft, moveForward ], north).
diffToAct(( 0,-1), north, [ turnLeft, turnLeft, moveForward ], south).
diffToAct(( 0, 1), north, [ moveForward ], north).
diffToAct(( 1, 0), north, [ turnRight, moveForward ], east).
diffToAct((-1, 0), south, [ turnRight, moveForward ], west).
diffToAct(( 0,-1), south, [ moveForward], south).
diffToAct(( 0, 1), south, [ turnLeft, turnLeft, moveForward], north).
diffToAct(( 1, 0), south, [ turnLeft, moveForward], east).

findWayTo(PosA, PosB, SafeSpots, Way, Ok) :-
  bfs([[PosA]], PosB, SafeSpots, [], Way, Ok).

bfs([[H|T]|Ws], H, _, _, Way, Ok) :- 
  reverse([H|T], Way).
bfs([[H|T]| AltWays], PosB, SS, Visited, Way, Ok) :-
  member(H, Visited), !,
  bfs(AltWays, PosB, SS, Visited, Way, Ok).
bfs([[H|T]| AltWays], PosB, SS, Visited, Way, Ok) :-
  neighbours(H, Ok, N),
  filterSafeN(N, SS, NSS),
  makeNewWays(NSS, [H|T], NW),
  Vp = [H | Visited],
  join(AltWays, NW, AltWaysEx),
  bfs(AltWaysEx, PosB, SS, Vp, Way, Ok).

makeNewWays([], _, []).
makeNewWays([H|T], W, [[H|W]| Wp]) :- 
  makeNewWays(T, W, Wp).

filterSafeN([], _, []).
filterSafeN([H|T], SS, [H|Tp]) :-
  member(H, SS), !,
  filterSafeN(T, SS, Tp).
filterSafeN([_|T], SS, Tp) :-
  filterSafeN(T, SS, Tp).
