-module(task1).
-export([eval/1, eval/2, map/2, filter/2, split/2, groupby/2, reverse/1]).

% Uppgift 1
% Pattern match calculator, recursively checks each expression. 
% If expression is an atom or does not fit into any pattern, return error and stop recursion.
calc(E) when is_integer(E) -> E;
calc(E) when is_atom(E) -> error;
calc({'add', E1, E2}) -> calc(E1) + calc(E2);
calc({'sub', E1, E2}) -> calc(E1) - calc(E2);
calc({'mul', E1, E2}) -> calc(E1) * calc(E2); 
calc({'div', E1, E2}) -> calc(E1) / calc(E2);
calc({_}) -> error.

% Takes expression: {Atom, E1, E2}
% E1 and E2 can be more tuples or numbers
eval(E) -> 
    try calc(E) of
        error -> error;
        Sum -> {ok, Sum}
    catch
        _:_ -> error
    end.

% Uppgift 2
% Pattern match calculator, recursively checks each expression.
% If expression is an atom or does not fit into any pattern, return error and stop recursion.
calc(E, _) when is_integer(E) -> E;
calc(E, M) when is_atom(E) -> 
    {ok, F} = maps:find(E, M), F;
calc({'add', E1, E2}, M) -> calc(E1, M) + calc(E2, M);
calc({'sub', E1, E2}, M) -> calc(E1, M) - calc(E2, M);
calc({'mul', E1, E2}, M) -> calc(E1, M) * calc(E2, M); 
calc({'div', E1, E2}, M) -> calc(E1, M) / calc(E2, M);
calc({_}, _) -> error.

% Takes expression: {Atom, E1, E2}, Map
% E1 and E2 can be more tuples or atoms or numbers
eval(E, M) ->
    try calc(E, M) of
        error -> {error, unknown_error};
        Sum -> {ok, Sum}
    catch
        _:_ -> {error, variable_not_found}
    end.

% Uppgift 3
% F = Function, L = List
% Tail recursive map algorithm. Append new list with elements that has
% had function operated on it.
map(F, L) -> map(F, L, []).
map(_, [], Result) -> lists:reverse(Result);
map(Function, [H|T], Result) -> map(Function, T, [Function(H)|Result]).

% P = Function, L = List
% Tail recursive filter algorithm. Append new list with elements that
% returned true with the function.
filter(P, L) -> filter(P, L, []).
filter(_, [], Result) -> lists:reverse(Result);
filter(P, [H|T], Result) -> 
    case P(H) of
        true -> filter(P, T, [H|Result]);
        false -> filter(P, T, Result)
    end.

% P = Function, L = List
% Tail recursive split algorithm. Make two lists where elements that
% return true with the function gets placed in list A, and elements
% that return false gets placed in list B.
split(P, L) -> split(P, L, {[],[]}).
split(_, [], {A, B}) -> 
    {lists:reverse(A), lists:reverse(B)};
split(P, [H|T], {A, B}) ->
    case P(H) of
        true -> split(P, T, {[H|A], B});
        false -> split(P, T, {A, [H|B]})
    end.

% Identifies key and then checks if there is a value attached to it. If yes, update that key's value. Else, make new entry.
% Calls separate reverse function that reverses the individual lists
groupby(F, L) -> groupby(F, L, 1, #{}).
groupby(_, [], _, Result) -> 
    reverse(Result);
groupby(F, [H|T], Index, Result) ->
    Key = F(H), 
    V = maps:get(Key, Result, error),
    if V == error ->  
            groupby(F, T, Index+1, Result#{Key => [Index]});    % New key
       true ->  
            groupby(F, T, Index+1, Result#{Key => [Index|V]})   % Append to existing key's value
    end.

% Reverse each key's value's list
reverse(Map) -> 
    reverse(Map, maps:keys(Map)).
reverse([], Map) -> Map;
reverse([H|T], Map) ->
    V = maps:get(H, Map, error),
    reverse(T, Map#{H := lists:reverse(V)}).

% groupby(F, L) -> groupby(F, L, 1, #{}, v).
% groupby(_, [], _, Result, _) -> 
%     % reverse(Result);
%     Result;
% groupby(F, [H|T], Index, Result, Value) when Value == error ->
%     V = maps:get(F(H), Result, error),
%     groupby(F, T, Index+1, Result#{F(H) => [Index]}, V);
% groupby(F, [H|T], Index, Result, _) ->
%     V = maps:get(F(H), Result, error),
%     groupby(F, T, Index+1, Result#{F(H) => [Index|V]}, V).

% groupby som sparar values
% groupby(F, L) -> groupby(F, L, #{}).
% groupby(_, [], Result) -> Result;
% groupby(F, [H|T], Result) ->
%     Key = F(H), 
%     V = maps:get(Key, Result, error), 
%     if V == error ->  groupby(F, T, Result#{Key => [H]});
%        true ->  groupby(F, T, Result#{Key => [H|V]})
%     end.

% Exercise3
% filter(P, L) -> filter(P, L, []).
% filter(_, [], Result) -> Result;
% % filter(Function, [H|T], Result) when Function(H) -> filter(Function, T, [H|Result]);
% % filter(Function, [_|T], Result) -> filter(Function, T, [Result]).

% Exercise 1
% things to fix: 
% *make it return {ok, sum}
% *check if integers
% eval(E) when is_integer(E) ->
%     E;                      %%not working, 
% eval({Op, E1, E2}) when is_tuple(E1) -> 
%     eval({Op, eval(E1), E2});   %%check if E1 is a tuple
% eval({Op, E1, E2}) when is_tuple(E2) -> 
%     eval({Op, E1, eval(E2)});   %%check if E2 is a tuple
% eval({Op, E1, E2}) ->
%     Result = case Op of
%             'add' -> E1+E2;
%             'sub' -> E1-E2;
%             'mul' -> E1*E2;
%             'div' -> E1/E2;
%             _-> error
%         end,
%     Result.