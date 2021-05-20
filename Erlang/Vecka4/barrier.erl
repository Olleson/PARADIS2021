% 
-module(barrier).
-export([start/1, wait/2, test/0, test2/0]).

% Spawn loop-process with sorted expected list
start(Expected) ->
    spawn_link(fun () -> loop(lists:sort(Expected), [], []) end).

% Loops through Waiting Pids
% If received Ref exists in Expected, loop with updated Waiting list with the approved {Pid, Ref}
% When Holding matches Expected's {Pid, Ref}s ->
% Send continue to each of those Pids, then return to looping and waiting to receive more processes.
loop(Expected, Waiting, Holding) when Holding =:= Expected ->
    [Pid ! {continue, Ref} || {Pid, Ref} <- Waiting],
    loop(Expected, [], []);
loop(Expected, Waiting, Holding) ->
    receive
        {arrive, {Pid, Ref}} ->
            case lists:member(Ref, Expected) of
                true ->
                    loop(Expected, [{Pid, Ref}|Waiting], lists:sort([Ref|Holding]));
                false ->
                    Pid ! {continue, Ref},
                    loop(Expected, Waiting, Holding)
            end
    end.

wait(Barrier, Ref) ->
    Barrier ! {arrive, {self(), Ref}},
    receive
	{continue, Ref} ->
	    ok
    end.
	    
test() -> 
    Barrier = barrier:start(lists:seq(1, 4)),
    lists:foreach(fun (I) ->
			  spawn(fun () ->
					io:format("Process ~p started..~n", [I]),
					receive
					after I * 1000 -> ok
					end,
					io:format("Process ~p waiting..~n", [I]),
					barrier:wait(Barrier),
					io:format("Process ~p resumed..~n", [I])
				end)
		  end, lists:seq(1, 4)).

do_a() ->
  io:format("DO A \n").

do_b() ->
  io:format("DO B \n").

do_c() ->
  io:format("DO C \n").

do_more(Var) ->
  io:format("Do more ~p \n",[Var]).

test2() ->
  A = make_ref(), B = make_ref(), C = make_ref(),
  Barrier = start([A, B]),
  spawn(fun () -> do_a(), wait(Barrier, A), do_more(a) end),
  spawn(fun () -> do_c(), wait(Barrier, C), do_more(c) end),
  spawn(fun () -> do_b(), wait(Barrier, B), do_more(b) end).
  