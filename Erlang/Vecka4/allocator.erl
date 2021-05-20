% Allocates resources, give and is able to take back resources.
-module(allocator).
-export([start/1, request/2, release/2, test2/0, allocate_test/3]).

start(Resources) ->
    spawn_link(fun () -> allocator(Resources) end).

%% Pid = Allocator pid, Keys = Requested resources
%% Sends request message to Allocator with list of requested resources,
%% it then blocks until it gets a response from the Allocator.
%% If Allocator fails, it sends error and request retries.
request(Pid, Keys) ->
    Ref = make_ref(),
    Pid ! {request, {self(), Ref, Keys}},
    receive
	{granted, Ref, Granted} ->
	    Granted;
    {error, Ref} ->
        request(Pid, Keys)
    end.

%% Pid = Allocator pid, Released = Resources to give back
%% Sends release message to Allocator with list of resources to give back,
%% it then blocks until it gets a response from the Allocator.
release(Pid, Released) ->
    Ref = make_ref(),
    Pid ! {release, {self(), Ref, Released}},
    receive
	{released, Ref} ->
	    ok
    end.

%% Loops infinitely with Resources-state
%% Waits to receive messages from shell, if message is request, it checks if those resources
%% are available with check_resources/2. If it returns true, we send back to user with
%% a message and a map with the requested resources, it then removes those resources from the State.
%% If user wants to release, it combines the inputted resources with the existing state.
allocator(Resources) ->
    receive
        {request, {Pid, Ref, Keys}} ->
            case check_resources(Resources, Keys) of
                true -> 
                    Pid ! {granted, Ref, maps:with(Keys, Resources)},
                    allocator(maps:without(Keys, Resources));
                false ->
                    Pid ! {error, Ref},
                    allocator(Resources)
            end;
        {release, {Pid, Ref, Release}} ->
            Pid ! {released, Ref},
            allocator(maps:merge(Resources, Release))
    end.

%% Returns false if resource does not exist in the map
check_resources(_, []) -> true;
check_resources(Resources, [H|T]) ->
    case maps:is_key(H, Resources) of
        true -> check_resources(Resources, T);
        false -> false
    end.

test2() ->
    Allocator = allocator:start(#{a=>10, b=>20, c=>30}),
    spawn(?MODULE, allocate_test, [Allocator, "Process A", [a,b]]),
    spawn(?MODULE, allocate_test, [Allocator, "Process B", [a,b]]),
    spawn(?MODULE, allocate_test, [Allocator, "Process C", [c]]).

allocate_test(Allocator, Name, N) ->    
    io:format("~p requests ~p resources ~n", [Name, N]),
    S = allocator:request(Allocator, N),
    receive after 2000 -> ok end,
    io:format("~p releasing ~p~n", [Name, S]),
    allocator:release(Allocator, S),
    allocate_test(Allocator, Name, N).
