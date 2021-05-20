-module(mapreduce).
-export([mapreduce/5, mapreduce/6, test/0, test_distributed/0]).

test() ->
    Mapper = fun (_Key, Text) ->
		     [{Word, 1} || Word <- Text]
	     end, 
    Reducer = fun (Word, Counts) ->
		      [{Word, lists:sum(Counts)}]
	      end,
    mapreduce(Mapper, 2, Reducer, 10, [{a, ["hello", "world", "hello", "text"]}, {b, ["world", "a", "b", "text"]}]).

%%% Assumes you've loaded the file with l(File)
%%% To test: make N-amount of nodes, connect them and load mapreduce file in each node's shell. Then call test_distributed().
test_distributed() ->
    Mapper = fun (_Key, Text) ->
		     [{Word, 1} || Word <- Text]
	     end, 
    Reducer = fun (Word, Counts) ->
		      [{Word, lists:sum(Counts)}]
	      end,
    LocalHost = net_adm:localhost(),
    {ok, LocalHosts} = net_adm:names(),
    Nodes = [list_to_atom(N ++ "@" ++ LocalHost) || {N, _P} <- LocalHosts],
    mapreduce(Nodes, Mapper, 10, Reducer, 10, [{a, ["a", "b", "b", "c"]}, {b, ["c", "a", "b", "b"]}, {c, ["c", "c", "c", "c"]}, {d, ["d", "a", "a", "a"]}, {e, ["b", "a", "b", "c"]}]).

%%% One node
% Spawn reducers and mappers, after spawning, block until all mappers are complete
% Send to each reducer a task
% Collect result, then present it
mapreduce(Mapper, Mappers, Reducer, Reducers, Input) ->
    Master = self(), Ref = make_ref(),
    Partitions = partition(Mappers, Input),
    ReducerPids = [spawn_reducer(Master, Ref, Reducer) || _ <- lists:seq(1, Reducers)],         % Spawn Reducers amount Reducer
    MapperPids = [spawn_mapper(Master, Ref, Mapper, ReducerPids, Part) || Part <- Partitions],  % Spawn Mappers amount Mapper
    [receive 
        {mapped, {Pid, Ref}} -> 
            ok 
        end || Pid <- MapperPids],                                                              % Block until all mappers have sent mapped
    [R ! {reduce, Master, Ref} || R <- ReducerPids],                                            % Send to each reducer the request to start reducing
    Output = [receive
		  {reduced, {Pid, Ref, Data}} ->
		      Data
	      end || Pid <- ReducerPids],
    lists:sort(lists:flatten(Output)).

%%% Distributed among list of Nodes
mapreduce(Nodes, Mapper, Mappers, Reducer, Reducers, Input) ->
    Master = self(), Ref = make_ref(),
    Partitions = partition(Mappers, Input),
    ReducerPids = distribute_reducers(Nodes, Reducer, Reducers, [], {Master, Ref}),
    MapperPids = distribute_mappers(Nodes, Mapper, Partitions, ReducerPids, [], {Master, Ref}),
    [receive 
        {mapped, {Pid, Ref}} -> 
            ok 
        end || Pid <- MapperPids],
    [R ! {reduce, Master, Ref} || R <- ReducerPids],
    Output = [receive
		  {reduced, {Pid, Ref, Data}} ->
		      Data
	      end || Pid <- ReducerPids],
    lists:sort(lists:flatten(Output)).

%%% Recursively distribute between list of nodes until Reducers = 0
distribute_reducers(_, _, 0, ReducerPids, _) ->
    ReducerPids;
distribute_reducers([H|T], Reducer, Reducers, ReducerPids, {Master, Ref}) ->
    distribute_reducers(T ++ [H], Reducer, Reducers - 1, [spawn_reducer(H, Master, Ref, Reducer)|ReducerPids], {Master, Ref}).

%%% Recursively distribute between list of nodes until Mappers = 0
distribute_mappers(_, _, [], _, MapperPids, _) ->
    MapperPids;
distribute_mappers([H|T], Mapper, [Part|Rest], ReducerPids, MapperPids, {Master, Ref}) ->
    distribute_mappers(T ++ [H], Mapper, Rest, ReducerPids, [spawn_mapper(H, Master, Ref, Mapper, ReducerPids, Part)|MapperPids], {Master, Ref}).

spawn_mapper(Master, Ref, Mapper, ReducerPids, Data) ->
    spawn_link(fun () -> mapper(Master, Ref, Mapper, ReducerPids, Data) end).
spawn_mapper(Node, Master, Ref, Mapper, ReducerPids, Data) ->
    spawn_link(Node, fun () -> mapper(Master, Ref, Mapper, ReducerPids, Data) end).
mapper(Master, Ref, Mapper, Reducers, Data) ->
    % Phash can be a number from 1 to length of Reducers
    Map = [{erlang:phash(MapKey, length(Reducers)), {MapKey, MapValue}} ||
		                                            {DataKey, DataValue} <- Data,
		                                            {MapKey, MapValue} <- Mapper(DataKey, DataValue)],
    [lists:nth(Phash, Reducers) ! {map_data, Ref, Element} || {Phash, Element} <- Map],             % Send data directly to Map's tuple's reducer by lists:nth (defined by its Phash)
    Master ! {mapped, {self(), Ref}}.

spawn_reducer(Master, Ref, Reducer) ->
    spawn_link(fun () -> reducer(Master, Ref, Reducer, []) end).
spawn_reducer(Node, Master, Ref, Reducer) ->
    spawn_link(Node, fun () -> reducer(Master, Ref, Reducer, []) end).      
reducer(Master, Ref, Reducer, Data) ->
    receive
        {map_data, Ref, MapperData} ->
            reducer(Master, Ref, Reducer, [MapperData | Data]);     % Collect and append map_data-list (because multiple datas can have the same reducer)
        {reduce, Master, Ref} ->                                    % When given the command by master, will start reducing the content but blocks until then
            Reduced = [KV || {K,Vs} <- groupkeys(lists:sort(Data)), KV <- Reducer(K,Vs)],
		    Master ! {reduced, {self(), Ref, Reduced}}
    end.

%% INPUT:  [{K1, V1}, {K1, V2}, {K2, V3}]
%% OUTPUT: [{K1, [V1, V2]}, {K2, [V3]}]
groupkeys([]) ->
    [];
groupkeys([{K, V}|Rest]) ->
    groupkeys(K, [V], Rest).
groupkeys(K, Vs, [{K, V}|Rest]) ->
    groupkeys(K, [V|Vs], Rest);
groupkeys(K, Vs, Rest) -> 
    [{K, lists:reverse(Vs)}|groupkeys(Rest)].

%% INPUT: [a,b,c,d], 2
%% OUTPUT: [[a,b], [c,d]]
%% INPUT: [a, b], 2
%% OUTPUT: [[],[], [a], [b]]
partition(N, L) ->
    partition(N, L, length(L)).
partition(1, L, _) ->
    [L];
partition(N, L, Len) ->
    {Prefix, Suffix} = lists:split(Len div N, L),
    [Prefix | partition(N - 1, Suffix, Len - (Len div N))].