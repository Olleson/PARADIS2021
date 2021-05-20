% A bank that can check balance, deposit money, withdraw money and lend money
-module(bank).
-export([start/0, balance/2, deposit/3, withdraw/3, lend/4]).

% Spawn init, 
% init is like a computer, anything it starts is a part of it. init-process receives are all the same (bank, check_balance, osv).
start() -> spawn(fun init/0).

% Starts bank-server
init() -> bank(#{}).

% Bank-server-loop, loops with User_map state
bank(User_map) ->
    receive 
        {From, User, Ref, Op} ->
            New_map = case Op of
                {balance} -> check_balance({From, User, Ref}, User_map);
                {deposit, X} -> update_deposit({From, User, Ref}, User_map, X);
                {withdraw, X} -> update_withdraw({From, User, Ref}, User_map, X);
                {lend, User1, User2, X} -> update_lend({From, User, Ref}, User_map, User1, User2, X)
            end,
        bank(New_map)
    end.

%%% Interface
balance(Pid, Who) ->
    Ref = erlang:monitor(process, Pid),     % Monitor each interaction with the server, if it fails, return no_bank
    Pid ! {self(), Who, Ref, {balance}},
    receive 
        {_, Ref, Message} -> 
            erlang:demonitor(Ref),
            Message;
        {'DOWN', Ref, process, Pid, _} -> 
            no_bank
    after 5000 ->
        timeout
    end.

deposit(Pid, Who, X) ->
    Ref = erlang:monitor(process, Pid),
    Pid ! {self(), Who, Ref, {deposit, X}},
    receive
        {_, Ref, Message} -> 
            erlang:demonitor(Ref),
            Message;
        {'DOWN', Ref, process, Pid, _} -> 
            no_bank
    after 5000 ->
        timeout
    end.

withdraw(Pid, Who, X) ->
    Ref = erlang:monitor(process, Pid),
    Pid ! {self(), Who, Ref, {withdraw, X}},
    receive
        {_, Ref, Message} -> 
            erlang:demonitor(Ref),
            Message;
        {'DOWN', Ref, process, Pid, _} -> 
            no_bank
    after 5000 ->
        timeout
    end.

lend(Pid, From, To, X) ->
    Ref = erlang:monitor(process, Pid),
    Pid ! {self(), From, Ref, {lend, From, To, X}},
    receive
        {_, Ref, Message} -> 
            erlang:demonitor(Ref),
            Message;
        {'DOWN', Ref, process, Pid, _} -> 
            no_bank
    after 5000 ->
        timeout
    end.

%%% Private
check_balance({From, User, Ref}, User_map) ->
    Balance = maps:get(User, User_map, no_account),
    case Balance of 
        no_account -> From ! {self(), Ref, Balance};
        _ -> From ! {self(), Ref, {ok, Balance}}
    end,
    User_map.

update_deposit({From, User, Ref}, User_map, X) ->
    Balance = maps:get(User, User_map, no_account),
    case Balance of
        no_account -> A = X;
        _ -> A = X + Balance
    end,
    From ! {self(), Ref, {ok, A}},
    User_map#{User => A}.

update_withdraw({From, User, Ref}, User_map, X) ->
    Balance = maps:get(User, User_map, no_account),
    case Balance of
        no_account ->
            From ! {self(), Ref, no_account},
            User_map;
        _ when X > Balance -> 
            From ! {self(), Ref, insufficient_funds},
            User_map;
        _ ->
            D = Balance - X,
            From ! {self(), Ref, {ok, D}},
            User_map#{User => D}
    end.

update_lend({From, _, Ref}, User_map, User1, User2, X) ->
    Bal1 = maps:get(User1, User_map, no_account),
    Bal2 = maps:get(User2, User_map, no_account),
    case {Bal1, Bal2} of 
        {no_account, no_account} -> 
            From ! {self(), Ref, {no_account, both}},
            User_map;
        {no_account, _} -> 
            From ! {self(), Ref, {no_account, User1}},
            User_map;
        {_, no_account} -> 
            From ! {self(), Ref, {no_account, User2}},
            User_map;
        {Bal1, Bal2} when Bal1 >= X ->
            From ! {self(), Ref, ok},
            User_map#{User1 => Bal1-X, User2 => Bal2+X};
        {Bal1, Bal2} ->
            From ! {self(), Ref, insufficient_funds},
            User_map
    end.