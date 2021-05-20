% Supervisor implementation instead of monitor
-module(monitor).
-export([start/0]).
-behavior(supervisor).

start() -> supervisor:start_link(?MODULE, []).

init(_) ->
    SupFlags = #{strategy   => one_for_one,
                 intensity  => 5,
                 period     => 5},
    ChildSpec = [#{id       => double_id,
                   start    => {double, start, []}}],
    {ok, {SupFlags, ChildSpec}}.