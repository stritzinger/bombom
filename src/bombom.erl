-module(bombom).

%% API exports
-export([main/1]).

-include_lib("rebar/src/rebar.hrl").

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main(Args) ->
    rebar3:run(rebar_state:new(config()), ["sbom" | Args]),
    erlang:halt(0).

%%====================================================================
%% Internal functions
%%====================================================================

config() ->
    [
        {project_plugins, [
            {rebar3_sbom, {git, "https://github.com/stritzinger/rebar3_sbom.git", {branch, "master"}}}
        ]}
    ].
