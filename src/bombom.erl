-module(bombom).

%% API exports
-export([main/1]).

-include_lib("rebar/src/rebar.hrl").

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main(Args) ->
    BaseState = init_config(),
    rebar3:run(BaseState, ["sbom"| Args]),
    erlang:halt(0).

%%====================================================================
%% Internal functions
%%====================================================================

rebar3_sbom_plugin() ->
    {rebar3_sbom, {git, "https://github.com/stritzinger/rebar3_sbom.git", {branch, "main"}}}.

init_config() ->
    rebar_utils:set_httpc_options(),
    %% Initialize logging system
    Verbosity = log_level(),
    ok = rebar_log:init(command_line, Verbosity),
    Config = rebar_config:consult_root(),

    MyPlugins = [rebar3_sbom_plugin()],
    %% Merge project_plugins with existing ones
    NewPlugins = case proplists:get_value(project_plugins, Config, []) of
        ExistingPlugins when is_list(ExistingPlugins) ->
            ExistingPlugins ++ [P || P <- MyPlugins, not lists:member(P, ExistingPlugins)];
        _ ->
            MyPlugins
    end,
    Config0 = lists:keystore(project_plugins, 1, Config, {project_plugins, NewPlugins}),

    Config1 = rebar_config:merge_locks(Config0, rebar_config:consult_lock_file(?LOCK_FILE)),
    InitState = rebar_state:new(Config1),
    %% If $HOME/.config/rebar3/rebar.config exists load and use as global config
    GlobalConfigFile = rebar_dir:global_config(InitState),
    State = case filelib:is_regular(GlobalConfigFile) of
                true ->
                    ?DEBUG("Load global config file ~ts", [GlobalConfigFile]),
                    try state_from_global_config(Config1, GlobalConfigFile)
                    catch
                        _:_ ->
                            ?WARN("Global config ~ts exists but can not be read. Ignoring global config values.", [GlobalConfigFile]),
                            InitState
                    end;
                false ->
                    InitState
            end,
    %% Determine the location of the rebar executable; important for pulling
    %% resources out of the escript
    State1 = try
                 ScriptName = filename:absname(escript:script_name()),
                 %% Running with 'erl -s rebar3 main' still sets a name for some reason
                 %% so verify it is a real file
                 case filelib:is_regular(ScriptName) of
                     true ->
                         rebar_state:escript_path(State, ScriptName);
                     false ->
                         State
                 end
             catch
                 _:_ ->
                     State
             end,
    %% TODO: Do we need this still? I think it may still be used.
    %% Initialize vsn cache
    rebar_state:set(State1, vsn_cache, dict:new()).

%% @doc get log level based on getopt options and ENV
-spec log_level() -> integer().
log_level() ->
    case os:getenv("QUIET") of
        Q when Q == false; Q == "" ->
            case os:getenv("DIAGNOSTIC") of
                Di when Di == false; Di == "" ->
                    case os:getenv("DEBUG") of
                        D when D == false; D == "" ->
                            try
                                {ok, L} = application:get_env(rebar, log_level),
                                 rebar_log:atom_to_level(L)
                            catch
                                _:_ -> rebar_log:default_level()
                            end;
                        _ ->
                            rebar_log:debug_level()
                    end;
                _ ->
                    rebar_log:diagnostic_level()
            end;
         _ ->
            rebar_log:error_level()
    end.

state_from_global_config(Config, GlobalConfigFile) ->
    GlobalConfigTerms = rebar_config:consult_file(GlobalConfigFile),
    GlobalConfigTmp = rebar_state:new(GlobalConfigTerms),

    GlobalConfig = case os:getenv("REBAR_CACHE_DIR") of
                false ->
                    GlobalConfigTmp;
                CachePath ->
                    rebar_state:set(GlobalConfigTmp, global_rebar_dir, CachePath)
            end,

    %% We don't want to worry about global plugin install state effecting later
    %% usage. So we throw away the global profile state used for plugin install.
    GlobalConfigThrowAway0 = rebar_state:current_profiles(GlobalConfig, [global]),

    Resources = application:get_env(rebar, resources, []),
    GlobalConfigThrowAway = rebar_state:create_resources(Resources, GlobalConfigThrowAway0),

    Compilers = application:get_env(rebar, compilers, []),
    GlobalConfigThrowAway1 = rebar_state:compilers(GlobalConfigThrowAway, Compilers),

    GlobalState = case rebar_state:get(GlobalConfigThrowAway1, plugins, []) of
                      [] ->
                          GlobalConfigThrowAway1;
                      GlobalPluginsToInstall ->
                          rebar_plugins:handle_plugins(global,
                                                       GlobalPluginsToInstall,
                                                       GlobalConfigThrowAway1)
                  end,
    GlobalPlugins = rebar_state:providers(GlobalState),
    GlobalConfig2 = rebar_state:set(GlobalConfig, plugins, []),
    GlobalConfig3 = rebar_state:set(GlobalConfig2, {plugins, global},
                                    rebar_state:get(GlobalConfigThrowAway1, plugins, [])),
    rebar_state:providers(rebar_state:new(GlobalConfig3, Config), GlobalPlugins).
