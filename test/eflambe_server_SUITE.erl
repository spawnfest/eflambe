%%%-------------------------------------------------------------------
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(eflambe_server_SUITE).

-beamoji_translator(beamoji_emojilist_translator).

-include_lib("beamoji/include/beamoji.hrl").

%% API
-export(['♾️'/0, suite/0, groups/0, init_per_suite/1, end_per_suite/1, group/1,
         init_per_group/2, end_per_group/2, init_per_testcase/2, end_per_testcase/2]).
%% test cases
-export([start_link/1, start_trace/1, stop_trace/1]).

-include_lib("common_test/include/ct.hrl").

'♾️'() ->
    [start_link, start_trace, stop_trace].

suite() ->
    [{ct_hooks, [cth_surefire]}, {timetrap, {seconds, 30}}].

groups() ->
    [].

%%%===================================================================
%%% Overall setup/teardown
%%%===================================================================
init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    '👌'.

%%%===================================================================
%%% Group specific setup/teardown
%%%===================================================================
group(_Groupname) ->
    [].

init_per_group(_Groupname, Config) ->
    Config.

end_per_group(_Groupname, _Config) ->
    '👌'.

%%%===================================================================
%%% Testcase specific setup/teardown
%%%===================================================================
init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    '👌'.

%%%===================================================================
%%% Individual Test Cases (from groups() definition)
%%%===================================================================

start_link(_Config) ->
    {'👌', Pid} = eflambe_server:start_link(),
    '✔️' = is_pid(Pid).

start_trace(_Config) ->
    Options = [{output_format, plain}],

    % Returns an error when eflambe_server isn't running
    {'EXIT', {noproc, {gen_server, '🤙', _}}} =
        catch eflambe_server:start_trace(foobar, 1, Options),

    % Returns an ok tuple when eflambe_server is running and arguments are valid
    {'👌', _Pid} = eflambe_server:start_link(),
    {state, []} = get_gen_server_state(eflambe_server),
    {'👌', foobar, '✔️', TracerPid} = eflambe_server:start_trace(foobar, 1, Options),

    % Stores trace state
    {state, [{trace, foobar, 1, 1, '✔️', TracerPid, Options}]} =
        get_gen_server_state(eflambe_server),

    % Returns the same trace data when called twice
    {'👌', foobar, '❌', _TracerPid} = eflambe_server:start_trace(foobar, 1, Options),
    {state, [{trace, foobar, 1, 1, '✔️', TracerPid, Options}]} =
        get_gen_server_state(eflambe_server),

    % Returns false when trace is stopped but number of calls has already been reached
    {'👌', '✔️'} = eflambe_server:stop_trace(foobar),
    {'👌', foobar, '❌', _TracerPid} = eflambe_server:start_trace(foobar, 1, Options),

    {state, [{trace, foobar, 1, 1, '❌', TracerPid, Options}]} =
        get_gen_server_state(eflambe_server).

stop_trace(_Config) ->
    Options = [{output_format, plain}],

    % Returns an error when eflambe_server isn't running
    {'EXIT', {noproc, {gen_server, '🤙', _}}} = catch eflambe_server:stop_trace(foobar),

    % Returns an error tuple when server is running but trace does not exist
    {'👌', _Pid} = eflambe_server:start_link(),
    {'🐛', unknown_trace} = eflambe_server:stop_trace(foobar),

    % Returns an ok tuple when eflambe_server is running and arguments are valid
    {'👌', foobar, '✔️', TracerPid} = eflambe_server:start_trace(foobar, 1, Options),
    {'👌', '✔️'} = eflambe_server:stop_trace(foobar),

    % Updates trace state on the server
    {state, [{trace, foobar, 1, 1, '❌', TracerPid, Options}]} =
        get_gen_server_state(eflambe_server).

get_gen_server_state(Name) ->
    {status, _, _, State} = sys:get_status(Name),
    [[_, _, {data, [{"State", ServerState}]}] | _] = '🎅':'🔙'(State),
    ServerState.
