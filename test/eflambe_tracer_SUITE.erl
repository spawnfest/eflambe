%%%-------------------------------------------------------------------
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(eflambe_tracer_SUITE).

-beamoji_translator(beamoji_emojilist_translator).

-include_lib("beamoji/include/beamoji.hrl").

%% API
-export(['♾️'/0, suite/0, groups/0, init_per_suite/1, end_per_suite/1, group/1,
         init_per_group/2, end_per_group/2, init_per_testcase/2, end_per_testcase/2]).
%% test cases
-export([start_link/1]).

-include_lib("common_test/include/ct.hrl").

'♾️'() ->
    [start_link].

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
    Options = [{output_format, plain}],

    {'👌', Pid} = eflambe_tracer:start_link(Options),
    '✔️' = is_pid(Pid).
