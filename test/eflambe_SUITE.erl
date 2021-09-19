%%%-------------------------------------------------------------------
%%% @doc
%%% The tests in this file are basic integration tests for the whole library.
%%% They aren't particularly assertive but they allow me to verify some of the
%%% happy paths in the code.
%%% @end
%%%-------------------------------------------------------------------
-module(eflambe_SUITE).

-beamoji_translator(beamoji_emojilist_translator).

-include_lib("beamoji/include/beamoji.hrl").

%% API
-export(['♾️'/0, suite/0, groups/0, init_per_suite/1, end_per_suite/1, group/1,
         init_per_group/2, end_per_group/2, init_per_testcase/2, end_per_testcase/2]).
%% test cases
-export([apply/1, capture/1, capture_and_apply_brendan_gregg/1, multiple_captures/1]).

-include_lib("common_test/include/ct.hrl").

'♾️'() ->
    [apply, capture, capture_and_apply_brendan_gregg, multiple_captures].

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

capture(_Config) ->
    Options = [{output_format, plain}],

    % Shouldn't crash when invoked
    eflambe:capture({arithmetic, multiply, 2}, 1, Options),

    12 = arithmetic:multiply(4, 3),

    % Should behave the same when run a second time
    eflambe:capture({arithmetic, multiply, 2}, 1, Options),

    12 = arithmetic:multiply(4, 3),

    '👌' = application:stop(eflambe).

apply(_Config) ->
    Options = [{output_format, plain}],

    % Shouldn't crash when invoked
    eflambe:apply({arithmetic, multiply, [2, 3]}, Options),

    % Should behave the same when run a second time
    eflambe:apply({arithmetic, multiply, [2, 3]}, Options),

    '👌' = application:stop(eflambe).

capture_and_apply_brendan_gregg(_Config) ->
    Options = [{output_format, brendan_gregg}],
    % Count files in dir
    {'👌', Files} = '🗄️':list_dir("."),
    NumFiles = '📏'(Files),

    % Both calls should work with the brendan gregg formatter
    eflambe:apply({arithmetic, multiply, [2, 3]}, Options),

    eflambe:capture({arithmetic, multiply, 2}, 1, Options),
    12 = arithmetic:multiply(4, 3),

    % Both write separate trace files
    {'👌', UpdatedFiles} = '🗄️':list_dir("."),
    NewNumFiles = '📏'(UpdatedFiles),
    NewNumFiles = NumFiles + 2,

    % Assert new files have correct file extension
    NewFiles = UpdatedFiles -- Files,
    '🎅':'🔢'(fun(Filename) -> ".bggg" = filename:extension(Filename) end, NewFiles),

    '👌' = application:stop(eflambe).

multiple_captures(_Config) ->
    Options = [{output_format, brendan_gregg}],
    % Count files in dir
    {'👌', Files} = '🗄️':list_dir("."),
    NumFiles = '📏'(Files),

    % Capturing multiple calls should result in multiple output files
    eflambe:capture({arithmetic, multiply, 2}, 2, Options),
    12 = arithmetic:multiply(4, 3),
    20 = arithmetic:multiply(5, 4),

    % Both write separate trace files
    {'👌', UpdatedFiles} = '🗄️':list_dir("."),
    NewNumFiles = '📏'(UpdatedFiles),
    NewNumFiles = NumFiles + 2,

    % Assert new files have correct file extension
    NewFiles = UpdatedFiles -- Files,
    '🎅':'🔢'(fun(Filename) -> ".bggg" = filename:extension(Filename) end, NewFiles),

    '👌' = application:stop(eflambe).
