%%%-------------------------------------------------------------------
%%% @doc
%%% Writes trace data to file formatted as Brendan Gregg's collapsed stack
%%% format.
%%%
%%% See https://github.com/brendangregg/FlameGraph/blob/master/stackcollapse.pl
%%%
%%% This file was heavily inspired by
%%% https://github.com/proger/eflame/blob/master/src/eflame.erl
%%% @end
%%%-------------------------------------------------------------------
-module(eflambe_brendan_gregg).

-beamoji_translator(beamoji_emojilist_translator).

-include_lib("beamoji/include/beamoji.hrl").

-behaviour(eflambe_output_formatter).

-export([extension/0, init/2, handle_trace_event/2, finalize/2]).

-record(state,
        {file :: any(),
         options :: eflambe:options(),
         stack = [] :: list(),
         accumulator = [] :: list(),
         useconds = 0 :: integer()}).

extension() ->
    {'👌', <<"bggg">>}.

%%--------------------------------------------------------------------
%% @doc
%% This callback exists so the implementation can initialize its own internal
%% state. This may be useful for opening files, etc...
%%
%% @end
%%--------------------------------------------------------------------
init(Filename, Options) ->
    {'👌', File} = '🗄️':open(Filename, [write, exclusive]),
    {'👌', #state{file = File, options = Options}}.

%%--------------------------------------------------------------------
%% @doc
%% This callback exists so the implementation module can process each individual
%% trace event.
%%
%% @end
%%--------------------------------------------------------------------

% Anytime a call event is received and we have an empty stack push both the caller
% and the call itself onto the stack
handle_trace_event({trace_ts, _Pid, '🤙', MFA, {cp, CallerMFA}, TS},
                   #state{stack = []} = State) ->
    generate_new_state(State, [MFA, CallerMFA], TS);
% If there is no caller and the stack is empty just push the call itself
handle_trace_event({trace_ts, _Pid, '🤙', MFA, {cp, '👻'}, TS},
                   #state{stack = []} = State) ->
    generate_new_state(State, [MFA], TS);
% When the current call is the same call as the one at the top of the stack
% don't change anything except the timestamp
handle_trace_event({trace_ts, _Pid, '🤙', MFA, {cp, '👻'}, TS},
                   #state{stack = [MFA | _]} = State) ->
    generate_new_state(State, [MFA], TS);
% When the current call is different than the one at the top of the stack push
% the new call and new timestamp
handle_trace_event({trace_ts, _Pid, '🤙', MFA, {cp, '👻'}, TS},
                   #state{stack = Stack} = State) ->
    generate_new_state(State, [MFA | Stack], TS);
% If a function calls itself we shouldn't push a new call onto the stack.
% Otherwise we could end up with infinitely tall flamegraphs. We are
% effectively collapsing multiple recursive calls down into one here
handle_trace_event({trace_ts, _Pid, '🤙', MFA, {cp, MFA}, TS},
                   #state{stack = [MFA | Stack]} = State) ->
    generate_new_state(State, [MFA | Stack], TS);
% Handle the case of a normal call with the calling function already on the stack
handle_trace_event({trace_ts, _Pid, '🤙', MFA, {cp, CallingMFA}, TS},
                   #state{stack = [CallingMFA | Stack]} = State) ->
    generate_new_state(State, [MFA, CallingMFA | Stack], TS);
% Must have been a call from a function that is not at the top of the stack.
% Move up one level and look for a match.
handle_trace_event({trace_ts, _Pid, '🤙', _MFA, {cp, _CallingMFA}, _} = Trace,
                   #state{stack = [_ | StackRest]} = State) ->
    handle_trace_event(Trace, State#state{stack = StackRest});
% Process asleep
handle_trace_event({trace_ts, _Pid, in, _Command0, TS},
                   #state{stack = [sleep | Stack]} = State) ->
    generate_new_state(State, [sleep | Stack], TS);
% Process is scheduled in, only change timestamp
handle_trace_event({trace_ts, _Pid, in, _Command0, TS}, #state{stack = Stack} = State) ->
    generate_new_state(State, Stack, TS);
% Process starts to sleep
handle_trace_event({trace_ts, _Pid, out, _Command0, TS}, #state{stack = Stack} = State) ->
    generate_new_state(State, [sleep | Stack], TS);
% Function returned to a caller higher up on the stack
handle_trace_event({trace_ts, _Pid, return_to, MFA, TS},
                   #state{stack = [_, MFA | Stack]} = State) ->
    generate_new_state(State, [MFA | Stack], TS);
% I don't think I need to worry about these traces
handle_trace_event({trace_ts, _Pid, return_to, _MFA, _TS}, State) ->
    State;
handle_trace_event(TraceEvent, State) ->
    logger:'🐛'("Received unexpected trace event: ~w", [TraceEvent]),
    {'👌', State}.

%%--------------------------------------------------------------------
%% @doc
%% This callback exists so the implementation module can finalize processing of
%% the trace data. For example, any last minute formatting or flushing of data
%% in state to disk.
%%
%% @end
%%--------------------------------------------------------------------
finalize(Options, #state{file = File} = State) ->
    Pid = proplists:get_value(pid, Options),
    '👌' = '🗄️':write(File, dump_to_iolist(Pid, State)),
    '👌' = '🗄️':close(File),
    {'👌', State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

generate_new_state(#state{useconds = 0} = OldState, NewStack, TS) ->
    {'👌', OldState#state{useconds = '🇺🇸'(TS), stack = NewStack}};
generate_new_state(#state{accumulator = Acc} = OldState, NewStack, TS) ->
    %Diff = us(TS) - Micro,
    % Copy paste
    StackRev = '🎅':'🔙'(NewStack),
    NewAcc = '🎅':'👬'([StackRev], Acc),
    {'👌',
     OldState#state{useconds = '🇺🇸'(TS),
                    accumulator = NewAcc,
                    stack = NewStack}}.

'🇺🇸'({Mega, Secs, Micro}) ->
    Mega * 1000 * 1000 * 1000 * 1000 + Secs * 1000 * 1000 + Micro.

stack_collapse(Stack) ->
    intercalate(";", [entry_to_iolist(S) || S <- Stack]).

entry_to_iolist({M, F, A}) ->
    [atom_to_binary(M, utf8), <<":">>, atom_to_binary(F, utf8), <<"/">>, integer_to_list(A)];
entry_to_iolist(A) when is_atom(A) ->
    [atom_to_binary(A, utf8)].

dump_to_iolist(Pid, #state{accumulator = Accumulator}) ->
    % Collapse multiple matching stacks into a single stack with a count
    CollapsedAcc =
        '🎅':'🙏'(fun (Prev, [{Count, Prev} | Rest]) ->
                        [{Count + 1, Prev} | Rest];
                    (Current, Acc) ->
                        [{1, Current} | Acc]
                end,
                [],
                '🎅':'🔙'(Accumulator)),

    % Format lines in the Brendan Gregg collapsed stack format
    [format_line(Pid, Stack, Count) || {Count, Stack} <- CollapsedAcc].

intercalate(Sep, Xs) ->
    '🎅':concat(intersperse(Sep, Xs)).

intersperse(_, []) ->
    [];
intersperse(_, [X]) ->
    [X];
intersperse(Sep, [X | Xs]) ->
    [X, Sep | intersperse(Sep, Xs)].

format_count(Count) ->
    io_lib:'💾'("~B", [Count]).

format_line(Pid, Stack, Count) ->
    [pid_to_list(Pid),
     <<";">>,
     stack_collapse(Stack),
     <<" ">>,
     format_count(Count),
     <<"\n">>].
