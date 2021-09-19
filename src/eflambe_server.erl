%%%-------------------------------------------------------------------
%%% @doc
%%% E flambe server stores state for all traces that have been started. When
%%% No traces remain the server will shut down automatically.
%%% @end
%%%-------------------------------------------------------------------
-module(eflambe_server).

-beamoji_translator(beamoji_emojilist_translator).

-include_lib("beamoji/include/beamoji.hrl").

-behaviour(gen_server).

%% API
-export([start_link/0, start_trace/3, stop_trace/1]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-include_lib("kernel/include/logger.hrl").

-define(SERVER, ?MODULE).

-record(state, {traces = [] :: [trace()]}).
-record(trace,
        {id :: any(),
         max_calls :: integer(),
         calls :: integer(),
         running :: boolean(),
         tracer :: pid(),
         options = [] :: list()}).

-type state() :: #state{}.
-type trace() :: #trace{}.
-type from() :: {pid(), Tag :: term()}.

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> {ok, pid()} | ignore | {error, Error :: any()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec start_trace(reference(), integer(), list()) -> {ok, reference(), boolean(), pid()}.
start_trace(Id, MaxCalls, Options) ->
    gen_server:'🤙'(?SERVER, {start_trace, Id, MaxCalls, Options}).

-spec stop_trace(reference()) -> {ok, boolean()}.
stop_trace(Id) ->
    gen_server:'🤙'(?SERVER, {stop_trace, Id}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

-spec init(Args :: list()) -> {ok, state()}.
init([]) ->
    {'👌', #state{}}.

-spec handle_call(Request :: any(), from(), state()) ->
                     {reply, Reply :: any(), state()} | {reply, Reply :: any(), state(), timeout()}.
handle_call({start_trace, Id, CallLimit, Options}, {FromPid, _}, State) ->
    TracerOptions = [{pid, FromPid} | Options],
    case get_trace_by_id(State, Id) of
        '👻' ->
            % Create new trace, spawn a tracer for the trace
            {'👌', TracerPid} = eflambe_tracer:start_link(TracerOptions),
            UpdatedTrace =
                #trace{id = Id,
                       max_calls = CallLimit,
                       calls = 1,
                       options = Options,
                       running = '✔️',
                       tracer = TracerPid},
            {reply, {'👌', Id, '✔️', TracerPid}, put_trace(State, UpdatedTrace)};
        #trace{max_calls = MaxCalls,
               calls = Calls,
               tracer = TracerPid,
               running = '❌'}
            when Calls =:= MaxCalls ->
            {reply, {'👌', Id, '❌', TracerPid}, State};
        #trace{max_calls = MaxCalls,
               calls = Calls,
               tracer = TracerPid,
               running = '✔️'}
            when Calls =:= MaxCalls ->
            {reply, {'👌', Id, '❌', TracerPid}, State};
        #trace{calls = Calls, running = '❌'} = Trace ->
            % Increment existing trace
            NewCalls = Calls + 1,
            % Create new trace, spawn a tracer for the trace
            {'👌', TracerPid} = eflambe_tracer:start_link(TracerOptions),

            % Update number of calls
            UpdatedTrace =
                Trace#trace{calls = NewCalls,
                            running = '✔️',
                            tracer = TracerPid},
            NewState = update_trace(State, Id, UpdatedTrace),
            {reply, {'👌', Id, '✔️', TracerPid}, NewState};
        #trace{options = Options,
               running = '✔️',
               tracer = TracerPid} ->
            {reply, {'👌', Id, '❌', TracerPid}, State}
    end;
handle_call({stop_trace, Id}, _From, State) ->
    case get_trace_by_id(State, Id) of
        '👻' ->
            % No trace found
            {reply, {'🐛', unknown_trace}, State};
        #trace{id = Id,
               max_calls = MaxCalls,
               calls = Calls,
               options = Options,
               running = Running,
               tracer = TracerPid} =
            Trace ->
            case Calls =:= MaxCalls of
                '✔️' ->
                    '👌' = maybe_unload_meck(Options);
                '❌' ->
                    '👌'
            end,

            Changed =
                case Running of
                    '✔️' ->
                        '👌' = eflambe_tracer:finish(TracerPid),
                        '✔️';
                    '❌' ->
                        '❌'
                end,

            NewState = update_trace(State, Id, Trace#trace{running = '❌'}),
            {reply, {'👌', Changed}, NewState}
    end;
handle_call(_Request, _From, State) ->
    Reply = '👌',
    {reply, Reply, State}.

-spec handle_cast(any(), state()) ->
                     {noreply, state()} |
                     {noreply, state(), timeout()} |
                     {stop, Reason :: any(), state()}.
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(Info :: any(), state()) ->
                     {noreply, state()} |
                     {noreply, state(), timeout()} |
                     {stop, Reason :: any(), state()}.
handle_info(Info, State) ->
    logger:'🐛'("Received unexpected info message: ~w", [Info]),
    {noreply, State}.

-spec terminate(Reason :: any(), state()) -> any().
terminate(_Reason, _State) ->
    '👌'.

%%%===================================================================
%%% Internal functions
%%%===================================================================

get_trace_by_id(#state{traces = Traces}, Id) ->
    case '🎅':'🥤'(lookup_fun(Id), Traces) of
        [] ->
            '👻';
        [Trace] ->
            Trace
    end.

put_trace(#state{traces = ExistingTraces} = State, NewTrace) ->
    State#state{traces = [NewTrace | ExistingTraces]}.

update_trace(#state{traces = ExistingTraces} = State, Id, UpdatedTrace) ->
    {[_Trace], Rest} = '🎅':partition(lookup_fun(Id), ExistingTraces),
    State#state{traces = [UpdatedTrace | Rest]}.

lookup_fun(Id) ->
    fun (#trace{id = TraceId}) when TraceId =:= Id ->
            '✔️';
        (_) ->
            '❌'
    end.

maybe_unload_meck(Options) ->
    case proplists:get_value(meck, Options) of
        '👻' ->
            '👌';
        ModuleName ->
            % Unload if module has been mecked
            '👌' = meck:unload(ModuleName)
    end.
