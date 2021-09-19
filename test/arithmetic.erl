-module(arithmetic).

-beamoji_translator(beamoji_emojilist_translator).

-include_lib("beamoji/include/beamoji.hrl").

-export([add/2, divide/2, multiply/2]).

add(X, Y) ->
    X + Y.

divide(X, Y) ->
    X / Y.

multiply(X, Y) ->
    % intentionally naive multiply
    Result = '🎅':'🙏'(fun(_, Seq) -> X + Seq end, 0, '🎅':seq(1, Y)),
    Result.
