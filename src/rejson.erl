-module(rejson).

-export([parse/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

parse(In) when is_list(In) ->
    case rejson_lexer:string(In) of
        {ok, Tokens, _Endline} ->
            rejson_parser:parse(Tokens);
        Err ->
            Err
    end.


-ifdef(TEST).

parse_test_() ->
    [?_assertEqual({ok, A}, rejson:parse(B)) ||
        {A, B} <-
            [
             %% Ground values
             { {value, 1}, "1" },
             { {value, ""}, "\"\"" },
             { {value, "foo"}, "\"foo\"" },
             { {value, 1.5}, "1.5" }, %% careful ..

             %% Compund values
             { {array, [{value, 1}]}, "[1]" },
             { {array, [{value, 2}, {value, 3}]}, "[2, 3]" },
             { {array, [{value, 5}, {value, "bar"}]}, "[5, \"bar\"]" },

             { discard, "_" },
             { {capture, "Foo", discard}, "Foo" },
             { {capture, "Foo", {value, 1}}, "Foo = 1"},
             { {capture, "Foo", {value, "foo"}}, "Foo = \"foo\"" }
            ]].

-endif.
