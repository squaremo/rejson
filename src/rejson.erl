-module(rejson).

-export([parse/1, match/2]).

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

match(Pattern, Json) ->
    match(Pattern, Json, []).

%% Ground types
match(Pattern, Json, Bindings) ->
    case trivial_match(Pattern, Json) of
        ok -> {ok, Bindings};
        no -> match1(Pattern, Json, Bindings)
    end.

trivial_match(discard, _Json)             -> ok;
trivial_match(string, String) when
      is_list(String)                     -> ok;
trivial_match(number, Number) when
      is_number(Number)                   -> ok;
trivial_match(boolean, Bool) when
      Bool =:= true orelse Bool =:= false -> ok;
trivial_match(true, true)                 -> ok;
trivial_match({value, Value}, Value)      -> ok;
trivial_match(_, _)                       -> no.

match1({array, Array}, Json, Bindings) when is_list(Json) ->
    match_sequence(Array, Json, Bindings);
match1(_ , _, _) ->
    no_match.

match_sequence([], [], Bindings) ->
    {ok, Bindings};
match_sequence([P | PT], [J | JT], Bindings) ->
    case trivial_match(P, J) of
        ok -> match1({array, PT}, JT, Bindings);
        no -> no_match
    end.

-ifdef(TEST).

parse_test_() ->
    [{B, ?_assertEqual({ok, A}, parse(B))} ||
        {A, B} <-
            [
             %% Ground values
             { discard, "_" },
             { {value, 1}, "1" },
             { {value, ""}, "\"\"" },
             { {value, "foo"}, "\"foo\"" },
             { {value, [$\"]}, [$\", $\\, $\", $\"] },
             { {value, [$\", $\"]}, [$\", $\\, $\", $\\, $\", $\"] },
             { {value, 1.5}, "1.5" }, %% careful ..

             %% Compund values
             { {array, []}, "[]" },
             { {array, [{value, 1}]}, "[1]" },
             { {array, [{value, 2}, {value, 3}]}, "[2, 3]" },
             { {array, [{value, 5}, {value, "bar"}]}, "[5, \"bar\"]" },
             { {object, []}, "{}" },
             { {object, [{"foo", {value, "bar"}}]}, "{ \"foo\" : \"bar\" }" },
             { {object, [{"foo", discard}, discard]}, "{\"foo\": _, _}" },

             %% Ground types
             { number, "number" },
             { string, "string" },
             { boolean, "boolean"},
             { {array, [number, {value, 6}]}, "[number, 6]" },
             { {object, [{"baz", string}]}, "{\"baz\" : string}" },

             %% Repeats and interleave
             { {array, [{star, number}]}, "[number *]" },
             { {array, [{value, 10}, {maybe, string}]}, "[10, string ?]"},
             { {interleave,
                {array, [{value, 1}, {value, 2}]},
                {array, [{value, 3}, {value, 4}]}}, "[1, 2] ^ [3, 4]" },

             { {object, [{"foo", {maybe, string}}]},
                        "{\"foo\": string ?}" },

             %% Simple variable capture
             { {capture, "Foo", discard}, "Foo" },
             { {capture, "Foo", {value, 1}}, "Foo = 1"},
             { {capture, "Foo", {value, "foo"}}, "Foo = \"foo\"" }
            ]].

value_match_test_() ->
    [{A, ?_test(case parse(A) of
                     {ok, P} -> ?assertMatch({ok, []}, match(P, B))
                 end)} ||
        {A, B} <-
            [
             {"_", 103},
             {"number", 46.5},
             {"string", "foo"},
             {"boolean", true},
             {"boolean", false},
             {"1", 1},
             {"\"foobar\"", "foobar"},
             {"true", true},
             {"false", false}
    ]].

array_match_test_() ->
    [{A, ?_test(case parse(A) of
                    {ok, P} -> ?assertMatch({ok, []}, match(P, B))
                end)} ||
        {A, B} <-
            [
             {"[]", []},
             {"[1]", [1]},
             {"[\"foo\", number]", ["foo", 4.7]},
             {"[_, _]", [4, "bar"]}
            ]].

-endif.
