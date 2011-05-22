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

%% Match a newly-parsed pattern to a term, all or nothing.
match(Pattern, Json) ->
    derive(Pattern, Json).

%% Start a derivation with fresh stacks. Every time we try a match, we
%% push onto the continuation stack. If the match is successful, we
%% pop and continue. Every time we may need to backtrack, we push onto
%% the backtrack stack; if we come to a dead-end, we pop and continue.
derive(P, J) ->
    derive(P, J, [], []).

%% TODO empty object
%% Either
derive({either, P1, P2}, Json, Ks, Backtrack) ->
    derive(P1, Json, Ks, [{P2, Json, Ks} | Backtrack]);
derive({array, Seq}, Json, Ks, Backtrack) when is_list(Json) ->
    derive_seq(Seq, Json, Ks, Backtrack);
derive({array, _}, _, _, Backtrack) ->
    fail(Backtrack);
derive({interleave, Seq1, Seq2}, Json, Ks, Backtrack) when
      is_list(Json) ->
    derive_interleave(Seq1, Seq2, Json, Ks, Backtrack);
derive({interleave, _, _}, _, _, Backtrack) ->
    fail(Backtrack);
derive({capture, _V, P}, Json, Ks, Backtrack) ->
    %% TODO
    derive(P, Json, Ks, Backtrack);
derive(Pattern, Json, Ks, Backtrack) ->
    derive_simple(Pattern, Json, Ks, Backtrack).

derive_seq(Pattern, [], Ks, Backtrack) ->
    succeed({array, Pattern}, Ks, Backtrack);
%% Unmatched values left over
derive_seq([], _Json, _, Backtrack) ->
    fail(Backtrack);
derive_seq([{maybe, P} | Rest], Json, Ks, Backtrack) ->
    %% shortcut the either
    derive_seq(Rest, Json, Ks,
               [{{array, [P | Rest]}, Json, Ks} | Backtrack]);
derive_seq([{star, P} | Rest], Json, Ks, Backtrack) ->
    derive_seq(Rest, Json, Ks,
               [{{array, [P, {star, P} | Rest]}, Json, Ks} | Backtrack]);
derive_seq([{plus, P} | Rest], Json, Ks, Backtrack) ->
    derive_seq([P, {star, P} | Rest], Json, Ks, Backtrack);
derive_seq([P | PRest], [J | JRest], Ks, Backtrack) ->
    %% FIXME make this 'push', otherwise simplify (e.g., don't bother
    %% pushing if a simple pattern)
    derive(P, J, [{{rest, {array, PRest}, JRest}, Backtrack} | Ks], Backtrack).

%% Interleave:
%% a ^ b / s |- (a / s) ^ b | a ^ (b / s)

%% TODO redundant?
derive_interleave(Seq1, Seq2, [], Ks, Backtrack) ->
    case nullable(Seq1) andalso nullable(Seq2) of
        true ->
            succeed({interleave, Seq1, Seq2}, Ks, Backtrack);
        false ->
            fail(Backtrack)
    end;
derive_interleave({array, []}, Seq2, Json, Ks, Backtrack) ->
    derive(Seq2, Json, Ks, Backtrack);
derive_interleave(Seq1, {array, []}, Json, Ks, Backtrack) ->
    derive(Seq1, Json, Ks, Backtrack);
derive_interleave(Seq1, Seq2, [H | T], Ks, Backtrack) ->
    %% ^ is commutative
    Fail = {Seq2, [H], [{{interleave, Seq1, T}, Backtrack} | Ks]},
    Succeed = {{interleave, Seq2, T}, [Fail | Backtrack]},
    derive(Seq1, [H], [Succeed | Ks], [Fail | Backtrack]).

%% 'empty' signifies that we have fully matched a value; this is the
%% case if either we have matched a ground term or discarded a value,
%% or we have matched a sequence and have a nullable pattern.

%% We have two kinds of continuation. For {nested, ...}, we are
%% expecting to have matched an element of a compound value and can
%% continue with the other elements. For anything else, we are
%% expecting to have some collection of values left to match.

%% ***TODO Use [..] for array patterns, {[]} for object patterns,
%% and explicit end_array/end_object (or empty) continuations.

%% Nothing left to match, return.
succeed(Pattern, [], Backtrack) ->
    case nullable(Pattern) of
        true ->  {ok, []};
        false -> fail(Backtrack)
    end;
%% Partially matched an array value
succeed(Remainder,
        [{{rest, Pattern, Json}, Backtrack} | Ks], Backtrack) ->
    %% shortcut; really should be nullable_seq specifically
    io:format("Remainder : ~p, Outer: ~p, JSON: ~p~n", [Remainder, Pattern, Json]),
    case nullable(Remainder) of
        true  -> derive(Pattern, Json, Ks, Backtrack);
        false -> fail(Backtrack)
    end;
%% Partially matched an array pattern, keep going ...
succeed(empty, [{{array, Rest}, Json} | Ks], Backtrack) ->
    derive_seq(Rest, Json, Ks, Backtrack);
%% Partially matched an interleaved pattern, keep going ..
succeed(LHS, [{{interleave, RHS, Json}, Backtrack} | Ks], _) ->
    derive_interleave(LHS, RHS, Json, Ks, Backtrack).

fail([]) ->
    no_match;
fail([{Pattern, Json, Ks} | Backtrack]) ->
    derive(Pattern, Json, Ks, Backtrack).

-define(MATCH, succeed(empty, Ks, Backtrack)).

derive_simple(string, String, Ks, Backtrack) when
      is_list(String) ->
    ?MATCH;
derive_simple(number, Number, Ks, Backtrack) when
      is_number(Number) ->
    ?MATCH;
derive_simple(boolean, Bool, Ks, Backtrack) when
      Bool =:= true orelse Bool =:= false ->
    ?MATCH;
derive_simple(true, true, Ks, Backtrack) ->
    ?MATCH;
derive_simple({value, Value}, Value, Ks, Backtrack) ->
    ?MATCH;
derive_simple(discard, _Json, Ks, Backtrack) ->
    ?MATCH;
derive_simple(_, _, _, Backtrack) ->
    fail(Backtrack).

nullable(empty) ->
    true;
nullable({array, Seq}) ->
    nullable_seq(Seq);
nullable({interleave, Seq1, Seq2}) ->
    nullable_seq(Seq1) andalso nullable_seq(Seq2);
%% TODO should this come up?
nullable({either, Left, Right}) ->
    nullable(Left) orelse nullable(Right);
nullable(_) ->
    false.

nullable_seq([]) ->
    true;
nullable_seq([{star, _} | T]) ->
    nullable_seq(T);
nullable_seq([{maybe, _} | T]) ->
    nullable_seq(T);
nullable_seq({either, P1, P2}) ->
    nullable_seq(P1) orelse nullable_seq(P2);
nullable_seq(_) ->
    false.

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

             %% Compound values
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

             %% Alternation
             { {either, number, string}, "number | string" },
             { {either, {value, "foo"}, {array, []}}, "\"foo\" | []" },
             { {either, {value, 38}, {object, []}}, " 38 | {}" },

             %% Repeats and interleave
             { {array, [{star, number}]}, "[number *]" },
             { {array, [{value, 10}, {maybe, string}]}, "[10, string ?]"},
             { {array, [{value, 1},
                        {array, [{value, 2}]}, {value, 3}]}, "[1, [2], 3]" },
             { {interleave,
                {array, [{value, 1}, {value, 2}]},
                {array, [{value, 3}, {value, 4}]}}, "[1, 2] ^ [3, 4]" },
             { {interleave,
                {array, [{value, 1}, {value, 2}]},
                {interleave,
                 {array, [{value, 3}, {value, 4}]},
                 {array, [{value, 5}, {value, 6}]}}}, "[1, 2] ^ [3, 4] ^ [5, 6]" },

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
             {"false", false},
             {"number | string", 34},
             {"number | string", "foo"},
             {"1 | 2 | 3", 3}
    ]].

array_match_test_() ->
    [{A, ?_test(case parse(A) of
                    {ok, P} -> ?assertMatch({ok, []}, match(P, B))
                end)} ||
        {A, B} <-
            [
             { "[]", [] },
             { "[1]", [1] },
             { "[\"foo\", number]", ["foo", 4.7] },
             { "[_, _]", [4, "bar"] },

             { "[number *]", [1, 2, 3] },
             { "[number *]", [] },
             { "[number +]", [1, 2, 3] },
             { "[number +]", [1] },
             { "[number ?]", [] },
             { "[number ?]", [1] },
             { "[1, 2, 3 ?]", [1, 2, 3] },
             { "[1, 2, 3 ?]", [1, 2] },
             { "[1, number *, 3]", [1, 3] },
             { "[1, number *, 3]", [1, 2, 2, 3] },

             { "[1, [2, 3], 4]", [1, [2, 3], 4] },
             { "[number +, 3, number *]", [1,2,3,4] }
            ]].

interleave_match_test_() ->
    [{A, ?_test(case parse(A) of
                    {ok, P} -> ?assertMatch({ok, []}, match(P, B))
                end)} ||
        {A, B} <-
            [
             { "[1] ^ [2]", [2, 1] },
             { "[1, 2] ^ [3, 4]", [1, 3, 2, 4]},
             { "[1, 2] ^ [3, 4] ^ [5, 6]", [1, 2, 3, 4, 5, 6] },
             { "[1, 2] ^ [3, 4] ^ [5, 6]", [3, 5, 1, 2, 6, 4] }
             ]].

capture_notestyet_() ->
    [{A, ?_test(case parse(A) of
                    {ok, P} ->
                        {ok, Bindings0} = match(P, B),
                        Bindings = lists:sort(proplists:compact(Bindings0)),
                        Expected = lists:sort(proplists:compact(C)),
                        ?assertEqual(C, Bindings)
                end)} ||
        {A, B, C} <-
            [
             { "Foo = 1", 1, [{"Foo", 1}] }
            ]].

nomatch_test_() ->
    [{A, ?_test(case parse(A) of
                    {ok, P} ->
                        ?assertMatch(no_match, match(P, B))
                end)} ||
        {A, B} <-
            [
             { "1", 2 },
             { "2", "foo" },
             { "string", 1 },
             { "number", "foo" },
             { "boolean", 56 },
             { "true", false }
             %% TODO array v string
            ]].

-endif.
