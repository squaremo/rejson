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
%% push onto the success continuation stack. Every time we may
%% backtrack, we push onto the failure continuation stack. If a match
%% is successful, we pop a success continuation; if we come to a
%% dead-end, we pop a failure continuation.
derive(P, J) ->
    derive(P, J, [], [], []).

derive({either, P1, P2}, Json, Ks, FKs, Bs) ->
    derive(P1, Json, Ks, [{P2, Json, Ks, Bs} | FKs], Bs);
derive(Seq, Json, Ks, FKs, Bs) when is_list(Seq) andalso is_list(Json) ->
    derive_seq(Seq, Json, Ks, FKs, Bs);
derive(Seq, _, _, FKs, _) when is_list(Seq) ->
    fail(FKs);
derive({Obj}, {Json}, Ks, FKs, Bs) when is_list(Obj) andalso is_list(Json) ->
    derive_object(Obj, Json, Ks, FKs, Bs);
derive({_}, _, _, FKs, _) ->
    fail(FKs);
derive({interleave, Seq1, Seq2}, Json, Ks, FKs, Bs) when
      is_list(Json) ->
    derive_interleave(Seq1, Seq2, Json, Ks, FKs, Bs);
derive({interleave, _, _}, _, _, FKs, _) ->
    fail(FKs);
derive({capture, V, P}, Json, Ks, FKs, Bs) ->
    derive_capture(V, P, Json, Ks, FKs, Bs);
derive(Pattern, Json, Ks, FKs, Bs) ->
    derive_simple(Pattern, Json, Ks, FKs, Bs).

derive_capture(V, P, J, Ks, FKs, Bs) ->
    derive(P, J, [{bind, V, J} | Ks], FKs, Bs).

derive_seq(Pattern, [], Ks, FKs, Bs) ->
    succeed(Pattern, Ks, FKs, Bs);
%% Unmatched values left over
derive_seq([], _, _, FKs, _) ->
    fail(FKs);
derive_seq([{maybe, P} | Rest], Json, Ks, FKs, Bs) ->
    %% shortcut the either
    derive_seq(Rest, Json, Ks,
               [{[P | Rest], Json, Ks, Bs} | FKs], Bs);
derive_seq([{star, P} | Rest], Json, Ks, FKs, Bs) ->
    derive_seq(Rest, Json, Ks,
               [{[P, {star, P} | Rest], Json, Ks, Bs} | FKs], Bs);
derive_seq([{plus, P} | Rest], Json, Ks, FKs, Bs) ->
    derive_seq([P, {star, P} | Rest], Json, Ks, FKs, Bs);
derive_seq([{capture, V, {R, P}} | Rest], Json, Ks, FKs, Bs)  when
      R =:= star orelse R =:= plus orelse R =:= maybe ->
    derive_seq([{R, {capture, V, P}} | Rest], Json,
               [{collect, V} | Ks], FKs, Bs);
derive_seq([P | PRest], [J | JRest], Ks, FKs, Bs) ->
    %% FIXME make this 'push', otherwise simplify (e.g., don't bother
    %% pushing if a simple pattern)
    derive(P, J, [{rest, PRest, JRest} | Ks], FKs, Bs).

%% Interleave:
%% a ^ b / s |- (a / s) ^ b | a ^ (b / s)

%% TODO redundant?
derive_interleave(Seq1, Seq2, [], Ks, FKs, Bs) ->
    %% TODO eliminate [] in seq1 or seq2
    succeed({interleave, Seq1, Seq2}, Ks, FKs, Bs);
derive_interleave([], Seq2, Json, Ks, FKs, Bs) ->
    derive(Seq2, Json, Ks, FKs, Bs);
derive_interleave(Seq1, [], Json, Ks, FKs, Bs) ->
    derive(Seq1, Json, Ks, FKs, Bs);
derive_interleave(Seq1, Seq2, [H | T], Ks, FKs, Bs) ->
    %% ^ is commutative
    Succeed = {interleave, Seq2, T},
    SucceedAfterFail = {interleave, Seq1, T},
    Fail = case Seq2 of
               [{capture, V1, {R1, P1}} | Seq2s] when
                     R1 =:= star orelse R1 =:= plus orelse R1 =:= maybe ->
                   {[{R1, {capture, V1, P1}} | Seq2s], [H],
                    [SucceedAfterFail, {collect, V1} | Ks], Bs};
               _ ->
                   {Seq2, [H], [SucceedAfterFail | Ks], Bs}
           end,
    case Seq1 of
        [{capture, V2, {R2, P2}} | Seq1s] when
              R2 =:= star orelse R2 =:= plus orelse R2 =:= maybe ->
            derive([{R2, {capture, V2, P2}} | Seq1s], [H],
                   [Succeed, {collect, V2} | Ks], [Fail | FKs], Bs);
        _ ->
            derive(Seq1, [H], [Succeed | Ks], [Fail | FKs], Bs)
    end.

derive_object(Obj, [], Ks, FKs, Bs) ->
    succeed({Obj}, Ks, FKs, Bs);
derive_object([], _, _, FKs, _) ->
    fail(FKs);
derive_object([discard], _, Ks, FKs, Bs) ->
    succeed(empty, Ks, FKs, Bs);
derive_object([{K, P} | Rest], Json, Ks, FKs, Bs) ->
    case P of
        %% 'maybe' here means the key can be absent, but if it is
        %% present, the value must match.
        {maybe, P1} ->
            derive_object([{K, P1} | Rest], Json,
                          Ks, [{{Rest}, {Json}, Ks, Bs} | FKs], Bs);
        P1 ->
            case proplists:get_value(K, Json) of
                undefined ->
                    fail(FKs);
                Val ->
                    derive(P1, Val,
                           [{rest, {Rest}, {proplists:delete(K, Json)}} | Ks],
                           FKs, Bs)
            end
    end.

%% 'empty' signifies that we have fully matched a value; this is the
%% case if either we have matched a ground term or discarded a value,
%% or we have matched a sequence and have a nullable pattern.

%% We have two kinds of continuation. For {rest, ...}, we are
%% expecting to have matched an element of a compound value and can
%% continue with the other elements. For anything else, we are
%% expecting to have some collection of values left to match.

%% Nothing left to match, return.
succeed(Pattern, [], FKs, Bs) ->
    case nullable(Pattern) of
        true ->  {ok, Bs};
        false -> fail(FKs)
    end;
%% Matched an element
succeed(Remainder,
        [{rest, Pattern, Json} | Ks], FKs, Bs) ->
    %% shortcut; really should be nullable_seq specifically
    case nullable(Remainder) of
        true  -> derive(Pattern, Json, Ks, FKs, Bs);
        false -> fail(FKs)
    end;
succeed(Remainder, [{bind, V, J} | Ks], FKs, Bs) ->
    succeed(Remainder, Ks, FKs, [{V, J} | Bs]);
succeed(Remainder, [{collect, V} | Ks], FKs, Bs) ->
    NewBs = [{V, lists:reverse(proplists:get_all_values(V, Bs))} |
             proplists:delete(V, Bs)],
    succeed(Remainder, Ks, FKs, NewBs);
%% Partially matched an array pattern, keep going ...
succeed(empty, [{array, Rest, Json} | Ks], FKs, Bs) ->
    derive_seq(Rest, Json, Ks, FKs, Bs);
%% Partially matched an interleaved pattern, keep going ..
succeed(LHS, [{interleave, RHS, Json} | Ks], FKs, Bs) ->
    derive_interleave(LHS, RHS, Json, Ks, FKs, Bs).

fail([]) ->
    no_match;
fail([{Pattern, Json, Ks, Bs} | FKs]) ->
    derive(Pattern, Json, Ks, FKs, Bs).

-define(MATCH, succeed(empty, Ks, FKs, Bs)).

derive_simple(string, String, Ks, FKs, Bs) when
      is_binary(String) ->
    ?MATCH;
derive_simple(number, Number, Ks, FKs, Bs) when
      is_number(Number) ->
    ?MATCH;
derive_simple(boolean, Bool, Ks, FKs, Bs) when
      Bool =:= true orelse Bool =:= false ->
    ?MATCH;
derive_simple(true, true, Ks, FKs, Bs) ->
    ?MATCH;
derive_simple(discard, _Json, Ks, FKs, Bs) ->
    ?MATCH;
derive_simple(Value, Value, Ks, FKs, Bs) ->
    ?MATCH;
derive_simple(_, _, _, FKs, _) ->
    fail(FKs).

nullable(empty) ->
    true;
nullable(Seq) when is_list(Seq) ->
    nullable_seq(Seq);
nullable({Obj}) when is_list(Obj) ->
    nullable_obj(Obj);
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

nullable_obj([]) ->
    true;
nullable_obj([{_, {maybe, _}} | Rest]) ->
    nullable_obj(Rest);
nullable_obj(_) ->
    false.

-ifdef(TEST).

parse_test_() ->
    [{B, ?_assertEqual({ok, A}, parse(B))} ||
        {A, B} <-
            [
             %% Ground values
             { discard, "_" },
             { 1, "1" },
             { <<"">>, "\"\"" },
             { <<"foo">>, "\"foo\"" },
             { <<$\">>, [$\", $\\, $\", $\"] },
             { <<$\", $\">>, [$\", $\\, $\", $\\, $\", $\"] },
             { 1.5, "1.5" }, %% careful ..

             %% Compound values
             { [], "[]" },
             { [1], "[1]" },
             { [2, 3], "[2, 3]" },
             { [5, <<"bar">>], "[5, \"bar\"]" },
             { {[]}, "{}" },
             { {[{"foo", <<"bar">>}]}, "{ \"foo\" : \"bar\" }" },
             { {[{"foo", discard}, discard]}, "{\"foo\": _, _}" },

             %% Ground types
             { number, "number" },
             { string, "string" },
             { boolean, "boolean"},
             { [number, 6], "[number, 6]" },
             { {[{"baz", string}]}, "{\"baz\" : string}" },

             %% Alternation
             { {either, number, string}, "number | string" },
             { {either, <<"foo">>, []}, "\"foo\" | []" },
             { {either, 38, {[]}}, " 38 | {}" },

             %% Repeats and interleave
             { [{star, number}], "[number *]" },
             { [10, {maybe, string}], "[10, string ?]"},
             { [1, [2], 3], "[1, [2], 3]" },
             { {interleave, [1, 2], [3, 4]}, "[1, 2] ^ [3, 4]" },
             { {interleave, [1, 2],
                {interleave, [3, 4], [5, 6]}}, "[1, 2] ^ [3, 4] ^ [5, 6]" },

             { {[{"foo", {maybe, string}}]}, "{\"foo\": string ?}" },

             %% Simple variable capture
             { {capture, "Foo", discard}, "Foo" },
             { {capture, "Foo", 1}, "Foo = 1"},
             { {capture, "Foo", <<"foo">>}, "Foo = \"foo\"" },
             { {capture, "Foo", {either, string, number}}, "Foo = string|number" },

             %% Parens and precedence
             { {either, {capture, "Foo", string}, number}, "(Foo = string)|number" },
             { {capture, "Foo", {either, string, number}}, "Foo = (string|number)" },
             { [{capture, "Foo", {star, number}}], "[Foo = number *]" }
            ]].

matches(Cases) ->
    [{A, ?_test(case parse(A) of
                    {ok, P} -> ?assertMatch({ok, []}, match(P, B))
                end)} ||
        {A, B} <- Cases].

value_match_test_() ->
    matches([
             {"_", 103},
             {"number", 46.5},
             {"string", <<"foo">>},
             {"boolean", true},
             {"boolean", false},
             {"1", 1},
             {"\"foobar\"", <<"foobar">>},
             {"true", true},
             {"false", false},
             {"number | string", 34},
             {"number | string", <<"foo">>},
             {"1 | 2 | 3", 3}
            ]).

array_match_test_() ->
    matches([
             { "[]", [] },
             { "[1]", [1] },
             { "[\"foo\", number]", [<<"foo">>, 4.7] },
             { "[_, _]", [4, <<"bar">>] },

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
            ]).

interleave_match_test_() ->
    matches([
             { "[1] ^ [2]", [2, 1] },
             { "[1, 2] ^ [3, 4]", [1, 3, 2, 4]},
             { "[1, 2] ^ [3, 4] ^ [5, 6]", [1, 2, 3, 4, 5, 6] },
             { "[1, 2] ^ [3, 4] ^ [5, 6]", [3, 5, 1, 2, 6, 4] },
             { "[number *] ^ [1, 2]", [0, 1, 0, 0, 2, 0] }
            ]).

object_match_test_() ->
    matches([
             { "{\"foo\": number}", {[{"foo", 1}]} },
             { "{\"foo\": number ?}", {[]} },
             { "{\"foo\": number ?, \"bar\": number}",
               {[{"bar", 2}]} },
             { "{\"foo\": number, _}",
               {[{"foo", 1}, {"bar", 2}]} },
             { "{_}", {[{"foo", 1}, {"bar", 5}]} },
             { "{\"foo\": 1, \"bar\": 2}",
               {[{"bar", 2}, {"foo", 1}]} }
            ]).

capture_test_() ->
    [{A, ?_test(case parse(A) of
                    {ok, P} ->
                        {ok, Bindings0} = match(P, B),
                        Bindings = lists:sort(proplists:compact(Bindings0)),
                        Expected = lists:sort(proplists:compact(C)),
                        ?assertEqual(Expected, Bindings)
                end)} ||
        {A, B, C} <-
            [
             { "Foo = 1", 1, [{"Foo", 1}] },
             { "Foo = string", <<"foo">>, [{"Foo", <<"foo">>}] },
             { "Foo = boolean", true, [{"Foo", true}] },
             { "Foo = string|number", 45, [{"Foo", 45}] },
             { "Foo = Bar = 4", 4, [{"Foo", 4}, {"Bar", 4}]},
             { "[Foo = _, Bar = _]", [4, 5], [{"Foo", 4}, {"Bar", 5}] },
             { "[Foo = number, _ *]", [3, 4, 5, 6], [{"Foo", 3}] },
             { "[number *, Foo = string, _ *]", [1, 2, <<"foo">>, 4, 5],
               [{"Foo", <<"foo">>}] },

             { "Foo = [_, _]", [1, 2], [{"Foo", [1, 2]}] },
             { "Foo = ([number, number] ^ [1, 2])", [1, 3, 4, 2],
               [{"Foo", [1, 3, 4, 2]}] },

             { "[Foo = number *, 4]", [1, 2, 3, 4], [{"Foo", [1, 2, 3]}] },
             { "[Foo = number +, 10, Bar = number *]", [1, 2, 10, 3, 4],
               [{"Foo", [1, 2]}, {"Bar", [3, 4]}] },
             { "[Foo = string ?, number *]", [1, 2, 3], [{"Foo", []}] },
             { "[Foo = number *] ^ [1, 2]", [10, 1, 9, 8, 2, 7],
               [{"Foo", [10, 9, 8, 7]}] },
             { "[Foo = number *, 3] ^ [Bar = string *, \"three\"]",
               [1, <<"one">>, 2, <<"two">>, <<"three">>, 3],
               [{"Foo", [1, 2]}, {"Bar", [<<"one">>, <<"two">>]}] },

             { "{\"foo\": Foo = number}", {[{"foo", 6}]}, [{"Foo", 6}] },
             { "{\"foo\": number?, \"bar\": Bar = number}", {[{"bar", 5}]},
               [{"Bar", 5}] },
             { "{\"foo\": Foo = number?, \"bar\": Bar = number?}",
               {[{"foo", 9}]}, [{"Foo", 9}]}
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
             { "true", false },
             { "[65, 66, 67]", <<"ABC">> },
             { "{\"foo\": number}", <<"bar">> },
             { "{\"foo\": number}", {[{"foo", <<"bar">>}]} }
            ]].

-endif.
