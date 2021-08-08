-module(efuse_filter_test).

-include_lib("eunit/include/eunit.hrl").

basic_test_() ->

    %% Expensive pre-computation.
    Ints = lists:seq(1, 100_001),
    MappingFun = fun(X) -> "test" ++ integer_to_list(X) end,
    Strings = lists:map(MappingFun, Ints),

    [
        {
            "fuse8 Test Group",
            [
                ?_test(fuse8_filter()),
                ?_test(fuse8_filter_no_hash()),
                ?_test(fuse8_invalid_hash()),
                ?_test(fuse8_contain_bad_filter()),
                ?_test(fuse8_bad_key()),
                ?_test(fuse8_invalid_pre_hash()),
                ?_test(fuse8_dirty_init(Strings)),
                ?_test(fuse8_incremental()),
                ?_test(fuse8_incremental_already_initialized()),
                ?_test(fuse8_incremental_custom_hash()),
                ?_test(fuse8_double_finalize()),
                ?_test(fuse8_incremental_custom_hash_bad_key()),
                ?_test(fuse8_finalize_on_new()),
                ?_test(fuse8_incremental_invalid_hash()),
                ?_test(fuse8_serialization()),
                ?_test(fuse8_serialization_default_hash())
            ]
        }
    ].

fuse8_filter() ->
   Filter = fuse8:new(["test1", "test2", "test3"]),
   ?assertEqual(true, fuse8:contain(Filter, "test1")),
   ?assertEqual(false, fuse8:contain(Filter, "test4")).

fuse8_filter_no_hash() ->
    Filter = fuse8:new([1, 2, 3], none),
    ?assertEqual(true, fuse8:contain(Filter, 1)),
    ?assertEqual(false, fuse8:contain(Filter, 4)).

fuse8_invalid_hash() ->
    Filter = fuse8:new([1, 2], invalid),
    ?assertEqual(Filter, {error, invalid_hash_method}).

fuse8_contain_bad_filter() ->
    ?assertEqual(fuse8:contain(invalid, invalid), false).

fuse8_bad_key() ->
    Filter = fuse8:new([1, 2], none),
    ?assertEqual(fuse8:contain(Filter, invalid), false).

fuse8_invalid_pre_hash() ->
    ?assertEqual(fuse8:new([asdf], none), 
                 {error, pre_hashed_values_should_be_ints}).

fuse8_dirty_init(Strings) ->
    Filter = fuse8:new(Strings),
    ?assertEqual(true, fuse8:contain(Filter, "test1")),
    ?assertEqual(false, fuse8:contain(Filter, "test100002")).

fuse8_incremental() ->
    Filter0 = fuse8:new_empty(),
    Filter1 = fuse8:add(Filter0, 1),
    Filter2 = fuse8:add(Filter1, [2, 3]),
    Filter3 = fuse8:finalize(Filter2),
    ?assertEqual(true, fuse8:contain(Filter3, 1)),
    ?assertEqual(true, fuse8:contain(Filter3, 2)),
    ?assertEqual(true, fuse8:contain(Filter3, 3)).

fuse8_incremental_already_initialized() ->
    Filter = fuse8:new([asdf]),
    ?assertEqual(fuse8:add(Filter, 1), {error, already_initialized_filter}).

fuse8_incremental_custom_hash() ->
    Filter0 = fuse8:new_empty(none),
    Filter1 = fuse8:add(Filter0, [1, 2]),
    Filter2 = fuse8:add(Filter1, 3),
    Filter3 = fuse8:finalize(Filter2),
    ?assertEqual(true, fuse8:contain(Filter3, 1)),
    ?assertEqual(true, fuse8:contain(Filter3, 2)),
    ?assertEqual(true, fuse8:contain(Filter3, 3)).

fuse8_double_finalize() ->
    Filter0 = fuse8:new_empty(),
    Filter1 = fuse8:add(Filter0, 1),
    Filter2 = fuse8:finalize(Filter1),
    ?assertEqual(fuse8:finalize(Filter2), {error, already_initialized_filter}).

fuse8_incremental_custom_hash_bad_key() ->
    Filter0 = fuse8:new_empty(none),
    ?assertEqual(fuse8:add(Filter0, asdf), {error, pre_hashed_values_should_be_ints}),
    ?assertEqual(fuse8:add(Filter0, [asdf]), {error, pre_hashed_values_should_be_ints}).

fuse8_finalize_on_new() ->
    Filter0 = fuse8:new([1, 2]),
    ?assertEqual(fuse8:finalize(Filter0), {error, already_initialized_filter}).

fuse8_incremental_invalid_hash() ->
    ?assertEqual(fuse8:new_empty(invalid), {error, invalid_hash_method}).

fuse8_serialization() ->
    Filter0 = fuse8:new([1, 2, 3, 4]),

    Bin0 = fuse8:to_bin(Filter0),
    ?assertEqual(true, fuse8:contain(Bin0, 4)),

    Filter1 = fuse8:from_bin(Bin0),
    ?assertEqual(true, fuse8:contain(Filter1, 4)),

    Filter2 = fuse8:new([1, 2, 3, 4]),
    Bin1 = fuse8:to_bin(Filter2),
    ?assertEqual(Bin0, Bin1).

fuse8_serialization_default_hash() ->
    Filter0 = fuse8:new([1, 2, 3, 4], none),

    Bin0 = fuse8:to_bin(Filter0),
    ?assertEqual(true, fuse8:contain(Bin0, 4, false, none)),

    %% Bad key
    ?assertEqual(false, fuse8:contain(Bin0, "asdf", false, none)),

    %% Bad filter
    ?assertEqual(false, fuse8:contain(invalid, 1, false, none)),

    Filter1 = fuse8:from_bin(Bin0, none),
    ?assertEqual(true, fuse8:contain(Filter1, 4, false)),

    Filter2 = fuse8:new([1, 2, 3, 4], none),
    Bin1 = fuse8:to_bin(Filter2),
    ?assertEqual(Bin0, Bin1).
