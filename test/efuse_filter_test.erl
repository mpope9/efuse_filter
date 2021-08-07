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
                ?_test(fuse8_dirty_init(Strings))
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
    ?assertEqual(fuse8:contain(invalid, invalid), {error, bad_filter}).

fuse8_bad_key() ->
    Filter = fuse8:new([1, 2], none),
    ?assertEqual(fuse8:contain(Filter, invalid), {error, bad_key}).

fuse8_invalid_pre_hash() ->
    ?assertEqual(fuse8:new([asdf], none), 
                 {error, pre_hashed_values_should_be_ints}).

fuse8_dirty_init(Strings) ->
    Filter = fuse8:new(Strings),
    ?assertEqual(true, fuse8:contain(Filter, "test1")),
    ?assertEqual(false, fuse8:contain(Filter, "test100002")).
