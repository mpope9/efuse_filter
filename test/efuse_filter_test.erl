-module(efuse_filter_test).

-include_lib("eunit/include/eunit.hrl").

basic_test_() ->

    %% Expensive pre-computation.
    %Ints = lists:seq(1, 80000),
    %MappingFun = fun(X) -> "test" ++ integer_to_list(X) end,
    %Strings = lists:map(MappingFun, Ints),

    [
        {
            "fuse8 Test Group",
            [
                ?_test(fuse8_filter())
            ]
        }
    ].

fuse8_filter() ->
   Filter = fuse8:new(["test1", "test2", "test3"]),
   ?assertEqual(true, fuse8:contain(Filter, "test1")),
   ?assertEqual(false, fuse8:contain(Filter, "test4")).
