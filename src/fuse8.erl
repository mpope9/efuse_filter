%%-----------------------------------------------------------------------------
%% @copyright (C) 2021, Matthew Pope
%% @author Matthew Pope
%% @doc Interface for the fuse8 filter.
%%
%% Example usage:
%% ```
%% Filter = fuse8:new(["cat", "dog", "mouse"]),
%% true   = fuse8:contain(Filter, "cat"),
%% false  = fuse8:contain(Filter, "goose").
%% '''
%% @end
%%-----------------------------------------------------------------------------
-module(fuse8).

-export([
    new/1,
    new/2,
    contain/2,
    contain/3
]).

-record(fuse8, {
    reference :: reference(), 
    hashing_method :: default_hash | none | fun((term()) -> non_neg_integer())
}).

-type fuse8() :: #fuse8{}.

-export_type([fuse8/0]).

%%-----------------------------------------------------------------------------
%% @doc Initializes the fuse filter
%%
%% Initializes a fuse8 filter frim the passed values. Each value is hashed
%% using the `erlang:phash2/1' function.
%%
%% Returns a fuse8 type, which is a filter to be used in `contain'
%% If a predefined set of hashes is desired, pass `none' as the second
%% argument.
%%
%% Otherwise, an `{error, reason}' tuple will be returned.
%% @end
%%-----------------------------------------------------------------------------
-spec new(List::[term()]) -> fuse8:fuse8() | {error, atom()}.

new(List) when is_list(List) ->

    HashedSet = lists:foldl(
        fun (Element, Set) ->
            sets:add_element(erlang:phash2(Element), Set)
        end, sets:new([{version, 2}]), List),

    HashedList = sets:to_list(HashedSet),

    FilterFun = case over_100k(HashedList) of
        true -> fun efuse_filter:fuse8_initialize_nif_dirty/1;
        false -> fun efuse_filter:fuse8_initialize_nif/1
    end,
    #fuse8{reference = FilterFun(HashedList), hashing_method = default_hash}.


-spec new(List::[term()], atom()) -> fuse8:fuse8() | {error, atom()}.

new(List, none) when is_list(List) ->

    Set = lists:foldl(
        fun (Element, Set) ->
            sets:add_element(Element, Set)
        end, sets:new([{version, 2}]), List),

    DedupedList = sets:to_list(Set),

    AllInts = lists:any(fun(Val) -> is_integer(Val) end, DedupedList),

    case AllInts of

        false ->
            {error, pre_hashed_values_should_be_ints};

        true ->

            FilterFun = case over_100k(List) of
                true -> fun efuse_filter:fuse8_initialize_nif_dirty/1;
                false -> fun efuse_filter:fuse8_initialize_nif/1
            end,
            #fuse8{reference = FilterFun(DedupedList), hashing_method = none}
    end;

new(_List, _Method) ->
    {error, invalid_hash_method}.


%%-----------------------------------------------------------------------------
%% @doc Tests to see if the passed argument is in the filter.  The first
%% argument must be the pre-initialized filter. Returns `false' if the
%% argument is not found.
%%
%% A filter previously serialized by `to_bin' is allowed
%% @end
%%-----------------------------------------------------------------------------
-spec contain(Filter::fuse8:fuse8(), Key::term()) -> boolean().

contain(Filter, Key) ->
    contain(Filter, Key, false).


%%-----------------------------------------------------------------------------
%% @doc Tests to see if the passed argument is in the filter.  The first
%% argument must be the pre-initialized filter. If the value is not found,
%% the third arguement is returned instead of `false'.
%%
%% A filter previously serialized by `to_bin' is allowed
%% @end
%%-----------------------------------------------------------------------------
-spec contain(Filter::fuse8:fuse8(), Key::term(), Default::term()) -> boolean().

contain(#fuse8{hashing_method = none}, Key, _Default)
    when not is_integer(Key) ->

    {error, bad_key};

contain(#fuse8{reference = Filter, hashing_method = none}, Key, Default)
    when is_integer(Key) ->

    case efuse_filter:fuse8_contain_nif(Filter, Key) of
        true -> true;
        false -> Default
    end;

contain(#fuse8{reference = Filter, hashing_method = default_hash}, Key, Default) ->

    case efuse_filter:fuse8_contain_nif(Filter, erlang:phash2(Key)) of
        true -> true;
        false -> Default
    end;

contain(_Filter, _Key, _Default) ->
    {error, bad_filter}.


-spec over_100k(List::[term()]) -> boolean().

over_100k(List) ->
    over_100k(List, 0).


-spec over_100k(List::[term()], Count::non_neg_integer()) -> boolean().

over_100k(_List, 10_001) -> true;
over_100k([], Count) when Count < 100_001 -> false;
over_100k([_|L], Count) -> over_100k(L, Count + 1).
