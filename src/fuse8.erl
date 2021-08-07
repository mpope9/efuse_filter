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
%% Returns a {`Ref<>', hash_method} to a filter to be used in `contain'
%% if a predefined hash function is specified.
%%
%% Otherwise, an `{error, reason}' be returned.
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

new(List, none) ->

    Set = lists:foldl(
        fun (Element, Set) ->
            sets:add_element(Element, Set)
        end, sets:new([{version, 2}]), List),

    DedupedList = sets:to_list(Set),

    FilterFun = case over_100k(List) of
        true -> fun efuse_filter:fuse8_initialize_nif_dirty/1;
        false -> fun efuse_filter:fuse8_initialize_nif/1
    end,
    #fuse8{reference = FilterFun(DedupedList), hashing_method = none}.


-spec contain(fuse8:fuse8(), term()) -> boolean().

contain(Filter, Key) ->
    contain(Filter, Key, false).


-spec contain(fuse8:fuse8(), term(), term()) -> boolean().

contain(#fuse8{reference = Filter, hashing_method = none}, Key, Default) ->

    case efuse_filter:fuse8_contain_nif(Filter, Key) of
        true -> true;
        false -> Default
    end;

contain(#fuse8{reference = Filter, hashing_method = default_hash}, Key, Default) ->

    case efuse_filter:fuse8_contain_nif(Filter, erlang:phash2(Key)) of
        true -> true;
        false -> Default
    end.


-spec over_100k(List::[term()]) -> boolean().

over_100k(List) ->
    over_100k(List, 0).


-spec over_100k(List::[term()], Count::non_neg_integer()) -> boolean().

over_100k(_List, 10_001) -> true;
over_100k([], Count) when Count < 100_001 -> false;
over_100k([_|L], Count) -> over_100k(L, Count + 1).
