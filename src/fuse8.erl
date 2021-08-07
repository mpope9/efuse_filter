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
    {FilterFun(HashedList), default_hash}.

new(List, none) ->

    HashedSet = lists:foldl(
        fun (Element, Set) ->
            sets:add_element(Element, Set)
        end, sets:new([{version, 2}]), List),

    HashedList = sets:to_list(HashedSet),
    FilterFun = case over_100k(List) of
        true -> fun efuse_filter:fuse8_initialize_nif_dirty/1;
        false -> fun efuse_filter:fuse8_initialize_nif/1
    end,
    {FilterFun(List), none}.


contain(Filter, Key) ->
    contain(Filter, Key, false).

contain({Filter, none}, Key, Default) ->

    case efuse_filter:fuse8_contain_nif(Filter, Key) of
        true -> true;
        false -> Default
    end;

contain({Filter, default_hash}, Key, Default) ->

    case efuse_filter:fuse8_contain_nif(Filter, erlang:phash2(Key)) of
        true -> true;
        false -> Default
    end.


over_100k(List) ->
    over_100k(List, 0).

over_100k(_List, 10_001) -> true;
over_100k([], Count) when Count < 100_001 -> false;
over_100k([_|L], Count) -> over_100k(L, Count + 1).
