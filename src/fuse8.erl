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
    contain/2,
    contain/3
]).

new(List) when is_list(List) ->

    HashedList = lists:map(fun erlang:phash2/1, List),

    FilterFun = case over_10k(HashedList) of
        true -> fun efuse_filter:fuse8_initialize_nif_dirty/1;
        false -> fun efuse_filter:fuse8_initialize_nif/1
    end,
    {FilterFun(HashedList), default_hash}.


contain({Filter, default_hash}, Key) ->
    contain({Filter, default_hash}, Key, false).

contain({Filter, default_hash}, Key, Default) ->

    case efuse_filter:fuse8_contain_nif(Filter, erlang:phash2(Key)) of
        true -> true;
        false -> Default
    end.


over_10k(List) ->
    over_10k(List, 0).

over_10k(_List, 10_001) -> true;
over_10k([], Count) when Count < 10_001 -> false;
over_10k([_|L], Count) -> over_10k(L, Count + 1).
