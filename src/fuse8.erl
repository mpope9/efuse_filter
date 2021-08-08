%%-----------------------------------------------------------------------------
%% @copyright (C) 2021, Matthew Pope
%% @author Matthew Pope
%% @doc Interface for the fuse8 filter.
%%
%% For a full set of examples, see the GitHub README at 
%%  https://github.com/mpope9/efuse_filter/blob/main/README.md
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
    new_empty/0,
    new_empty/1,
    contain/2,
    contain/3,
    add/2,
    finalize/1
]).

-record(fuse8, {
    reference :: reference() | undefined,
    hashing_method :: default | none,
    elements :: set:set() | undefined
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
%%
%% Do not modify the return value of this function.
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
    #fuse8{
        reference = FilterFun(HashedList), 
        hashing_method = default,
        elements = undefined}.


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
            #fuse8{
                reference = FilterFun(DedupedList),
                hashing_method = none,
                elements = undefined}
    end;

new(_List, _Method) ->
    {error, invalid_hash_method}.


%%-----------------------------------------------------------------------------
%% @doc Initializes an empty filter. This should be filled incrementally.
%% Do not modify the reutrn value of this function.
%% @end
%%-----------------------------------------------------------------------------
-spec new_empty() -> fuse8:fuse8().

new_empty() ->
    #fuse8{
        reference = undefined,
        hashing_method = default,
        elements = sets:new([{version, 2}])
    }.


%%-----------------------------------------------------------------------------
%% @doc Initializes an empty filter. This should be filled incrementally.
%% If a value other than `none' is passed, then `{error, invalid_hash_method}'
%% will be returned. Values passed to `fuse8:add/2' need to be pre-hashed as
%% integers before adding to a filter returned by this function.
%% Do not modify the return value of this function.
%% @end
%%-----------------------------------------------------------------------------
-spec new_empty(atom()) -> fuse8:fuse8() | {error, atom()}.

new_empty(none) ->
    #fuse8{
        reference = undefined,
        hashing_method = none,
        elements = sets:new([{version, 2}])
    };

new_empty(_) ->
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
%%
%% If an invalid filter is passed or if the key is not an integer and the 
%% hashing method is set to `none' then false will be returned.
%%
%% @end
%%-----------------------------------------------------------------------------
-spec contain(Filter::fuse8:fuse8(), Key::term(), Default::term()) -> boolean().

contain(#fuse8{hashing_method = none}, Key, _Default)
    when not is_integer(Key) ->

    false;

contain(#fuse8{reference = Filter}, _Key, _Default)
    when Filter == undefined ->

    false;

contain(#fuse8{reference = Filter, hashing_method = none}, Key, Default)
    when is_integer(Key) ->

    case efuse_filter:fuse8_contain_nif(Filter, Key) of
        true -> true;
        false -> Default
    end;

contain(#fuse8{reference = Filter, hashing_method = default}, Key, Default) ->

    case efuse_filter:fuse8_contain_nif(Filter, erlang:phash2(Key)) of
        true -> true;
        false -> Default
    end;

contain(_Filter, _Key, _Default) ->
    false.


%%-----------------------------------------------------------------------------
%% @doc Adds elements to filter, and applys the default hashing mechanism if
%% `none' wasn't specified in the `fuse8:new_empty/1' function. This function
%% attempts to catch issues early, before they reach the NIF code. So if
%% any elements that are passed when custom hashing was specified this 
%% function will return `{error, pre_hashed_values_should_be_ints}'.
%%
%% This function accepts both a list of elements and a single element.
%% @end
%%-----------------------------------------------------------------------------
-spec add(Filter::fuse8:fuse8(), Element::term()) -> fuse8:fuse8() | {error, atom()}.

add(#fuse8{reference = Reference}, _Elements) when Reference /= undefined ->
    {error, already_initialized_filter};

add(#fuse8{elements = undefined}, _Elements) ->
    {error, uninitialized_filter};

add(#fuse8{hashing_method = default, elements = Elements} = Filter, ElementsInput) 
    when is_list(ElementsInput) ->

    ElementsNew = lists:foldl(
        fun(Element, Acc) ->
            sets:add_element(erlang:phash2(Element), Acc)
        end, Elements, ElementsInput),

    Filter#fuse8{elements = ElementsNew};

add(#fuse8{hashing_method = default, elements = Elements} = Filter, Element) ->
    Filter#fuse8{elements = sets:add_element(erlang:phash2(Element), Elements)};

add(#fuse8{hashing_method = none, elements = Elements} = Filter, ElementsInput) 
    when is_list(ElementsInput) ->

    case lists:any(fun(Val) -> is_integer(Val) end, ElementsInput) of

        false ->
            {error, pre_hashed_values_should_be_ints};

        true ->

            ElementsNew = lists:foldl(
                fun(Element, Acc) ->
                    sets:add_element(Element, Acc)
                end, Elements, ElementsInput),
            Filter#fuse8{elements = ElementsNew}

    end;

add(#fuse8{hashing_method = none, elements = Elements} = Filter, Element) ->

    case is_integer(Element) of

        false ->
            {error, pre_hashed_values_should_be_ints};

        true ->
            Filter#fuse8{elements = sets:add_element(Element, Elements)}
    end.

%%-----------------------------------------------------------------------------
%% @doc Initializes filter internally. Equivalent to calling `fuse8:new'.
%% Elements are deduplicated at this point, so this function should only fail
%% if an already intiialized filter is passed to it.
%%
%% If more than 100K elements have been added, then the dirty version of the 
%% NIF is called. This is based around some simple benchmarking, and 
%% 100K elements initialized in under 1ms.
%% @end
%%-----------------------------------------------------------------------------
finalize(#fuse8{reference = Reference}) when Reference /= undefined ->
    {error, already_initialized_filter};

finalize(#fuse8{elements = undefined}) ->
    {error, invalid_state_error};

finalize(#fuse8{elements = Elements} = Filter) ->

    List = sets:to_list(Elements),
    FilterFun = case over_100k(List) of
        true -> fun efuse_filter:fuse8_initialize_nif_dirty/1;
        false -> fun efuse_filter:fuse8_initialize_nif/1
    end,
    Filter#fuse8{
        reference = FilterFun(List),
        elements = undefined
    }.


-spec over_100k(List::[term()]) -> boolean().

over_100k(List) ->
    over_100k(List, 0).


-spec over_100k(List::[term()], Count::non_neg_integer()) -> boolean().

over_100k(_List, 10_001) -> true;
over_100k([], Count) when Count < 100_001 -> false;
over_100k([_|L], Count) -> over_100k(L, Count + 1).
