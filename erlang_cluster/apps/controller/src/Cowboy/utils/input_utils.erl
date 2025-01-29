-module(input_utils).
-export([input_line_tokenizer/1, input_splitter/2]).

input_line_tokenizer(Input) ->
    % Multiple matches in order to avoid compatibility problems
    Splits = binary:split(Input, [<<"\n">>, <<"\r">>, <<"\n\r">>], [global]),
    NewSplits = lists:delete(<<>>,Splits),
    lists:map(fun(Split) -> binary_to_list(Split) end, NewSplits).

% there is less then 1 input per worker, we assign all the work to a single worker 
input_splitter(List, 0, _, _)->
    [List];
input_splitter(List, _, Splits, 1)->
    case List of
        [] ->
            Splits;
        _ ->
        [List | Splits]
    end;
input_splitter(List, SplitSize, Splits, SplitNumber)->
    {CurrentSplit, NewList} = lists:split(SplitSize, List),
    input_splitter(NewList, SplitSize, [CurrentSplit|Splits], SplitNumber - 1).
input_splitter(List, HowManySplits) ->
    Length = length(List),
    SplitSize = Length div HowManySplits,
    Splits = input_splitter(List, SplitSize, [], HowManySplits),
    Splits.
