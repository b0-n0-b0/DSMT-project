-module(input_utils).
-export([input_line_tokenizer/1]).

input_line_tokenizer(Input)->
    % Multiple matches in order to avoid compatibility problems
    Splits = binary:split(Input, [<<"\n">>, <<"\r">>,<<"\n\r">>],[global]),
    lists:map(fun(Split) ->  binary_to_list(Split) end, Splits).
