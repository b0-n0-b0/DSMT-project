-module(random_string).
-export([generate/1]).

% Generates a random alphabetic string of the given length
generate(Length) when Length > 0 ->
    % Create a list of random alphabetic characters of the given length
    lists:map(fun(_) -> random_alpha() end, lists:seq(1, Length)).

% Helper function to generate a random alphabetic character
random_alpha() ->
    Alphabet = "abcdefghijklmnopqrstuvwxyz0123456789-_",
    Index = rand:uniform(length(Alphabet)),
    lists:nth(Index, Alphabet).
