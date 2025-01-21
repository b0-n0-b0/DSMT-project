-record(task, {
    id,                     % Unique identifier for the partial result
    task_module             % User provided module to be executed
    % input_splits = []                  % Input for the module
}).


-record(input_split, {
    id,                   % Unique identifier for the input split
    task_id,              % ID of the associated task
    data                  % The content of the input split
}).


-record(partial_result, {
    id,                   % Unique identifier for the partial result
    task_id,              % ID of the associated task
    data                 % The content of the partial result
}).
