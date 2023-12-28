%% Part of the erlix library
-record(router, {module = nil, imports = [], routes = []}).
-record(scope, {path = "/", pipe_through = [], routes = []}).
-record(route, {method = get, path = "/", controller = nil, action = nil}).
