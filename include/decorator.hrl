-ifndef(__DECORATOR_491352617__).
-define(__DECORATOR_491352617__, true).

-compile([{parse_transform, decorator}]).

-ifdef(nodecorator).
    -define(decorator(X), -file(?FILE, ?LINE)).
-else.
    -define(decorator(X), -decorator(X)).
-endif.

-endif. %%% __DECORATOR_491352617__