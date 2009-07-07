-module(problem36).
-compile(export_all).

is_palindrom(N) ->
    List = integer_to_list(N),
    List == lists:reverse(List).
