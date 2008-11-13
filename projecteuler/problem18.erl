-module(problem18).
-compile(export_all).

triangle(small) ->
    [
        [ 3 ],
       [ 7,5 ],
      [ 2,4,6 ],
     [ 8,5,9,3 ]
    ];
triangle(large) -> 
    [
                                 [ 75 ],
                               [ 95, 64 ],
                             [ 17, 47, 82 ],
                           [ 18, 35, 87, 10 ],
                         [ 20, 04, 82, 47, 65 ],
                       [ 19, 01, 23, 75, 03, 34 ],
                     [ 88, 02, 77, 73, 07, 63, 67 ],
                   [ 99, 65, 04, 28, 06, 16, 70, 92 ],
                 [ 41, 41, 26, 56, 83, 40, 80, 70, 33 ],
               [ 41, 48, 72, 33, 47, 32, 37, 16, 94, 29 ],
             [ 53, 71, 44, 65, 25, 43, 91, 52, 97, 51, 14 ],
           [ 70, 11, 33, 28, 77, 73, 17, 78, 39, 68, 17, 57 ],
         [ 91, 71, 52, 38, 17, 14, 91, 43, 58, 50, 27, 29, 48 ],
       [ 63, 66, 04, 68, 89, 53, 67, 30, 73, 16, 69, 87, 40, 31 ],
     [ 04, 62, 98, 27, 23, 09, 70, 98, 73, 93, 38, 53, 60, 04, 23 ] ];
triangle(huge) ->
    {ok, Binary} = file:read_file("triangle.txt"),
    lists:map(fun(Str) -> 
		      lists:map(fun(StrNum) ->
					{Int, _} = string:to_integer(StrNum),
					Int
				end,
				string:tokens(Str, " "))
	      end, 
	      string:tokens(binary_to_list(Binary), "\r\n")).

p18(Type) ->
    [L1,L2|Rest] = lists:reverse(triangle(Type)),
    process_rows(L1,L2,Rest).

process_rows(L1, L2, [L3|Rest]) ->
    L2_0 = lists:reverse(process_nodes(L2, L1, [])),
    if Rest == [] ->
	    [Top] = L3,
	    lists:max(L2_0) + Top;
       true ->
	    process_rows(L2_0, L3, Rest)
    end.

process_nodes([], _, Acc) ->
    Acc;
process_nodes([N|L2], [NLeft,NRight|Rest], Acc) ->
    if NLeft > NRight ->
	    process_nodes(L2, [NRight|Rest], [N + NLeft|Acc]);
       true ->
	    process_nodes(L2, [NRight|Rest], [N + NRight|Acc])
    end.
    
    

		
