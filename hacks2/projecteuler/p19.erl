-module(p19).
-compile(export_all).

p19() ->
    Start = calendar:date_to_gregorian_days({1901, 1, 1}),
    End = calendar:date_to_gregorian_days({2000, 12, 31}),
    
    lists:foldr(
      fun(Days, AccIn) ->
	      Date = calendar:gregorian_days_to_date(Days),
	      {_,_,Day} = Date,
	      case {Day, calendar:day_of_the_week(Date)} of
		  {1, 7} -> 1 + AccIn;
		  _ -> AccIn
	      end
      end,
      0, lists:seq(Start, End)).
