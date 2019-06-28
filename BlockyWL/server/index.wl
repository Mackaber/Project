If[
	Not[ValueQ[$Session]],
	$Session = <||>
]


Execute[input_String] := With[
	{time = UnixTime[], result = ToExpression[input]}, 
	$Session[time] = result;
	<|"id"-> time, "result"-> result|>
]


APIFunction[
    {"code" -> "String"},
    Execute[#code] &,
    {"WL", "JSON"}
]


