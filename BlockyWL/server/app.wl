If[
    Not[ValueQ[$Session]],
    $Session = <||>
]

StoreResult[id_, expr_] :=
    $Session[id] = expr;

GetResult[id_] :=
    $Session[id]

ExportResult[id_, result_] :=
    <|"id" -> id, "result" -> ToString@result|>

Execute[input_String] := With[
    {id = CreateUUID[], result = ToExpression[input]}, 

    StoreResult[id, result];

    ExportResult[id, result]
    
]


EvaluationAPI = 
    APIFunction[
        {"code" -> "String"},
        Execute[#code] &,
        "JSON"
    ]

ResultsAPI = 
    APIFunction[
        {"id" -> "String"},
        ExportResult[#id, GetResult[#id]] &,
        "JSON"
    ]



URLDispatcher[{
    "/" ~~ EndOfString :> EvaluationAPI,
    "/results" ~~ EndOfString :> ResultsAPI
}]


