
If[
    Not[ValueQ[$Session]],
    $Session = <||>
]

(* ---------FUNCTIONS------- *)
DrawInside[el_, list_] :=
    With[{pos = PixelValuePositions[Rasterize@el, 0]},
      Graphics@MapThread[Text, {list[[;; (pos // Length)]], pos}]];


(* ------------------------- *)

StoreResult[id_, expr_] :=
    $Session[id] = expr;

GetResult[id_] :=
    $Session[id]

ExportResult[id_, result_, exporter_] :=
    <|
        "id" -> id,
        "result" -> exporter[result]
    |>

(* ToDataUri[result_] := ExportString[result, {"Base64", "PNG"}]*)
ToDataUri[result_] := ExportString[result, {"Base64", "SVG"}]

ToInputForm[result_] := ToString[result, InputForm]

(*ExportResult[id_, result_] :=
    <|
        "id" -> id,
        "type" -> ToString@Head[result],
        "symbol" -> result,
        "result" -> ToString@result
    |>*)

Execute[input_String, exporter_:ToDataUri] := With[
    {id = CreateUUID[], result = ToExpression[input]},

    StoreResult[id, result];
    ExportResult[id, result, exporter]
]


EvaluationAPI[args__] :=
    HTTPResponse[
        APIFunction[
            {"code" -> "String"},
            Execute[#code, args] &,
            "JSON"
        ],
        <|"Headers"-> {"Access-Control-Allow-Origin" -> "*", "Content-Type" -> "application/json"} |>
    ]

ResultsAPI[args___] :=
    HTTPResponse[
        APIFunction[
            {"id" -> "String"},
            ExportResult[#id, GetResult[#id], args] &,
            "JSON"
        ],
        <|"Headers"-> {"Access-Control-Allow-Origin" -> "*", "Content-Type" -> "application/json"} |>
    ]



URLDispatcher[{
    "/evaluate" ~~ EndOfString :> EvaluationAPI[ToDataUri],
    "/evaluate/input" ~~ EndOfString :> EvaluationAPI[ToInputForm],
    "/result" ~~ EndOfString :> ResultsAPI[ToDataUri],
    "/result/input" ~~ EndOfString :> ResultsAPI[ToInputForm]
}]


