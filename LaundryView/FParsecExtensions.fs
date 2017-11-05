module LaundryView.FParsecExtensions

open FParsec

type ParserResult<'Result, 'UserState> with
    static member toChoice = function
        | ParserResult.Success (result : 'Result, _ : 'UserState, _) ->
            Choice1Of2 (result)
        | ParserResult.Failure (errorMessage, _, _) ->
            Choice2Of2 (errorMessage)

let manyChoose p = many p |>> List.choose id
