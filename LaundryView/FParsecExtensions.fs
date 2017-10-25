module LaundryView.FParsecExtensions

open FParsec

type ParserResult<'Result, 'UserState> with
    static member toResult = function
        | ParserResult.Success (result : 'Result, _ : 'UserState, _) ->
            Result.Ok (result)
        | ParserResult.Failure (errorMessage, _, _) ->
            Result.Error (errorMessage)