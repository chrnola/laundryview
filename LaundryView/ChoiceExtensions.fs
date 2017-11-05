module LaundryView.ChoiceExtensions

type Choice<'T1,'T2> with
    /// Provided for convenience now that we're targeting an older version of
    /// FSharp.Core and don't have Result.Map anymore.
    static member Map f c =
        match c with
        | Choice1Of2 (success: 'T1) -> f success |> Choice1Of2
        | Choice2Of2 (failure: 'T2) -> failure |> Choice2Of2
