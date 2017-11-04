module LaundryView.AsyncExtensions

type Async with
    static member Map f a = async {
        let! a' = a
        return f a'
    }
