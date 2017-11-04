#I "bin/Debug"

#r "HttpClient"
#r "FParsec"
#r "FParsecCS"
#r "LaundryView"

open LaundryView

let slug = "..."
let roomId = "..."

Status.getNumberOfFreeMachines slug roomId |> Async.RunSynchronously