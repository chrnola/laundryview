#I "bin/Debug"

#r "Hopac.Core"
#r "Hopac"
#r "HttpFs"
#r "FParsec"
#r "FParsecCS"
#r "LaundryView"

open LaundryView

let slug = "..."
let roomId = "..."

Status.getNumberOfFreeMachines slug roomId