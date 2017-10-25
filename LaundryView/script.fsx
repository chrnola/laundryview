#I "bin/Debug"

#r "Hopac.Core"
#r "Hopac"
#r "HttpFs"
#r "FParsec"
#r "LaundryView"

open LaundryView

let slug = "..."
let roomId = "..."

EnhancedModeApi.getLaundryDataForRoom slug roomId