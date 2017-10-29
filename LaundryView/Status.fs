module LaundryView.Status

open EnhancedModeApi
open StaticData
open DynamicData

type FreeMachines =
    { washers: int
      dryers: int }

let private indexStatesBySerial =
    List.map (fun state -> state.serial, state)
    >> Map.ofList

/// Calls the API, parses the responses, joins the static
/// and dynamic data, and determines the overall status
/// of the given room in the given property.
let getNumberOfFreeMachines propertySlug roomId =
    let machines, states =
        getLaundryDataForRoom propertySlug roomId
        |> Hopac.Hopac.run

    let statesBySerial = indexStatesBySerial states

    let getState (mach: MachineComponent) =
        // This *should* throw an exception if the serial number can't
        // be found in the Map; implies that our parsing logic is likely stale.
        mach, statesBySerial.[mach.serial]

    let washers, dryers =
        machines
        |> List.fold (fun (washers, dryers) nextMachine ->
            match nextMachine with
            | { composition = MachineComposition.FrontLoadingWasher washer } ->
                getState washer :: washers, dryers
            | { composition = MachineComposition.DoubleDryer (fst, snd) } ->
                washers, getState snd :: (getState fst :: dryers)
            | { composition = MachineComposition.Unknown } ->
                washers, dryers
            ) ([],[])

    let numAvailable =
        List.filter (snd >> MachineComponentStatus.isCurrentlyUsable)
        >> List.length

    { FreeMachines.washers = numAvailable washers
      dryers = numAvailable dryers }
