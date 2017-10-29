module LaundryView.DynamicData

type MachineComponentState =
    { /// Uniquely identifies a machine component
      serial: string
      /// The current status of this machine component.
      status: MachineComponentStatus
      /// Miscellaneous notes. Will (eventually) be parsed to discover things like:
      /// * how long ago the cycle ended
      /// * if the machine has entered an extended cycle
      notes: string option (* TODO: Parse this *) }
and MachineComponentStatus =
    /// This machine component is out-of-service.
    | OutOfService
    /// The machine component is healthy and available for use.
    | Available
    /// The machine component is healthy, but is currently being used.
    | Unavailable of cycleRemaining: int * cycleDuration: int
    /// The machine component's state could not be determined.
    | Unknown

module Parser =
    open FParsec
    open FParsecExtensions

    type private DynamicData =
        { /// Uniquely identifies a machine component
          serial: string
          /// A flag indicating whether or not this machine component is
          /// out-of-service.
          isOutOfService: bool
          /// A flag indicating whether or not this machine is currently available.
          isAvailable: bool
          /// The amount of time, in minutes, remaining in the current cycle.
          cycleTimeRemainingInMins: int
          /// The amount of time, in minutes, of the entire cycle.
          cycleDurationInMins: int
          /// Miscellaneous notes.
          notes: string option }
     with
        static member toMachineComponentState dynamicData =
            let status =
                match dynamicData with
                | { isOutOfService = oos } when oos ->
                    MachineComponentStatus.OutOfService
                | { isAvailable = ia } when ia ->
                    MachineComponentStatus.Available
                | { cycleTimeRemainingInMins = remain; cycleDurationInMins = dur } ->
                    MachineComponentStatus.Unavailable (remain, dur)
                | _ ->
                    // TODO: Log this
                    MachineComponentStatus.Unknown

            { MachineComponentState.serial = dynamicData.serial
              status = status
              notes = dynamicData.notes  }

    type private DynamicConfigLine =
        | SingleMachineStatus of int * DynamicData
        | DoubleMachineStatus of int * (DynamicData * DynamicData)
        | Skip of unit
    with
        /// Maps a parsed line to a case of the DynamicConfigLine DU
        static member ofParsedLine (id, statuses) =
            match statuses with
            | [ justOne ] ->
                DynamicConfigLine.SingleMachineStatus (id, justOne)
            | [ firstOne; secondOne ] ->
                DynamicConfigLine.DoubleMachineStatus (id, (firstOne, secondOne))
            | _ ->
                // TODO: Log that we couldn't parse id
                DynamicConfigLine.Skip ()
        static member toOption = function
            | DynamicConfigLine.SingleMachineStatus (_, single) ->
                single
                |> DynamicData.toMachineComponentState
                |> List.singleton
                |> Some
            | DynamicConfigLine.DoubleMachineStatus (_, (fst, snd)) ->
                let first = DynamicData.toMachineComponentState fst
                let second = DynamicData.toMachineComponentState snd
                Some [ first; second ]
            | DynamicConfigLine.Skip _ ->
                None

    let private pDynamicDataLine =
        let pMachineStatusKey = skipString "machineStatus" >>. pint32

        let pMachineStatus =
            let pbit = (pchar '0' <|> pchar '1') |>> ((=) '1')
            let pStringNoDelims = manyChars <| noneOf [':'; '&']
            let pStringOpt =
                (pchar '0' |>> (fun _ -> None))
                <|>
                (pStringNoDelims |>> Some)
            let sep = skipChar ':'
            let skipNum = skipSatisfy isDigit

            let pStatusFront =
                pbit .>> sep .>>.         // is available
                pint32 .>> sep .>>.       // mins remaining
                pbit .>> sep .>>.         // is out-of-service
                pStringOpt .>> sep        // serial

            let pStatusBack =
                pint32 .>> sep .>>        // cycle duration
                skipNum .>> sep .>>.      // always 1?
                pStringOpt .>> sep .>>    // notes (optional)
                skipNum .>> sep .>>       // always 0?
                skipNum .>> sep .>>       // always 0?
                skipNewline

            let pStatus = parse {
                let! (((isAvailable, minutesRemaining), isOutOfService), serial) =
                    pStatusFront

                return!
                    match serial with
                    | None ->
                        // If serial is missing, we want to ignore the rest of the
                        // line and throw away whatever we've parsed so far.
                        skipRestOfLine true |>> (fun _ -> None)
                    | Some validSerial ->
                        // Otherwise, continue parsing the rest of the line
                        pStatusBack
                        |>> (fun (duration, notes) ->
                            { DynamicData.isAvailable = isAvailable
                              isOutOfService = isOutOfService
                              cycleTimeRemainingInMins = minutesRemaining
                              cycleDurationInMins = duration
                              serial = validSerial
                              notes = notes } |> Some)
                }

            pMachineStatusKey .>> skipString "=" .>>. manyChoose pStatus
            |>> DynamicConfigLine.ofParsedLine

        let pIgnoreKey = skipRestOfLine true |>> DynamicConfigLine.Skip

        skipString "&" >>. (pMachineStatus <|> pIgnoreKey)

    let private pDynamicData = many pDynamicDataLine

    /// Parses the given dynamic data string from the LaundryView API
    let parseDynamicData =
        run pDynamicData
        >> ParserResult.toResult
        >> Result.map (List.choose DynamicConfigLine.toOption)
        >> Result.map (List.collect id)
