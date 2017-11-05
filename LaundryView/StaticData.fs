module LaundryView.StaticData

/// A Machine is made up of one or more machine components.
type Machine =
    { /// Uniquely identifies a machine in a room.
      id: int
      composition: MachineComposition }
and
    /// Describes the arrangement of usable components within the machine.
    MachineComposition =
        /// A single washing machine.
        | FrontLoadingWasher of MachineComponent
        /// A double washing machine.
        | DoubleDryer of MachineComponent * MachineComponent
        /// Unknown
        | Unknown
and
    /// Uniquely identifies a machine component. This is
    /// the smallest entity for which we can report status.
    MachineComponent =
    { id: string
      serial: string }

module Parser =
    open ChoiceExtensions
    open FParsec
    open FParsecExtensions

    let [<Literal>] private FrontLoadingWasherCode = "washFL"
    let [<Literal>] private DoubleDryerCode = "dblDry"

    type Machine with
        static member ofParsedLine (machineId, valueSegments) =
            let machComp =
                match valueSegments with
                | [_; _;_ ; code; compId; serial; _; _]
                    when code = FrontLoadingWasherCode ->
                        let comp =
                            { MachineComponent.id = compId; serial = serial }

                        MachineComposition.FrontLoadingWasher (comp)
                | [_; _;_ ; code; comp1Id; serial1; _; _; comp2Id; serial2; _]
                    when code = DoubleDryerCode ->
                        let comp1 =
                            { MachineComponent.id = comp1Id; serial = serial1 }
                        let comp2 =
                            { MachineComponent.id = comp2Id; serial = serial2 }
                        MachineComposition.DoubleDryer (comp1, comp2)
                | _ ->
                    MachineComposition.Unknown

            { Machine.id = machineId
              composition = machComp }

    type private StaticConfigLine =
        /// Contains data about a machines in the room.
        | MachineData of Machine
        /// Contains irrelevant data about the appearance of the room.
        | Other of unit
    with
        static member toOption = function
            | MachineData (machine) -> Some machine
            | Other _ -> None

    let private pStaticDataLine =
        let pMachDataKey = skipString "machineData" >>. pint32

        let pMachData =
            let pValueSegment =
                let pSegment = manyChars <| noneOf [':'; '&']
                pSegment .>> skipString ":" .>> skipMany newline

            pMachDataKey .>> skipString "=" .>>. (many pValueSegment)
            |>> (Machine.ofParsedLine >> StaticConfigLine.MachineData)

        let pIgnoreKey = skipRestOfLine true |>> StaticConfigLine.Other

        skipString "&" >>. (pMachData <|> pIgnoreKey)

    let private pStaticData = many pStaticDataLine

    type private Choice = Choice<StaticConfigLine list, string>

    /// Parses the given static data string from the LaundryView API
    let parseStaticData =
        run pStaticData
        >> ParserResult.toChoice
        >> Choice.Map (List.choose StaticConfigLine.toOption)
