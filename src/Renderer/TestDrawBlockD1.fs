module TestDrawBlockD1

open TestDrawBlock
open TestDrawBlock.HLPTick3
open CommonTypes
open DrawModelType
open Symbol
open Optics
open GenerateData
open RotateScale

/// Rotates a symbol by a specified amount
///
/// label - The label of the symbol to be rotated
///
/// rotation - The amount to rotate the symbol by
///
/// sheet - The sheet model on which symbol rotation needs to be changed.
let rotateSymbol (rotation: Rotation) (label: string) (sheet: SheetT.Model): SheetT.Model =
    let symMap = sheet.Wire.Symbol.Symbols
    let symbol =
        (Map.toList >> List.map snd) symMap
        |> List.find (fun sym -> sym.Component.Label = label)

    let symCentre = getRotatedSymbolCentre symbol // If not rotated, just returns the correct centre, but accounts for rotated case.
    let symId = ComponentId symbol.Component.Id

    let rotatedSymbol =
        symMap
        |> Map.tryFind symId
        |> Option.map (fun sym -> rotateSymbolInBlock rotation symCentre sym) // The symbol is the only one in the block

    match rotatedSymbol with
    | Some (sym) ->
        let newSymbols =
            (symId, sym)
            |> symMap.Add
        Optic.set SheetT.symbols_ newSymbols sheet
        |> SheetUpdateHelpers.updateBoundingBoxes // Need to recalculate bounding boxes because rotation changes them
    | None ->
        printf $"Given symbol {symbol.Component.Label} does not exist on the sheet. Returning sheet before change."
        sheet

let verticalLinePositions =
    fromList [-42.0..7.0..42.0]


// Initial test circuit with 1 MUX, crossed wires for input symbol and an output symbol with offset.
let simpleMuxTest (inputPos: XYPos) =
    initSheetModel
    |> Builder.placeSymbol "I1" (Input1(1, None)) {X = inputPos.X; Y = inputPos.Y + 40.0}
    |> Result.bind (Builder.placeSymbol "I2" (Input1(1, None)) {X = inputPos.X; Y = inputPos.Y - 40.0})
    |> Result.bind (Builder.placeSymbol "MUX" (Mux2) middleOfSheet)
    |> Result.bind (Builder.placeSymbol "S1" (Input1(1, None)) {X = middleOfSheet.X - 20.0; Y = middleOfSheet.Y + 150.0})
    |> Result.bind (Builder.placeSymbol "O1" (Output(1)) {X = middleOfSheet.X + 100.0; Y = middleOfSheet.Y + 20.0})
    |> TestLib.getOkOrFail
    |> rotateSymbol Degree90 "S1"
    |> Builder.placeWire (portOf "I1" 0) (portOf "MUX" 0)
    |> Result.bind (Builder.placeWire (portOf "S1" 0) (portOf "MUX" 2))
    |> Result.bind (Builder.placeWire (portOf "I2" 0) (portOf "MUX" 1))
    |> Result.bind (Builder.placeWire (portOf "MUX" 0) (portOf "O1" 0))
    |> TestLib.getOkOrFail


// Simple yet important case - what to do when there are two non straight wires coming out of a
// singly connected component? Also highlights 4 visible segments for the select input in certain orientations.
// 4 visible segments != nearly straight, but it CAN be straightened. Extension work.
let multipleConnectionsTest (offsetFromCentre: float) =
    initSheetModel
    |> Builder.placeSymbol "I1" (Input1(1, None)) {X = middleOfSheet.X; Y = middleOfSheet.Y + offsetFromCentre}
    |> Result.bind (Builder.placeSymbol "S" (Input1(1, None)) {X = middleOfSheet.X; Y = middleOfSheet.Y - offsetFromCentre})
    |> Result.bind (Builder.placeSymbol "MUX" (Mux2) middleOfSheet)
    |> Result.bind (Builder.placeSymbol "O1" (Output(1)) {X = middleOfSheet.X + 100.0; Y = middleOfSheet.Y + 20.0})
    |> Result.bind (Builder.placeWire (portOf "I1" 0) (portOf "MUX" 0))
    |> Result.bind (Builder.placeWire (portOf "I1" 0) (portOf "MUX" 1))
    |> Result.bind (Builder.placeWire (portOf "S" 0) (portOf "MUX" 2))
    |> Result.bind (Builder.placeWire (portOf "MUX" 0) (portOf "O1" 0))
    |> TestLib.getOkOrFail


// Circuit demonstrating enough degrees of freedom to straighten non singly constrained components.
let makeMultipleMux (_: XYPos) =
    initSheetModel
    |> Builder.placeSymbol "MUX2" Mux2 middleOfSheet
    |> Result.bind (Builder.placeSymbol "MUX1" Mux2 {X = middleOfSheet.X - 175.0; Y = middleOfSheet.Y - 20.0})
    |> Result.bind (Builder.placeSymbol "A" (Input1(1, None)) {X = middleOfSheet.X - 300.0; Y = middleOfSheet.Y - 56.0})
    |> Result.bind (Builder.placeSymbol "B" (Input1 (1, None)) {X = middleOfSheet.X - 300.0; Y = middleOfSheet.Y})
    |> Result.bind (Builder.placeSymbol "S2" (Input1 (1, None)) {X = middleOfSheet.X - 250.0; Y = middleOfSheet.Y + 100.0})
    |> Result.bind (Builder.placeSymbol "C" (Output(1)) {X = middleOfSheet.X + 150.0; Y = middleOfSheet.Y + 10.0})
    |> Result.bind (Builder.placeWire (portOf "A" 0) (portOf "MUX1" 0))
    |> Result.bind (Builder.placeWire (portOf "B" 0) (portOf "MUX1" 1))
    |> Result.bind (Builder.placeWire (portOf "S2" 0) (portOf "MUX2" 2))
    |> Result.bind (Builder.placeWire (portOf "MUX1" 0) (portOf "MUX2" 0))
    |> Result.bind (Builder.placeWire (portOf "MUX2" 0) (portOf "C" 0))
    |> TestLib.getOkOrFail


// Circuit emulating results from orderFlip, for compatibility with the other beautify algorithms.
let orderFlipTest (_: XYPos) =
    initSheetModel
    |> Builder.placeSymbol "S1" (Input1(1, None)) {X = middleOfSheet.X - 275.0; Y = middleOfSheet.Y - 40.0}
    |> Result.bind (Builder.placeSymbol "S2" (Input1(1, None)) {X = middleOfSheet.X - 275.0; Y = middleOfSheet.Y + 60.0})
    |> Result.bind (Builder.placeSymbol "MUX2" (Mux2) middleOfSheet)
    |> Result.bind (Builder.placeSymbol "MUX1" (Mux2) {X = middleOfSheet.X - 150.0; Y = middleOfSheet.Y - 150.0})
    |> Result.bind (Builder.placeSymbol "G1" (GateN(And, 2)) {X = middleOfSheet.X + 150.0; Y = middleOfSheet.Y - 160.0})
    |> Result.bind (Builder.placeWire (portOf "S1" 0) (portOf "MUX2" 1))
    |> Result.bind (Builder.placeWire (portOf "S2" 0) (portOf "MUX2" 2))
    |> Result.bind (Builder.placeWire (portOf "MUX1" 0) (portOf "MUX2" 0))
    |> Result.bind (Builder.placeWire (portOf "MUX1" 0) (portOf "G1" 0))
    |> Result.bind (Builder.placeWire (portOf "MUX2" 0) (portOf "G1" 1))
    |> TestLib.getOkOrFail


// Defined custom component
let customMain =
    {
        Name = "MAIN";
        InputLabels = [("A", 1); ("B", 1); ("S2", 1); ("S1", 1)];
        OutputLabels = [("E", 1); ("F", 1); ("G", 1)];
        Form = None
        Description = None
    }


// Aligning and scaling of circuits with custom components
let customComponentScaling (_: XYPos) =
    initSheetModel
    |> Builder.placeSymbol "MAIN1" (Custom(customMain)) middleOfSheet
    |> Result.bind (Builder.placeSymbol "MAIN2" (Custom(customMain)) {middleOfSheet with X = middleOfSheet.X + 200.0})
    |> Result.bind (Builder.placeWire (portOf "MAIN1" 0) (portOf "MAIN2" 0))
    |> Result.bind (Builder.placeWire (portOf "MAIN1" 1) (portOf "MAIN2" 1))
    |> Result.bind (Builder.placeWire (portOf "MAIN1" 2) (portOf "MAIN2" 2))
    |> TestLib.getOkOrFail
