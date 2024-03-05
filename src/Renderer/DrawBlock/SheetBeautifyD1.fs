﻿module SheetBeautifyD1
open SheetBeautifyHelpers
open Optics

open CommonTypes
open DrawModelType.SymbolT
open DrawModelType
open DrawModelType.SheetT
open DrawModelType.BusWireT

//________________________________________________________________________________________________________________________
//TEAM  DELIVERABLES
//D1. sheetAlignScale ASB
//________________________________________________________________________________________________________________________


//List of all the symbols on the sheet
let symbolList (sheet: SheetT.Model) = 
    Map.toList sheet.Wire.Symbol.Symbols
    |> List.map snd


///Returns True if a wire is streightened.
let streightenedWire (wire: Wire) (sheet: SheetT.Model) = 
    let wId = wire.WId
    (visibleSegments wId sheet |> List.length) = 1

///Returns True if a wire has potential to be streightened.
let straighteningPotentialWire (wire: Wire) (sheet: SheetT.Model) =
    let wId = wire.WId
    (visibleSegments wId sheet |> List.length) = 3


let checkIfSinglePortComponent (sym: Symbol) =
    let numInputPorts= sym.Component.InputPorts |> List.length 
    let numOutputPorts= sym.Component.OutputPorts |> List.length
    (numInputPorts + numOutputPorts) = 1


let endOfWire (wire: BusWireT.Wire) (sheet: SheetT.Model) = 
    visibleSegments wire.WId sheet
    |> List.fold (fun start segEnd -> start + segEnd) wire.StartPos

let changeOffsetSign (sym: Symbol) (portId: string) =
    match Map.tryFind portId sym.PortMaps.Orientation with
    | Some Left -> -1.0
    | _ -> 1.0



///Should be used just for wires with 3 visible segments
/// Calculates the offSet based on the middle segmant of a potential wire 
let calculateOffset (wire: Wire) (sheet: SheetT.Model) =
    wire.WId
    |> (fun wId -> visibleSegments wId sheet)
    |> (fun visSegs -> if List.length visSegs = 3 then visSegs[1] else XYPos.zero)



///Returns a list of the potential wire if there are no straight wires already.
let listPotentialWires (wires: list<Wire> ) (sheet: SheetT.Model) =
    let streightWires = wires |> List.exists (fun wire -> streightenedWire wire sheet)
    if streightWires = false
    then wires |> List.filter (fun wire -> straighteningPotentialWire wire sheet)
    else []


//Write a  function that changes the position of the symbol according rhe first potential wire
let getPotentialWireOffset (sheet: SheetT.Model) (wires: list<Wire> ) =
    if wires = []
    then XYPos.zero
    else
        let firstWire = List.item 0 wires
        calculateOffset firstWire sheet
    

let getWiresFromPort (sheet: SheetT.Model) (port: Port) (wireInputPort: bool) =
            sheet.Wire.Wires
            |> Map.toList
            |> List.map snd
            |> List.filter (fun wire -> if wireInputPort = true
                                        then match wire.InputPort with
                                                | InputPortId id when id = port.Id -> true
                                                | _ -> false
                                        else match wire.OutputPort with
                                                | OutputPortId id when id = port.Id -> true
                                                | _ -> false)


///Get all the wires from a Symbol that has strictly just one Port
let allWires1PortSym (sym: Symbol) (sheet: SheetT.Model)=
        if sym.Component.OutputPorts = []
        then
            let i = List.item 0 sym.Component.InputPorts
            let wires = getWiresFromPort sheet i true
            wires
        else
            let o = List.item 0  sym.Component.OutputPorts
            let wires = getWiresFromPort sheet o false
            wires
        
/// VERY IMPORTANT THAT YOU CHECK THAT THE SYMBOL HAS ONLY ONE PORT
let align1PortSymbol (onePortSym: Symbol) (sheet: SheetT.Model)= // (offsetSign: +/-)
    let portId = Map.toList onePortSym.PortMaps.Orientation |> List.item 0 |> fst
    let offsetSign = changeOffsetSign onePortSym portId
    let potentialList = 
        sheet
        |> allWires1PortSym onePortSym
        |> listPotentialWires 
    sheet
    |> potentialList 
    |> getPotentialWireOffset sheet
    |> (fun offset -> BlockHelpers.moveSymbol {X = offset.X * offsetSign; Y = offset.Y * offsetSign} onePortSym)
    // |> (fun newSym -> Optic.set (SheetT.symbolOf_ onePortSym.Id) newSym sheet)
    // |> SheetUpdateHelpers.updateBoundingBoxes


/// <summary>
/// After all the symbols have been moved, update the wiring on the entire sheet.
/// </summary>
/// <param name="newcIdList">List of cIds of all symbols that have moved.</param>
/// <param name="sheet">The sheet to be changed.</param>
/// <param name="symbolMovedBy">Take as 0 for now, not sure what this does, needed in updateWires.</param>
/// <returns>Model with rerouted </returns>

let rerouteWires (newcIdList: List<ComponentId>) (symbolMovedBy: XYPos) (sheet: SheetT.Model) = 
    BusWireRoute.updateWires sheet.Wire newcIdList symbolMovedBy
    |> (fun newWireModel -> Optic.set SheetT.wire_ newWireModel sheet)

let scaleSymbol (newVertical: float option) (newHorizontal: float option) (symbol: Symbol) (sheet: SheetT.Model) =
    let symbols = sheet.Wire.Symbol.Symbols

    let newSymbol = {symbol with VScale = newVertical; HScale = newHorizontal}
    // let newComp = {symbol.Component with H = symbol.Component.H * newVertical; W = symbol.Component.W * newHorizontal}
    // let newSymbol = {symbol with Component = newComp}

    let newSymbols = Map.add symbol.Id newSymbol symbols

    Optic.set SheetT.symbols_ newSymbols sheet
    |> SheetUpdateHelpers.updateBoundingBoxes
    //fix the function Update boundingBoxes for scaling!!!

//Not used but might be needed later
//let offestPortPos (firstPort:Port) (secondPort:Port) (sheet: SheetT.Model)=
//    let firstPortPos = readPortPosOnSheet sheet firstPort
//    let secPortPos = readPortPosOnSheet sheet secondPort
//    let Xdiff  = abs(firstPortPos.Y - secPortPos.Y)
//    let Ydiff = abs(firstPortPos.X - secPortPos.X)
//    let diff = max Xdiff Ydiff
//    diff

///Calculates port offset between two consecutive ports of same type (input or output).
let calcPortOffset (sym: SymbolT.Symbol) (portType: PortType) =
    let portList =
        match portType with
        | PortType.Input -> sym.Component.InputPorts
        | PortType.Output -> sym.Component.OutputPorts
    if List.length portList < 2
    then None
    else 
        (Symbol.getPortPos sym portList[1]) - (Symbol.getPortPos sym portList[0])
        |> (fun pos -> Some (max pos.X pos.Y))

///Calculates the ratio that is needed for scaleSymbol (newVertical and newHorizontal)
let calcPortRatio (outSym: SymbolT.Symbol) (inSym: SymbolT.Symbol) =
    match calcPortOffset outSym PortType.Output, calcPortOffset inSym PortType.Input with
    | Some (outOff), Some (inOff) -> outOff/inOff
    | _ ->
        printfn "Something's wrong I can feel it..." 
        1.0


///Finds two symbols connected by a wire.
let symbolsConnected (wire: Wire) (sheet: SheetT.Model) = 
    //Checks if port is on the symbol based on port id
    let portOnSymbol (pId: string) (symbol: Symbol) =
        Map.toList symbol.PortMaps.Orientation
        |> List.map fst
        |> List.contains pId

    match wire.InputPort, wire.OutputPort with 
    | InputPortId (iId), OutputPortId(oId)  -> 
        List.find (fun sym -> portOnSymbol oId sym) (symbolList sheet),
        List.find (fun sym -> portOnSymbol iId sym) (symbolList sheet)


///Finds the wires between any two symbols on the sheet for all symbols on the sheet.
let connectedSymbolsMap (sheet: SheetT.Model) = 
    Map.toList sheet.Wire.Wires
        |> List.map snd
        |> List.fold (fun map wire -> 
                            let syms = symbolsConnected wire sheet
                            match Map.tryFind syms map with
                            | Some wireList -> Map.add syms (wire :: wireList) map
                            | None -> Map.add syms [wire] map) Map.empty

let findSymbol (symId:ComponentId) (sheet:SheetT.Model)=
    sheet.Wire.Symbol.Symbols |> Map.find  symId

let checkMoreInput1OutputPortSymbol (compId:ComponentId) (sheet:SheetT.Model) =
    let sym = findSymbol compId sheet
    let numInputPorts= sym.Component.InputPorts |> List.length 
    let numOutputPorts= sym.Component.OutputPorts |> List.length
    (numInputPorts >= 0) && (numOutputPorts = 1)
 
/// Gets the all the wires from the output ports. 
let getOutputPortWires (compId:ComponentId) (sheet: SheetT.Model)=
    let sym = findSymbol compId sheet
    let outputPorts= sym.Component.OutputPorts 
    outputPorts 
    |> List.collect (fun port -> getWiresFromPort sheet port false)

// Only take the first wire in the list of wires, you are only straightening them one at a time
// let signedSymbolOffset (compId:ComponentId) (sheet: SheetT.Model)=
//     let sym = findSymbol compId sheet
//     (getOutputPortWires compId sheet)[0] , changeOffsetSign sym sym.Component.OutputPorts[0].Id
// let changeOffsetSign (sym: Symbol) (portId: string) =
    

/// Gets the all the wires from the input ports.
let getInputPortWires (compId:ComponentId) (sheet: SheetT.Model)=
    let sym = findSymbol compId sheet
    let inputPorts= sym.Component.InputPorts 
    inputPorts 
    |> List.collect (fun port -> getWiresFromPort sheet port true)


// You're partitioning the BoundingBoxes map into two maps: symBBMap, containing the bounding boxes of the symbol
// with the specified symId, and otherSymBBMap, containing the bounding boxes of all other symbols.
// You extract the bounding box of the specified symbol (symBB) from symBBMap.
// You collect all the bounding boxes of the other symbols (otherSymsBB) from otherSymBBMap.
// Finally, you iterate over otherSymsBB and check if any of these bounding boxes overlap with the bounding
// box of the specified symbol (symBB), using the overlap2DBox function from BlockHelpers.

let checkSymbolOverlap (symId: ComponentId) (sheet: SheetT.Model) =
    // let symBBMap = Map.filter (fun otherSymId _ -> otherSymId = symId) sheet.BoundingBoxes
    // let otherSymBBMap = Map.filter (fun otherSymId _ -> not(otherSymId = symId)) sheet.BoundingBoxes
    // These two are logically the same as below (I think)
    let symBBMap, otherSymBBMap = Map.partition (fun otherSymId _ -> otherSymId = symId) sheet.BoundingBoxes
    let symBB = (Map.toList >> List.map snd >> List.item 0) symBBMap
    let otherSymsBB = (Map.toList >> List.map snd) otherSymBBMap
    List.exists (fun otherSymBB -> BlockHelpers.overlap2DBox otherSymBB symBB) otherSymsBB




///In this phase all singly-connected components are aligned and their wires are streightend.
let firstPhaseStraightening (sheet: SheetT.Model) =
    let initialOverlap = countIntersectedSymbols sheet
    let singlePortComponents = 
        sheet.Wire.Symbol.Symbols
        |> Map.toList 
        |> List.filter (fun (_, sym) -> checkIfSinglePortComponent sym)


    let changedSymbolList = 
        singlePortComponents
        |> List.map (fun (_, sym) -> align1PortSymbol sym sheet)
    changedSymbolList
    |> List.fold (fun sheet newSym -> 
                        let newSheet = Optic.set (SheetT.symbolOf_ newSym.Id) newSym sheet
                        if countIntersectedSymbols newSheet > initialOverlap
                        then sheet
                        else newSheet) sheet
    // Fix bounding boxes update logic - needs to be updated after each symbol is moved on the sheet
    |> SheetUpdateHelpers.updateBoundingBoxes
    |> rerouteWires (List.map (fun sym -> sym.Id) changedSymbolList) XYPos.zero
    

/// Phase where scaling and moving is done to a pair of multiport symbols (works for custom and non-custom) in order
///  to streighten the wires between them.
let secondPhaseStraightening (sheet: SheetT.Model)=
    connectedSymbolsMap sheet
    |> Map.filter (fun _ value -> List.length value > 1 )
    |> Map.toList 
    |> List.map fst
    |> List.fold (fun sheet syms -> 
                    let scaleValue = calcPortRatio (fst syms) (snd syms)
                    let scaledModel = 
                        scaleSymbol (Some scaleValue) None (snd syms) sheet
                        |> rerouteWires [(snd syms).Id] XYPos.zero
                    let newSym = align1PortSymbol(fst syms) scaledModel
                    Optic.set (SheetT.symbolOf_ newSym.Id) newSym scaledModel
                    |> rerouteWires [newSym.Id] XYPos.zero) sheet

    
    //IMPORTANT NOTE: The align1PortSymbol function has a terible name, it actually aligns a symbol according its wire. 
    //Will change the name when I can focus in order to find the name that makes sense for the both phases.

let extractMoreInput1OutputPortSymbolsSorted (sheet: SheetT.Model)=
    sheet.Wire.Symbol.Symbols 
    |> Map.filter (fun compId _ -> checkMoreInput1OutputPortSymbol compId sheet)
    |> Map.toList 
    |> List.map snd
    |> List.sortBy (fun sym -> sym.Pos.X)
    |> List.rev


let thirdPhase (sheet: SheetT.Model) = 

    extractMoreInput1OutputPortSymbolsSorted sheet
    |> List.map (fun sym -> sym, (getOutputPortWires sym.Id sheet)[0])
    |> Map.ofList
    |> Map.filter (fun _sym wire -> straighteningPotentialWire wire sheet)
    |> Map.map (fun _sym wire -> calculateOffset wire sheet)
    |> Map.map (fun sym unsignedOffset -> 
                    let sign = (changeOffsetSign sym sym.Component.OutputPorts[0].Id)
                    {X = unsignedOffset.X * sign; Y = unsignedOffset.Y * sign})
    |> Map.fold (fun sheet sym offset -> 
                    let newSym = BlockHelpers.moveSymbol offset sym
                    Optic.set (SheetT.symbolOf_ sym.Id) newSym sheet
                    |> rerouteWires [sym.Id] offset
                    |> SheetUpdateHelpers.updateBoundingBoxes) sheet
    

// For all phases, check symbol overlap after move symbol, else keep old sheet - check firstPhaseStraightening logic for reason
// If overlap you dont want to move anything. Third phase logic seems the most sound - refactor first phase
