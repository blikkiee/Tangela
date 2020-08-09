namespace Domain

module Utils =
    let private nonEmptyListAction method lst =
        match lst with
        | head :: tail -> method head tail
        | [] -> None

    let flatMap func input = Option.flatten (Option.map func input)

    let replaceElement (lst: 'a list) (index: int) (newElement: 'a): 'a list option =
        let rec iterateElements currentIndex headLst currentElement tailLst =
            if index = currentIndex
            then Some(headLst @ [ newElement ] @ tailLst)
            else nonEmptyListAction (iterateElements (currentIndex + 1) (headLst @ [ currentElement ])) tailLst

        nonEmptyListAction (iterateElements 0 []) lst

    let add a b =
        match (a, b) with
        | (Some (c), Some (d)) -> Some(c + d)
        | _ -> None

    let multiply a b =
        match (a, b) with
        | (Some (c), Some (d)) -> Some(c * d)
        | _ -> None

    let sum total x = total + x

module FuelCalculator =

    // mass of the module of a spacecraft
    // estimate fuel based on mass of the module

    type Mass = Mass of int
    type Fuel = Fuel of int

    let valueOfMass (Mass mass) = mass

    let valueOfFuel (Fuel fuel) = fuel

    let add fuel1 fuel2 =
        (valueOfFuel fuel1) + (valueOfFuel fuel2) |> Fuel

    let massOfFuel fuel = fuel |> valueOfFuel |> Mass

    let calculateFuel massInput =
        let divide3 input = input / 3
        let substract2 input = input - 2
        let notNegative input = if input < 0 then 0 else input
        massInput
        |> valueOfMass
        |> divide3
        |> substract2
        |> notNegative
        |> Fuel

    let rec calculateFuelRecursive totalFuel massInput =
        if (valueOfMass massInput) = 0
        then totalFuel
        else calculateFuelRecursive (add totalFuel (calculateFuel massInput)) (calculateFuel massInput |> massOfFuel)

    let calculateTotalFuelForComponent mass = calculateFuelRecursive (Fuel 0) mass


    let totalFuel massModules =
        let value (Fuel fuel) = fuel
        let sumFuel total fuel = value total + value fuel |> Fuel
        List.map calculateTotalFuelForComponent massModules
        |> List.fold sumFuel (Fuel 0)

module IntcodeInterpreter =
    open Utils

    type ParameterMode =
        | PositionMode
        | ImmediateMode

    let getParameter program mode parameter =
        let getItem index = List.tryItem index program
        match mode with
        | PositionMode -> parameter |> flatMap getItem
        | ImmediateMode -> parameter

    let decode (input: string) =
        input.Split ',' |> Array.toList |> List.map int

    let encode (input: int list) =
        List.map (sprintf "%d") input |> String.concat ","

    let tryUpdateProgram program index newValue =
        match (index, newValue) with
        | (Some (indx), Some (value)) -> replaceElement program indx value
        | _ -> None

    let addAction program parameters parametermode: int list option * int =
        let (p1, p2, replaceIndex, nextIndex) = parameters
        let (p1mode, p2mode, _) = parametermode
        let param1 = getParameter program p1mode p1
        let param2 = getParameter program p2mode p2
        let updatedProgram =
            add param1 param2 |> tryUpdateProgram program replaceIndex
        (updatedProgram, nextIndex)

    let multiplyAction program parameters parametermode: int list option * int =
        let (p1, p2, replaceIndex, nextIndex) = parameters
        let (p1mode, p2mode, _) = parametermode
        let param1 = getParameter program p1mode p1
        let param2 = getParameter program p2mode p2
        let updatedProgram =
            multiply param1 param2
            |> tryUpdateProgram program replaceIndex
        (updatedProgram, nextIndex)

    let inputAction (input: int) program parameters _: int list option * int =
        let (replaceIndex, _, _, nextIndex) = parameters
        let updatedProgram =
            tryUpdateProgram program replaceIndex (Some input)
        (updatedProgram, nextIndex - 2)

    let outputAction program parameters _: int list option * int =
        let (showIndex, _, _, nextIndex) = parameters
        List.item (Option.get showIndex) program
        |> printfn "%d" // TODO both List.item and Option.get assume no None
        (Some(program), nextIndex - 2)

    let jumpIfTrueAction program parameters parametermode: int list option * int =
        let (p1, p2, _, nextIndex) = parameters
        let (p1mode, p2mode, _) = parametermode
        let param1 = getParameter program p1mode p1 |> Option.get
        let param2 = getParameter program p2mode p2 |> Option.get
        let nextInstructionPointer = 
            if param1 <> 0 then param2
            else nextIndex - 1
        (Some program, nextInstructionPointer)

    let jumpIfFalseAction program parameters parametermode: int list option * int =
        let (p1, p2, _, nextIndex) = parameters
        let (p1mode, p2mode, _) = parametermode
        let param1 = getParameter program p1mode p1 |> Option.get
        let param2 = getParameter program p2mode p2 |> Option.get
        let nextInstructionPointer = 
            if param1 = 0 then param2
            else nextIndex - 1
        (Some program, nextInstructionPointer)

    let lessThanAction program parameters parametermode: int list option * int =
        let (p1, p2, p3, nextIndex) = parameters
        let (p1mode, p2mode, p3mode) = parametermode
        let param1 = getParameter program p1mode p1
        let param2 = getParameter program p2mode p2
        let replaceIndex = getParameter program p3mode p3
        let updatedProgram =
            if param1 < param2 then Some 1 else Some 0
            |> tryUpdateProgram program replaceIndex
        (updatedProgram, nextIndex)

    let equalsAction program parameters parametermode: int list option * int =
        let (p1, p2, p3, nextIndex) = parameters
        let (p1mode, p2mode, p3mode) = parametermode
        let param1 = getParameter program p1mode p1
        let param2 = getParameter program p2mode p2
        let replaceIndex = getParameter program p3mode p3
        let updatedProgram =
            if param1 = param2 then Some 1 else Some 0
            |> tryUpdateProgram program replaceIndex
        (updatedProgram, nextIndex)

    let endProgram _ _ _: int list option * int =
        printfn "end"
        (None, 0)

    let selectAction input opcode =
        match opcode with
        | 1 -> addAction
        | 2 -> multiplyAction
        | 3 -> inputAction input
        | 4 -> outputAction
        | 5 -> jumpIfTrueAction
        | 6 -> jumpIfFalseAction
        | 7 -> lessThanAction
        | 8 -> equalsAction
        | _ -> endProgram // 99

    let doAction program parameters parametermode action = action program parameters parametermode

    let getParameterMode ch =
        if ch = '0' then PositionMode
        else ImmediateMode

    let getOpcodeAndParameterModes instruction =
        let reconstructOpcode (p1:char) (p2:char) = [p1; p2] |> System.String.Concat |> int
        match (string instruction |> Seq.toList) with
        | [p3; p2; p1; opcode1; opcode2] -> reconstructOpcode opcode1 opcode2 , (getParameterMode p1, getParameterMode p2, getParameterMode p3)
        | [p2; p1; opcode1; opcode2] -> reconstructOpcode opcode1 opcode2 , (getParameterMode p1, getParameterMode p2, ImmediateMode)
        | [p1; opcode1; opcode2] -> reconstructOpcode opcode1 opcode2, (getParameterMode p1, PositionMode, ImmediateMode)
        | _ -> instruction, (PositionMode, PositionMode, ImmediateMode)

    let rec readProgram index input program =
        let instruction = List.item index program
        let getParam i = List.tryItem (index + i) program
        let parameters = (getParam 1, getParam 2, getParam 3, index + 4)
        let (opcode, parameterMode) = getOpcodeAndParameterModes instruction
        let next result =
            match result with
            | (Some prgrm, 0) -> prgrm
            | (Some prgrm, indx) -> readProgram indx input prgrm
            | _ -> program
        opcode
        |> (selectAction input)
        |> (doAction program parameters parameterMode)
        |> next

    let start input program = readProgram 0 input program

    let setVerb (verb: int) memory = replaceElement memory 2 verb

    let setNoun (verb: int) memory = replaceElement memory 1 verb

module Wires =
    type Direction =
        | Up
        | Down
        | Left
        | Right

    type WirePathDescription = Direction * int

    type GridX = GridX of int
    type GridY = GridY of int
    type WireCoordinate = GridX * GridY
    type WirePath = WireCoordinate * WireCoordinate
    type Wire = WirePath list

    // translate input to wire
    let toDirection direction =
        match direction with
        | "U" -> Up
        | "D" -> Down
        | "L" -> Left
        | "R" -> Right

    let toWirePathDescription (wirePath: string): WirePathDescription =
        let direction = wirePath.Remove 1 |> toDirection
        let pathLength = wirePath.Substring 1 |> int
        (direction, pathLength)

    let extendWire wire wirePath =
        let (GridX xCoordinate, GridY yCoordinate) = List.last wire

        let newCoordinate =
            match wirePath with
            | (Up, y) -> (GridX xCoordinate, GridY(yCoordinate + y))
            | (Down, y) -> (GridX xCoordinate, GridY(yCoordinate - y))
            | (Right, x) -> (GridX(xCoordinate + x), GridY yCoordinate)
            | (Left, x) -> (GridX(xCoordinate - x), GridY yCoordinate)

        wire @ [ newCoordinate ]

    let toWireCoordinates (wirePathDescription: WirePathDescription list) =
        List.fold extendWire [ (GridX 0, GridY 0) ] wirePathDescription

    let toWire coordinates =
        let rec toWirePath result cs =
            match cs with
            | head :: [ tail ] -> result @ [ (head, tail) ]
            | head :: tail -> toWirePath (result @ [ (head, tail.Head) ]) tail
            | _ -> result

        toWirePath [] coordinates

    let decode (wirePath: string) =
        wirePath.Split ','
        |> Array.toList
        |> List.map toWirePathDescription
        |> toWireCoordinates
        |> toWire

    let normalizePath path =
        let reversedWirePath =
            let (coordinate1, coordinate2) = path
            WirePath(coordinate2, coordinate1)

        let ((GridX x1, GridY y1), (GridX x2, GridY y2)) = path
        if (x1 < x2 || y1 < y2) then path else reversedWirePath

    let isBetween lowerBound upperBound x = x > lowerBound && x < upperBound

    // check single intersection
    let checkIntersection (pathA: WirePath) (pathB: WirePath) =
        let normalizedPathA = normalizePath pathA
        let normalizedPathB = normalizePath pathB
        let ((GridX a1x, GridY a1y), (GridX a2x, GridY a2y)) = normalizedPathA
        let ((GridX b1x, GridY b1y), (GridX b2x, GridY b2y)) = normalizedPathB
        if (a1x = a2x && b1y = b2y)
           && (a1x |> isBetween b1x b2x)
           && (b1y |> isBetween a1y a2y) then
            Some(GridX a1x, GridY b1y)
        else if (a1y = a2y && b1x = b2x)
                && (a1y |> isBetween b1y b2y)
                && (b1x |> isBetween a1x a2x) then
            Some(GridX b1x, GridY a1y)
        else
            None

    let compareWirePathToWire wire path =
        wire
        |> List.map (checkIntersection path)
        |> List.choose id

    let getIntersections (wireA: Wire) (wireB: Wire) =
        wireA
        |> List.map (compareWirePathToWire wireB)
        |> List.concat

    let getManhattenDistance coordinate =
        let (GridX x, GridY y) = coordinate
        abs x + abs y

    let getNearestIntersection rawWireA rawWireB =
        let wireA = decode rawWireA
        let wireB = decode rawWireB
        getIntersections wireA wireB
        |> List.map getManhattenDistance
        |> List.min

    let areAllEqual a b c = a = b && a = c

    let isOnPath coordinate path =
        let ((GridX x1, GridY y1), (GridX x2, GridY y2)) = normalizePath path
        let (GridX cx, GridY cy) = coordinate
        (areAllEqual x1 x2 cx && cy |> isBetween y1 y2)
        || (areAllEqual y1 y2 cy && cx |> isBetween x1 x2)

    let getPathDistance path =
        let ((GridX x1, GridY y1), (GridX x2, GridY y2)) = path
        if (x1 = x2) then y1 - y2 |> abs else x1 - x2 |> abs

    let getPathDistanceToCoordinate path coordinate =
        let ((GridX x1, GridY y1), (GridX x2, _)) = path
        let (GridX cx, GridY cy) = coordinate
        if (x1 = x2) then cy - y1 |> abs else cx - x1 |> abs

    let rec getDistanceToIntersection coordinate wire =
        match wire with
        | [] -> 0
        | path :: _ when isOnPath coordinate path -> getPathDistanceToCoordinate path coordinate
        | path :: paths ->
            getPathDistance path
            + getDistanceToIntersection coordinate paths

    let getTotalLoopDistance wireA wireB coordinate =
        getDistanceToIntersection coordinate wireA
        + getDistanceToIntersection coordinate wireB

    let getShortestLoop rawWireA rawWireB =
        let wireA = decode rawWireA
        let wireB = decode rawWireB
        getIntersections wireA wireB
        |> List.map (getTotalLoopDistance wireA wireB)
        |> List.min

module PasswordCracking =
    type PasswordOption = PasswordOption of int

    let doesNotDecrease password =
        let (PasswordOption pw) = password
        let chars = pw |> string |> Seq.toList
        chars = List.sort chars

    let containsDoubleDigit password =
        let (PasswordOption pw) = password
        pw
        |> string
        |> Seq.toList
        |> List.groupBy id
        |> List.map (fun (_, x) -> x.Length)
        |> List.contains 2

    let countViablePasswords lowerBound upperBound =
        [ lowerBound .. upperBound ]
        |> List.map PasswordOption
        |> List.filter (fun x -> doesNotDecrease x && containsDoubleDigit x)
        |> List.length
