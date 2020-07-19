namespace Domain

module Utils =
    let private nonEmptyListAction method lst =
        match lst with
        | head :: tail -> method head tail
        | [] -> None

    let flatMap func input = Option.flatten (Option.map func input)

    let getElement (lst: 'a list) index =
        let rec toElement lst currentIndex =
            let checkIndex head tail =
                if index = currentIndex then Some(head) else toElement tail (currentIndex + 1)

            nonEmptyListAction checkIndex lst

        if lst.IsEmpty then None else toElement lst 0

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

    let decode (input: string) =
        input.Split ',' |> Array.toList |> List.map int

    let encode (input: int list) =
        List.map (sprintf "%d") input |> String.concat ","

    let calculate (lst: int list) method inputPosition1 inputPosition2 =
        let input1 = flatMap (getElement lst) inputPosition1
        let input2 = flatMap (getElement lst) inputPosition2
        method input1 input2

    let replace (lst: int list) index calculation =
        match (index, calculation) with
        | (Some (i), Some (c)) -> replaceElement lst i c
        | _ -> None

    let rec nextAction currentIndex (input: int list) =
        let getInt = getElement input
        match (getElement input currentIndex) with
        | Some (1) ->
            calculate input add (getInt (1 + currentIndex)) (getInt (2 + currentIndex))
            |> replace input (getInt (3 + currentIndex))
            |> flatMap (nextAction (currentIndex + 4))
        | Some (2) ->
            calculate input multiply (getInt (1 + currentIndex)) (getInt (2 + currentIndex))
            |> replace input (getInt (3 + currentIndex))
            |> flatMap (nextAction (currentIndex + 4))
        | Some (99) -> Some(input)
        | _ -> None

    let start (input: int list) = nextAction 0 input

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

    let containsDuplicateDigit password =
        let (PasswordOption pw) = password
        let chars = pw |> string |> Seq.toList
        chars
        |> List.distinct
        |> (fun x -> x.Length < chars.Length)

    let countViablePasswords lowerBound upperBound =
        [ lowerBound .. upperBound ]
        |> List.map PasswordOption
        |> List.filter (fun x -> doesNotDecrease x && containsDuplicateDigit x)
        |> List.length
