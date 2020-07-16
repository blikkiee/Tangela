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

