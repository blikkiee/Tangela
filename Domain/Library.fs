namespace Domain

module FuelCalculator =

    // mass of the module of a spacecraft
    // estimate fuel based on mass of the module  

    type Mass = Mass of int
    type Fuel = Fuel of int        

    let valueOfMass (Mass mass) = 
        mass

    let valueOfFuel (Fuel fuel) =
        fuel

    let add fuel1 fuel2 =
        (valueOfFuel fuel1) + (valueOfFuel fuel2)
        |> Fuel

    let massOfFuel fuel =
        fuel
        |> valueOfFuel
        |> Mass

    let calculateFuel massInput =
        let divide3 input =
            input / 3
        let substract2 input =
            input - 2
        let notNegative input =
            if input < 0 then 0
            else input
        massInput 
        |> valueOfMass
        |> divide3
        |> substract2
        |> notNegative
        |> Fuel

    let rec calculateFuelRecursive totalFuel massInput =
        if (valueOfMass massInput) = 0 then totalFuel
        else calculateFuelRecursive (add totalFuel (calculateFuel massInput)) (calculateFuel massInput |> massOfFuel)

    let calculateTotalFuelForComponent mass =
        calculateFuelRecursive (Fuel 0) mass

    
    let totalFuel massModules =
        let value (Fuel fuel) =
            fuel
        let sumFuel total fuel =
            value total + value fuel
            |> Fuel
        List.map calculateTotalFuelForComponent massModules 
        |> List.fold sumFuel (Fuel 0)
            
    