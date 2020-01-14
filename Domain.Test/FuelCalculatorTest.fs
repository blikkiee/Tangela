namespace Domain.Test

open NUnit.Framework
open Domain.FuelCalculator

[<TestFixture>]
type TestClass () =

    // day 1 - part 1 \\
    // [<TestCase (12, 2)>]
    // [<TestCase (14, 2)>]
    // [<TestCase (1969, 654)>]
    // [<TestCase (100756, 33583)>]
    // day 1 - part 2 \\
    [<TestCase (2, 0)>]
    [<TestCase (14, 2)>]
    [<TestCase (1969, 966)>]
    [<TestCase (100756, 50346)>]
    member this.CalculateFuelTest (mass, expected) =
        let result = calculateTotalFuelForComponent (Mass mass)
        Assert.AreEqual((Fuel expected), result)

    // day 1 - part 1 \\
    // [<TestCase (12, 12, 12, 6)>]
    // [<TestCase (14, 1969, 100756, 34239)>]
    // day 1 - part 2 \\
    [<TestCase (12, 12, 12, 6)>]
    [<TestCase (14, 1969, 100756, 51314)>]
    member this.SumFuelTest (mass1, mass2, mass3, expected) =
        let massModules = List.map Mass [mass1; mass2; mass3]
        let result = totalFuel massModules
        Assert.AreEqual((Fuel expected), result)
