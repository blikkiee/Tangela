namespace Domain.Test

open NUnit.Framework
open Domain.PasswordCracking

[<TestFixture>]
type PasswordCrackingTestClass () =

    [<TestCase (123456, true)>]
    [<TestCase (123465, false)>]
    member this.DistanceToIntersection (pw, expected) =
        let result = PasswordOption pw |> doesNotDecrease
        Assert.AreEqual(expected, result)

    [<TestCase (123456, false)>]
    [<TestCase (123444, false)>]
    [<TestCase (111122, true)>]
    [<TestCase (112345, true)>]
    member this.ContainsDoubleDigit (pw, expected) =
        let result = PasswordOption pw |> containsDoubleDigit
        Assert.AreEqual(expected, result)
    
    [<TestCase (197487, 222233, 1)>]
    [<TestCase (200000, 222244, 2)>]
    member this.CountViablePasswords (lowerBound, upperBound, expected) =
        let result = countViablePasswords lowerBound upperBound
        Assert.AreEqual(expected, result)