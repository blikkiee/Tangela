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
    [<TestCase (112345, true)>]
    member this.ContainsDuplicateDigit (pw, expected) =
        let result = PasswordOption pw |> containsDuplicateDigit
        Assert.AreEqual(expected, result)
    
    [<TestCase (197487, 200000, 1)>]
    [<TestCase (200000, 222223, 2)>]
    member this.CountViablePasswords (lowerBound, upperBound, expected) =
        let result = countViablePasswords lowerBound upperBound
        Assert.AreEqual(expected, result)