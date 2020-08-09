namespace Domain.Test

open NUnit.Framework
open Domain.IntcodeInterpreter

[<TestFixture>]
type IntcodeInterpreterTestClass() =

    [<TestCase("1,0,0,0,99", "2,0,0,0,99")>]
    [<TestCase("2,3,0,3,99", "2,3,0,6,99")>]
    [<TestCase("2,4,4,5,99,0", "2,4,4,5,99,9801")>]
    [<TestCase("1,1,1,4,99,5,6,0,99", "30,1,1,4,2,5,6,0,99")>]
    [<TestCase("1,9,10,3,2,3,11,0,99,30,40,50", "3500,9,10,70,2,3,11,0,99,30,40,50")>]
    member this.IntcodeInterpreter(input, expected) =
        let result =
            decode input 
            |> start 
            |> Option.get 
            |> encode

        Assert.AreEqual(expected, result)
