namespace Domain.Test

open NUnit.Framework
open Domain.Utils

[<TestFixture>]
type UtilsTestClass () =
    
    [<Test>]
    member this.ReplaceOutOfBoundsElement () =
        let lst = ["a"; "b"]
        let result = replaceElement lst 2 "c"
        Assert.AreEqual(None, result)
    
    [<Test>]
    member this.ReplaceElement () =
        let lst = ["a"; "b"]
        let result = replaceElement lst 1 "c"
        Assert.AreEqual(Some(["a"; "c"]), result)