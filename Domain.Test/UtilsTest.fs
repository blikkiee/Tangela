namespace Domain.Test

open NUnit.Framework
open Domain.Utils

[<TestFixture>]
type UtilsTestClass () =    
    [<Test>]
    member this.GetElementOfEmptyList () =
        let lst = []
        let result = getElement lst 0
        Assert.AreEqual(None, result)

    [<Test>]
    member this.GetSingleElement () =
        let lst = ["a"; "b"]
        let result = getElement lst 0
        Assert.AreEqual(Some("a"), result)

    [<Test>]
    member this.GetOutOfBoundsElement () =
        let lst = ["a"; "b"]
        let result = getElement lst 3
        Assert.AreEqual(None, result)
    
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