namespace Domain.Test

open NUnit.Framework
open Domain.Wires

[<TestFixture>]
type WiresTestClass () =

    [<Test>]
    member this.ToWire () =
        let description = "R8,U5,L5,D3"
        let result = decode description
        let expected: Wire = [
            ((GridX 0, GridY 0), (GridX 8, GridY 0));
            ((GridX 8, GridY 0), (GridX 8, GridY 5));
            ((GridX 8, GridY 5), (GridX 3, GridY 5));
            ((GridX 3, GridY 5), (GridX 3, GridY 2))]
        Assert.AreEqual(expected, result)

    [<TestCase (0, 1, 2, 1, 1, 0, 1, 2, 1, 1)>]
    [<TestCase (2, 1, 0, 1, 1, 0, 1, 2, 1, 1)>]
    member this.IntersectionFound (a1x, a1y, a2x, a2y, b1x, b1y, b2x, b2y, x, y) =
        let a = ((GridX a1x, GridY a1y), (GridX a2x, GridY a2y))
        let b = ((GridX b1x, GridY b1y), (GridX b2x, GridY b2y))
        let result = checkIntersection a b
        let expected = Some (GridX x, GridY y)
        Assert.AreEqual(expected, result)
    
    [<TestCase (0, 0, 0, 2, 1, 0, 1, 2)>]
    [<TestCase (0, 0, 0, 2, 1, 1, 3, 1)>]
    member this.IntersectionNotFound (a1x, a1y, a2x, a2y, b1x, b1y, b2x, b2y) =
        let a = ((GridX a1x, GridY a1y), (GridX a2x, GridY a2y))
        let b = ((GridX b1x, GridY b1y), (GridX b2x, GridY b2y))
        let result = checkIntersection a b
        Assert.AreEqual(null, result)

    [<TestCase ("R8,U5,L5,D3", "U7,R6,D4,L4", 2)>]
    [<TestCase ("R75,D30,R83,U83,L12,D49,R71,U7,L72", "U62,R66,U55,R34,D71,R55,D58,R83", 4)>]
    [<TestCase ("R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51", "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7", 5)>]
    member this.WireIntersections (w1, w2, intersections) =
        let result = getIntersections (decode w1) (decode w2) |> List.length
        Assert.AreEqual(intersections, result)

    [<TestCase ("R8,U5,L5,D3", "U7,R6,D4,L4", 6)>]
    [<TestCase ("R75,D30,R83,U83,L12,D49,R71,U7,L72", "U62,R66,U55,R34,D71,R55,D58,R83", 159)>]
    [<TestCase ("R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51", "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7", 135)>]
    member this.WireClosestIntersections (wire1, wire2, closestIntersection) =
        let result = getNearestIntersection wire1 wire2
        Assert.AreEqual(closestIntersection, result)

    [<TestCase (2, 0, 2, 4, true)>]
    [<TestCase (0, 2, 4, 2, true)>]
    [<TestCase (1, 0, 1, 4, false)>]
    [<TestCase (0, 1, 4, 1, false)>]
    member this.CoordinateIsOnPath (path1x, path1y, path2x, path2y, expected) =
     let coordinate = (GridX 2, GridY 2)
     let path = ((GridX path1x, GridY path1y), (GridX path2x, GridY path2y))
     let result = isOnPath coordinate path
     Assert.AreEqual(result, expected)


    [<TestCase (3, 3, 40)>]
    [<TestCase (6, 5, 30)>]
    member this.DistanceToIntersection (x, y, distance) =
        let wireA = decode "R8,U5,L5,D3"
        let wireB = decode "U7,R6,D4,L4"
        let result = getTotalLoopDistance wireA wireB (GridX x, GridY y)
        Assert.AreEqual(distance, result)

    [<TestCase ("R8,U5,L5,D3", "U7,R6,D4,L4", 30)>]
    [<TestCase ("R75,D30,R83,U83,L12,D49,R71,U7,L72", "U62,R66,U55,R34,D71,R55,D58,R83", 610)>]
    [<TestCase ("R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51", "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7", 410)>]
    member this.FewestStepsToIntersection(wire1, wire2, steps) =
        let result = getShortestLoop wire1 wire2
        Assert.AreEqual(steps, result)