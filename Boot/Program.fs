open Domain.FuelCalculator
open Domain.IntcodeInterpreter
open Domain.Utils

[<EntryPoint>]
let main argv =
    let replaceElement2 (index:int) (newElement:'int) (lst:'int list) = replaceElement lst index newElement
    let initialMemory = "1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,1,10,19,2,9,19,23,2,23,10,27,1,6,27,31,1,31,6,35,2,35,10,39,1,39,5,43,2,6,43,47,2,47,10,51,1,51,6,55,1,55,6,59,1,9,59,63,1,63,9,67,1,67,6,71,2,71,13,75,1,75,5,79,1,79,9,83,2,6,83,87,1,87,5,91,2,6,91,95,1,95,9,99,2,6,99,103,1,5,103,107,1,6,107,111,1,111,10,115,2,115,13,119,1,119,6,123,1,123,2,127,1,127,5,0,99,2,14,0,0"
    let result = 
        initialMemory 
        |> decode 
        |> setNoun 12
        |> flatMap (setVerb 2)
        |> flatMap start
    let address0 noun verb =
        let res = 
            initialMemory 
            |> decode 
            |> setNoun noun
            |> flatMap (setVerb verb)
            |> flatMap start
        (getElement res.Value 0).Value
    
    let simulation =
        let rec simulate noun verb (map:Map<int, int*int>) =
            let res = address0 noun verb
            if noun = 99 && verb = 99 then map.Add (res, (noun, verb))
            else if verb = 99 then simulate (noun+1) 0 (map.Add (res, (noun, verb)))
            else simulate noun (verb+1) (map.Add (res, (noun, verb)))
        simulate 0 0 Map.empty

    let aimedOutput = 19690720
    let (noun, verb) = simulation.Item aimedOutput
    // Day 2 - part 1 \\
    printfn "Day 2 - part 2: In order to get %d as output on 'address 0', %d is needed as noun and %d is needed as verb" aimedOutput noun verb
    // Day 2 - part 1 \\
    printfn "Day 2 - part 1: After running the program '%d' is on 'address 0'." (getElement result.Value 0).Value

    let massComponents = [
        143843
        144558
        116244
        117881
        138945
        97456
        58650
        117108
        92832
        65454
        134030
        96221
        139052
        66021
        95159
        60921
        97715
        55947
        64516
        112463
        109063
        117606
        74592
        53954
        57131
        109084
        94829
        146641
        90803
        82129
        51563
        135004
        62815
        51944
        98078
        87360
        133867
        130745
        118620
        67247
        84032
        76912
        96189
        146929
        50217
        129260
        52994
        148798
        62244
        83613
        93746
        93226
        84804
        70442
        74736
        96469
        124597
        128177
        60771
        92722
        125505
        58898
        62380
        107386
        126861
        50007
        107730
        130300
        85507
        147266
        57945
        88357
        79129
        147921
        126996
        95815
        91783
        125716
        94350
        131258
        90897
        107305
        71579
        104766
        104400
        148072
        95196
        104221
        96059
        137814
        104159
        51053
        73143
        137363
        129416
        123035
        82901
        67221
        77263
        73447 ]
    let fuel = List.map Mass massComponents |> totalFuel
    let value (Fuel f) = f
    // Day 1 - part 2 \\
    printfn "Day 1 - part 2: Santa requires %d fuel for his spacecraft" (value fuel)
    // Day 1 - part 1 \\
    printfn "Day 1 - part 1: Santa requires 3252897 fuel for his spacecraft"

    0