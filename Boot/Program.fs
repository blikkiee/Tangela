﻿// Learn more about F# at http://fsharp.org

open System
open Domain.FuelCalculator

[<EntryPoint>]
let main argv =
    
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
    printfn "Day 1 - part 2: Santa requres %d fuel for his spacecraft" (value fuel)
    // Day 1 - part 1 \\
    printfn "Day 1 - part 1: Santa requres 3252897 fuel for his spacecraft"

    0