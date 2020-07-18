open Domain.FuelCalculator
open Domain.IntcodeInterpreter
open Domain.Utils
open Domain.Wires

[<EntryPoint>]
let main argv =

    let wireA = "R990,U944,L921,U993,L64,U29,R899,D406,R841,U716,L32,U658,L830,D481,L441,U491,L687,D847,L459,U920,R165,U224,L896,D868,L191,U877,L909,U467,R798,U132,R234,U49,R484,U911,R108,D308,R867,U350,L404,U107,L84,U668,R850,U470,L182,U93,R284,U999,L664,U110,R650,D189,R540,D112,R794,U999,R871,D829,L549,U988,R654,D411,R323,D774,R529,U275,L909,U936,R122,D922,L331,U813,L748,U770,R511,D892,L770,U318,R661,U823,R210,D393,L694,U929,L76,D619,R395,U651,R526,U145,R851,U112,R73,D89,R17,U929,R807,U87,R764,D158,L820,U803,L785,D205,L828,D271,L839,D482,L797,U338,R322,D633,L292,U16,R627,U19,R548,U516,L384,U83,R256,U937,R404,U322,R671,D543,L412,U446,R238,U246,L794,D750,L34,U317,L994,U874,L247,D20,R491,U834,L498,D987,R2,U175,R452,U168,R495,D183,R201,U532,L192,U984,L566,D471,L704,D2,L776,U5,R911,U308,R763,D555,R458,D439,L968,D78,R549,D583,R289,D355,L503,D871,L881,U516,L507,D551,R711,U702,L308,D905,L408,U932,L884,U218,L158,D562,L200,D114,R673,U448,R887,U181,R247,U329,L965,U495,L329,D162,L265,D389,R419,U435,R258,U146,R208,D184,R730,D19,L78,D886,R472,D350,R484,U392,L542,U601,L202,U974,L310,U52,L537,U597,L163,D655,R928,U269,L926,D790,L513,U441,L90,U581,L211,U871,R603,D130,L220,U459,L933,U648,R721,U642,R301,U537,L858,D777,R823,U14,R820,D218,L96,D318,L206,U280,R887,D241,L752,U828,R354,U864,R844,D872,L728,D298,L234,U695,R434,D94,R905,D592,L32,D860,R680,D197,R886,U760,L232,U916,L452,U248,R715,D773,R867,U77,R751,D36,R565,U897,R782,U931,R546,U261,R920,D296,R451,U258,L394,U965,R912,D593,L990"
    let wireB = "L994,U515,R163,U863,L343,U162,L875,D92,L483,D601,R79,D761,L389,U167,L145,U145,L247,U886,R61,U820,L584,D239,R402,U805,R956,U126,R615,D322,R431,D460,R397,D511,R805,D177,L778,U296,R599,U759,R40,U1,L422,U751,R94,U401,R504,U940,L564,U24,R595,U944,R815,U672,R787,D854,R579,D604,L62,U670,L516,U199,L639,D919,L485,U655,R391,U669,R772,D34,R868,D12,L108,U295,R701,D603,R493,U927,R29,D34,R499,U111,L87,U190,R884,D658,R474,D166,R921,U698,R592,U25,R710,D398,L26,U696,L432,D887,R469,U656,L428,D188,L543,D150,R160,U543,R743,U692,R618,D148,R956,U753,L175,D789,R897,U305,L137,D914,R330,D780,R744,D473,L754,U482,L975,D413,L698,U656,L177,U419,R13,D827,L67,D800,R369,U97,L34,D588,L41,D760,L164,U224,L921,D311,R489,U956,R277,U180,R724,U748,R785,U826,L426,D957,R303,U16,L729,U224,L712,U43,L280,D648,R987,D941,R154,D581,R876,U615,L480,D103,R636,D276,R948,U89,R434,D212,R837,D295,R532,D390,R374,D926,R911,D110,R258,U83,L955,U747,L925,D366,R571,U241,R628,D344,R919,U117,R337,D683,L720,U261,L124,D545,R979,D601,L906,D324,R441,U678,L978,U744,L472,D217,R799,U740,L77,U964,L278,U497,R441,U21,L37,U319,L24,D211,L44,U459,R35,D609,R900,D538,R397,D776,R629,D860,R519,D340,R168,U603,R46,U889,R897,D442,R997,U705,L82,D963,R941,U701,L347,D824,R269,U891,L569,D558,L867,U145,R121,D369,R542,U227,L198,U863,L755,U273,L734,D233,R578,U67,L821,U600,L203,D234,R695,U819,L639,D700,R295,D129,L612,U157,R212,U536,L968,U562,L999,D391,L231,U262,R334,D967,R463,U748,R842,D500,R860,U856,R263,D633,R460,D337,L880,U146,R910"
    let distance = getNearestIntersection wireA wireB
    let steps = getShortestLoop wireA wireB
    // Day 3 - part 2 \\
    printfn "Day 3 - part 2: The fewest amount of steps required to reach an intersection is %d" steps
    // Day 3 - part 1 \\
    printfn "Day 3 - part 1: The Manhattan distance to the closest intersection is %d" distance

    let initialMemory = "1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,1,10,19,2,9,19,23,2,23,10,27,1,6,27,31,1,31,6,35,2,35,10,39,1,39,5,43,2,6,43,47,2,47,10,51,1,51,6,55,1,55,6,59,1,9,59,63,1,63,9,67,1,67,6,71,2,71,13,75,1,75,5,79,1,79,9,83,2,6,83,87,1,87,5,91,2,6,91,95,1,95,9,99,2,6,99,103,1,5,103,107,1,6,107,111,1,111,10,115,2,115,13,119,1,119,6,123,1,123,2,127,1,127,5,0,99,2,14,0,0"
    let result = 
        initialMemory 
        |> Domain.IntcodeInterpreter.decode 
        |> setNoun 12
        |> flatMap (setVerb 2)
        |> flatMap start
    let address0 noun verb =
        let res = 
            initialMemory 
            |> Domain.IntcodeInterpreter.decode 
            |> setNoun noun
            |> flatMap (setVerb verb)
            |> flatMap start
        (getElement res.Value 0).Value
    
    let simulation =
        let rec simulate noun verb (map:Map<int,int*int>) =
            let res = address0 noun verb
            if noun = 99 && verb = 99 then map.Add (res, (noun, verb))
            else if verb = 99 then simulate (noun+1) 0 (map.Add (res, (noun, verb)))
            else simulate noun (verb+1) (map.Add (res, (noun, verb)))
        simulate 0 0 Map.empty

    let aimedOutput = 19690720
    let (noun, verb) = simulation.Item aimedOutput
    // Day 2 - part 2 \\
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