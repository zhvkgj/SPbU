module Tests

open NUnit.Framework
open FsUnit
open hw5
open Task3

[<Test>]
let ``Simple test1 dataToStrArr function`` () =
    dataToStrArr << List.rev <| [("Ariel", "88005553535"); ("Almagusam", "7355608"); ("Domestos", "3990")]
    |> should equal [|"Ariel 88005553535"; "Almagusam 7355608"; "Domestos 3990"|]
    
[<Test>]
let ``Simple test2 dataToStrArr function`` () =
    dataToStrArr << List.rev <| [("Zaghraba", "2218301"); ("Abreghi", "+792184392481"); ("Zuhra", "+799941020401")]
    |> should equal [|"Zaghraba 2218301"; "Abreghi +792184392481"; "Zuhra +799941020401"|]

[<Test>]
let ``Simple test1 stringsToData function`` () =
    stringsToData << List.rev <| ["Zaghraba 2218301"; "Abreghi +792184392481" ; "Zuhra +799941020401"]
    |> should equal [("Zaghraba", "2218301"); ("Abreghi", "+792184392481"); ("Zuhra", "+799941020401")]

[<Test>]
let ``Simple test2 stringsToData function`` () =
    stringsToData << List.rev <| ["Ariel 88005553535"; "Almagusam 7355608"; "Domestos 3990"]
    |> should equal [("Ariel", "88005553535"); ("Almagusam", "7355608"); ("Domestos", "3990")]

(*Some question
[<Test>]
let ``Simple test1 readFromFile function`` () =
    readFromFile "D:\Admin\Desktop\SPbU\course2\sem4\hw5\task3\phone_book.txt" 
    |> should equal [("Anna", "2194881498"); ("Elena", "0319393"); ("Samuel", "3213213477"); ("Rufus", "399100")]*)
