module Tests

open NUnit.Framework
open FsUnit
open hw5
open Task3

[<Test>]
let ``Simple test1 dataToStrArr function`` () =
    dataToStringArray << List.rev <| [("Ariel", "88005553535"); ("Almagusam", "7355608"); ("Domestos", "3990")]
    |> should equal [|"Ariel 88005553535"; "Almagusam 7355608"; "Domestos 3990"|]
    
[<Test>]
let ``Simple test2 dataToStrArr function`` () =
    dataToStringArray << List.rev <| [("Zaghraba", "2218301"); ("Abreghi", "+792184392481"); ("Zuhra", "+799941020401")]
    |> should equal [|"Zaghraba 2218301"; "Abreghi +792184392481"; "Zuhra +799941020401"|]

[<Test>]
let ``Simple test1 stringsToData function`` () =
    stringsToData << List.rev <| ["Zaghraba 2218301"; "Abreghi +792184392481" ; "Zuhra +799941020401"]
    |> should equal [("Zaghraba", "2218301"); ("Abreghi", "+792184392481"); ("Zuhra", "+799941020401")]

[<Test>]
let ``Simple test2 stringsToData function`` () =
    stringsToData << List.rev <| ["Ariel 88005553535"; "Almagusam 7355608"; "Domestos 3990"]
    |> should equal [("Ariel", "88005553535"); ("Almagusam", "7355608"); ("Domestos", "3990")]

[<Test>]
let ``Simple test1 readFromFile function`` () =
    readFromFile @"D:\Admin\Desktop\SPbU\course2\sem4\hw5\task3\phone_book.txt" 
    |> should equal [("Rufus", "399100"); ("Samuel", "3213213477"); ("Elena", "0319393"); ("Anna", "2194881498")]

[<Test>]
let ``Simple test1 searchByName function`` () =
    searchByName "Zaghraba" [("Zaghraba", "2218301"); ("Abreghi", "+792184392481"); ("Zuhra", "+799941020401")]
    |> should equal "Call number: 2218301"

[<Test>]
let ``Simple test2 searchByName function`` () =
    searchByName "Elena" [("Rufus", "399100"); ("Samuel", "3213213477"); ("Elena", "0319393"); ("Anna", "2194881498")]
    |> should equal "Call number: 0319393"

[<Test>]
let ``Simple test3 searchByName function`` () =
    searchByName "Almagusam" [("Rufus", "399100"); ("Samuel", "3213213477"); ("Elena", "0319393"); ("Anna", "2194881498")]
    |> should equal "Contact doesn't exist!"

[<Test>]
let ``Simple test1 searchByCallNumber function`` () =
    searchByCallNumber "+799941020401" [("Zaghraba", "2218301"); ("Abreghi", "+792184392481"); ("Zuhra", "+799941020401")]
    |> should equal "Name: Zuhra"

[<Test>]
let ``Simple test2 searchByCallNumber function`` () =
    searchByCallNumber "399100" [("Rufus", "399100"); ("Samuel", "3213213477"); ("Elena", "0319393"); ("Anna", "2194881498")]
    |> should equal "Name: Rufus"

[<Test>]
let ``Simple test3 searchByCallNumber function`` () =
    searchByCallNumber "919484818" [("Rufus", "399100"); ("Samuel", "3213213477"); ("Elena", "0319393"); ("Anna", "2194881498")]
    |> should equal "Contact doesn't exist!"