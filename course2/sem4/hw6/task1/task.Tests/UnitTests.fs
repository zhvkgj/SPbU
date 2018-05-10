(*namespace hw6.Tests

open hw6
open Task1
open NUnit.Framework

module Task1Test =
    let random = new System.Random()
    let computers = 
        [Computer("linux", false, random); 
         Computer("windows", true, random); 
         Computer("masOS", false, random)]
    
    let matrix = 
        [[false; true; true];
         [true; false; true;]
         [true; true; false]]

    let network = new Network(computers, matrix)
    
    [<Test>]
    let ``Test1``() =
        let*)