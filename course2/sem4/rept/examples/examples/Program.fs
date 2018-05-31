//examples units of measure, active patterns and lazy calculation

module UnitsMeasure = 
     
    open Microsoft.FSharp.Core

    [<Measure>] type cm
    [<Measure>] type m
    [<Measure>] type km
    [<Measure>] type ml = cm^3
    [<Measure>] type s // seconds
    [<Measure>] type hz = 1 / s // hertz
    [<Measure>] type miles
    [<Measure>] type hour
    
    let speed = 55.0<miles/hour>
    let length = 100.0<cm>
    let count = 1.0<1>
    let cmPerMeter : float<cm m^-1> = 100.0<cm/m>
    let x = 12.0
    let v1 = 7.9<km/s>
    let v2 = 11.2<km/s>
    let x1 = 1.2<m>
    let t1 = 1.0<s>

    let convertcm2meters (x : float<cm>) = x / 1000.0<cm/m>
    let convertCmToMeters (x : float<cm>) = x / cmPerMeter
    let convertHzToDimensionless (x : float<hz>) = x / 1.0<hz>
    // let convertHzToDimensionless (x : float<hz>) = float x
    let convertToMiles (x : float) = x * 1.0<miles>
    let height : float<cm> = LanguagePrimitives.FloatWithMeasure x
    let printFunc (x : float<cm>) = printf "%f" (x / 1.0<cm>)
    let genericSumUnits ( x : float<'u>) (y: float<'u>) = x + y
    // OK, testing units of measure was successful.
    let result1 = genericSumUnits v1 v2 
    // Error, mismatched units.
    // let result2 = genericSumUnits v1 x1

    // Define a vector together with a measure type parameter. 
    // Note the attribute applied to the type parameter. 
    type vector3D<[<Measure>] 'u> = { x : float<'u>; y : float<'u>; z : float<'u>}

    let fstVec : vector3D<m> = 
        { x = 1.0<m>; y = 0.0<m>; z = 0.0<m> }
 
    let sndVec : vector3D<m> = 
        { x = 0.0<m>; y = 1.0<m>; z = 0.0<m> }

    let thdVec : vector3D<m> = 
        { x = 1.0<m>; y = 0.0<m>; z = 1.0<m> }

    let newVec : vector3D<m/s> = 
        { x = 1.0<m/s>; y = -1.0<m/s>; z = 1.0<m/s> }

    let printFunc2 (x : float<m/s>) = 
        printf "%s" (x.ToString())  //7.9

module LazyComputations =
    let x = 10
    let result = lazy (x + 10)
    let printResult (result : Lazy<int>) = printfn "%d" (result.Force())
    //Note
    let lazyValue n = Lazy.Create (fun () ->
        let rec factorial n =
            match n with
            | 0 | 1 -> 1
            | n -> n * factorial (n - 1)
        factorial n)
    let lazyVal = lazyValue 10
    printfn "%d" (lazyVal.Force())
    //Note
    let lazyMul = 
        lazy ( 
            let value = 10 * 10
            printf "%s" "Value is "
            value)
    printfn "%A" (lazyMul.Force()) // Value is 100
    printfn "%A" (lazyMul.Force()) // 100

module ActivePatterns =
    type Num =
    | Even
    | Odd

    let isEven n =
        match (n % 2) with
        | 0 -> Even
        | _ -> Odd
    // or
    let (|Even|Odd|) input = if input % 2 = 0 then Even else Odd
    // example
    let TestNumber (input : int) =
        match input with
        | Even -> printfn "%d is even" input
        | Odd -> printfn "%d is odd" input

    TestNumber 7  // 7 is odd
    TestNumber 11 // 11 is odd
    TestNumber 32 // 32 is even
    
    //more examples
    open System.Drawing

    let (|RGB|) (col : System.Drawing.Color) =
     ( col.R, col.G, col.B )

    let (|HSB|) (col : System.Drawing.Color) =
     ( col.GetHue(), col.GetSaturation(), col.GetBrightness() )

    let printRGB (col: System.Drawing.Color) =
        match col with
        | RGB(r, g, b) -> printfn " Red: %d Green: %d Blue: %d" r g b

    let printHSB (col: System.Drawing.Color) =
        match col with
        | HSB(h, s, b) -> printfn " Hue: %f Saturation: %f Brightness: %f" h s b

    let printAll col colorString =
        printfn "%s" colorString
        printRGB col
        printHSB col

    printAll Color.Red "Red"
    printAll Color.Black "Black"
    printAll Color.White "White"
    printAll Color.Gray "Gray"
    printAll Color.BlanchedAlmond "BlanchedAlmond"

    // create an active pattern
    let (|Int|_|) str =
        match System.Int32.TryParse(str) with
        | (true, int) -> Some(int)
        | _ -> None

    // create an active pattern
    let (|Bool|_|) str =
        match System.Boolean.TryParse(str) with
        | (true, bool) -> Some(bool)
        | _ -> None

    // create a function to call the patterns
    let testParse str = 
        match str with
        | Int i -> printfn "'%i' is an int" i
        | Bool b -> printfn "'%b' is a bool" b
        | _ -> printfn "The value '%s' is something else" str

    // test
    testParse "12"      // '12' is an int
    testParse "true"    // 'true' is a bool
    testParse "abc"     // The value 'abc' is something else

    // create an active pattern
    open System.Text.RegularExpressions

    let (|FirstRegexGroup|_|) pattern input =
        let m = Regex.Match(input, pattern) 
        if (m.Success) then Some m.Groups.[1].Value else None
    // create a function to call the pattern
    let testRegex str = 
        match str with
        | FirstRegexGroup "http://(.*?)/(.*)" host -> 
            printfn "The value is a url and the host is %s" host
        | FirstRegexGroup ".*?@(.*)" host -> 
            printfn "The value is an email and the host is %s" host
        | _ -> printfn "The value '%s' is something else" str
   
    // test
    testRegex "http://google.com/test" // The value is a url and the host is google.com
    testRegex "alice@hotmail.com"   // The value is an email and the host is hotmail.com
