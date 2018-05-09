namespace hw7

module Task1 =
    type RoundingBuilder(rnd: int) =
        member this.Bind(x: float, f) = 
            f <| System.Math.Round (x, rnd)
        member this.Return(x: float) =
            System.Math.Round (x, rnd)
    
    let rounding a = new RoundingBuilder(a)

module Task2 =
    open System
    
    type CalculateStringBuilder() =
        member this.Bind(str, f) =
            match Int32.TryParse str with
            | true, result -> result |> f
            | false, _ -> None

        member this.Return(x) =
             Some x

    let calculate() = new CalculateStringBuilder()    
