namespace hw7

module Task1 =
    type RoundingBuilder(rnd: int) =
        member this.Bind(x, f) = f x
        member this.Return(x: float) =
            let y = System.Math.Round (x, rnd)
            y
    
    let rounding a = new RoundingBuilder(a)

module Task2 =
    open System
    
    type CalculateStringBuilder() =
        member this.Bind(str, f) =
            let mutable result = 0
            match Int32.TryParse(str, &result) with
            | true -> result |> f
            | false -> None

        member this.Return(x) =
             Some x

    let calculate() = new CalculateStringBuilder()    
