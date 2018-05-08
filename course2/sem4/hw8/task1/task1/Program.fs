open System.Net
open System.IO
open System.Text.RegularExpressions
  
let fetchPageAsync url =
    async {
        do printfn "Creating request for %s..." url
        let request = WebRequest.Create(url)
        use! response = request.AsyncGetResponse()
        do printfn "Getting response stream for %s.." url
        use stream = response.GetResponseStream()
        do printfn "Reading response for %s.." url
        use reader = new StreamReader(stream)
        let html = reader.ReadToEnd()
        do printfn "%s  %d characters" url html.Length
        return html
    }

let getAllHref url =
    async {
        let! myPage = fetchPageAsync url
        let matches = Regex.Matches(myPage, @"<a href=""http://.+?"">")

    } |> Async.RunSynchronously

getAllHref "http://matrixcalc.org/"


