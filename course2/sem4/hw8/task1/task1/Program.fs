open System.Net
open System.IO
open System.Text.RegularExpressions
  
let fetchPageAsync (url: string) =
    async {
        let request = WebRequest.Create(url)
        use! response = request.AsyncGetResponse()
        use stream = response.GetResponseStream()
        use reader = new StreamReader(stream)
        let html = reader.ReadToEnd()
        do printfn "%s : %d characters" url html.Length
        return html
    }

let getRefFromHref (href: string) = 
    let spl = href.Split('\"')
    spl.[1]

let getAllRefPage url =
    async {
        let! myPage = fetchPageAsync url
        let matches = Regex.Matches(myPage, @"<a href=""http://.+?"">")
        let asyncList = [for href in matches -> 
                            href.Value
                            |> getRefFromHref 
                            |> fetchPageAsync ]
        let! result = Async.Parallel asyncList
        return result
        } |> Async.RunSynchronously |> ignore

getAllRefPage "http://websharper.com/"


