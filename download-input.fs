module DownloadAocInput

open System
open System.Net

type WebClientWithCookies(cookies: CookieContainer) =
    inherit WebClient()

    override _.GetWebRequest(address: Uri) =
        let req = base.GetWebRequest(address)
        match req with
        | :? HttpWebRequest as req -> req.CookieContainer <- cookies
        | _ -> ()
        req

[<EntryPoint>]
let main args =
    let year, day, cookie =
        match args with
        | [| year; day; cookie |] -> int year, int day, cookie
        | _ ->
            eprintfn "Usage: download-input.fsx <year> <day> <session cookie>"
            exit 1

    let cookies =
        let container = CookieContainer()
        container.Add(Cookie("session", cookie, Domain = "adventofcode.com"))
        container

    let uri =
        sprintf "https://adventofcode.com/%d/day/%d/input" year day

    let req = new WebClientWithCookies(cookies)

    req.DownloadFile(uri, sprintf "./advent-of-code-%d/day-%02d.input" year day)

    0
