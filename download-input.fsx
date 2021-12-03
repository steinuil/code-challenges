#if !INTERACTIVE
module DownloadAocInput
#endif

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

#if !INTERACTIVE
[<EntryPoint>]
#endif
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

    if not (IO.Directory.Exists(sprintf "./advent-of-code-%d" year)) then
        IO.Directory.CreateDirectory(sprintf "./advent-of-code-%d" year)
        |> ignore

    let uri =
        sprintf "https://adventofcode.com/%d/day/%d/input" year day

    let req = new WebClientWithCookies(cookies)

    req.DownloadFile(uri, sprintf "./advent-of-code-%d/day_%02d.input" year day)

    0

#if INTERACTIVE
do
    let args =
        Array.sub fsi.CommandLineArgs 1 (fsi.CommandLineArgs.Length - 1)

    main args |> ignore
#endif
