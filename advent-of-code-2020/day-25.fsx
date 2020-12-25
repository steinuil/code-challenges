#load "monke.fs"


let input =
    "day-25.input"
    |> Monke.IO.readLines
    |> Seq.map int64


let transform subjectNumber v = (v * subjectNumber) % 20201227L


let loopTransform loopSize subjectNumber =
    let rec loop i v =
        if i = 0L
        then v
        else loop (i - 1L) (transform subjectNumber v)

    loop loopSize 1L


let handshake cardLoopSize doorLoopSize =
    let doorPublicKey = loopTransform doorLoopSize 7L

    let encryptionKey = loopTransform cardLoopSize doorPublicKey
    encryptionKey



let findLoopSize subjectNumber publicKey =
    let rec find i k =
        let k = transform subjectNumber k
        if k = publicKey then i else find (i + 1L) k

    find 1L 1L


let findEncryptionKey (inp: seq<int64>) =
    let cardLoopSize = findLoopSize 7L (Seq.item 0 inp)
    let doorLoopSize = findLoopSize 7L (Seq.item 1 inp)

    handshake cardLoopSize doorLoopSize


17807724L
|> findLoopSize 7L
|> printfn "Example 1: %d"


findEncryptionKey input |> printfn "Part One: %d"
