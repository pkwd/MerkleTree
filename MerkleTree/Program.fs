open MerkleTree
open MerkleValidation

let noHash byteArray : byte [] = byteArray

let SHA256 (hash: byte []) =
    System.Security.Cryptography.SHA256.HashData(hash)

let toByte (msg: string) : byte [] =
    System.Text.Encoding.ASCII.GetBytes(msg)

// Creating empty MerkleTree and adding nodes
let emptyTree = MerkleTree(Seq.empty, noHash)
emptyTree.PrintTree


let createAndPrintTree tree byte =
    let newTree = addNode tree byte
    newTree.PrintTree
    newTree

let seqTree =
    Seq.fold createAndPrintTree emptyTree (seq { for i in 0 .. 16 -> toByte (i.ToString()) })

//Creating MerkleTree from sequence of data and validation
let data =
    seq { for i in 0 .. 10 -> toByte (i.ToString()) }

let multiDataTree = createMerkleTree data noHash
multiDataTree.PrintTree

let multiDataTreeProof =
    toByte "10" |> getMerkleProof multiDataTree

let multiDataTreeProofStrings =
    multiDataTreeProof
    |> Result.map
        (fun r ->
            r
            |> List.map
                (fun ph ->
                    let (PathHash (h, b)) = ph
                    h)
            |> List.map System.Text.Encoding.ASCII.GetString)

printf "Printing recieved proof nodes for node 10: "

match multiDataTreeProofStrings with
| Ok result -> result |> List.iter (fun x -> printf "'%s' " x)
| Error msg -> printf "%s" msg

printfn ""

printfn "Validating data point '10' in multiDataTree. Valid: %b" (validateData multiDataTree (toByte "10"))

let multiDataTree' = addNode multiDataTree (toByte "11")

printfn
    "Validating data point '5' in multiDataTree' with multiDataTreeProof. Valid: %b"
    (validateWithProof multiDataTree' (toByte "5") multiDataTreeProof)

try
    printfn "Validating data point '20' in multiDataTree. Valid: %b" (validateData multiDataTree (toByte "20"))
with
| Failure (msg) -> printfn "%s" msg


//Creating tree from sequence of data and validation - with real hashing
let multiDataHashedTree = createMerkleTree data SHA256
multiDataHashedTree.PrintTree

printfn "Validating data point '10' in multiDataHashedTree. Valid: %b" (validateData multiDataHashedTree (toByte "10"))

try
    printfn
        "Validating data point '20' in multiDataHashedTree. Valid: %b"
        (validateData multiDataHashedTree (toByte "20"))
with
| Failure (msg) -> printfn "%s" msg
