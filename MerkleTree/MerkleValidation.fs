module MerkleValidation

open MerkleTree

type PathHash = PathHash of data: byte [] * isLeftNode: bool

let rec private getPathHashes (node: Tree) (leaf: Tree) =
    let (searchIndex, leafHash) =
        match leaf with
        | Node (i, hash, _, _) -> (i, hash)
        | Empty -> failwith "expected Node"

    match node with
    | Empty -> [ PathHash(Array.empty, false) ]
    | Node (_, hash, _, _) when hash = leafHash -> []
    | Node (_, _, l, r) ->
        let (leftIndex, leftHash) =
            match l with
            | Node (li, lh, _, _) -> (li, lh)
            | Empty -> failwith "expected Node"

        if (searchIndex <= leftIndex) then
            let rightHash =
                match r with
                | Node (_, rHash, _, _) -> rHash
                | Empty -> leftHash

            [ PathHash(rightHash, false) ]
            |> List.append (getPathHashes l leaf)
        else
            [ PathHash(leftHash, true) ]
            |> List.append (getPathHashes r leaf)

let getMerkleProof (merkleTree: MerkleTree) (data: byte []) =

    let getLeaf (merkleTree: MerkleTree) (nodeHash: byte []) =
        merkleTree.Leaves
        |> Seq.tryFind
            (fun x ->
                match x with
                | Node (_, hash, _, _) -> hash = nodeHash
                | _ -> false)

    let nodeOption = merkleTree.HashFunction data

    match nodeOption |> getLeaf merkleTree with
    | None -> Error $"No matching Node for data parameter {System.Text.Encoding.ASCII.GetString data}. "
    | Some (node) -> Ok(getPathHashes merkleTree.Root node)

let validateWithProof (merkleTree: MerkleTree) data (proof: Result<list<PathHash>, string>) =
    let hashFunc = merkleTree.HashFunction

    let hash2Func a (b: PathHash) =
        let (PathHash (hash, isLeftNode)) = b

        if (isLeftNode) then
            hashFunc (Array.append hash a)
        else
            hashFunc (Array.append a hash)

    let rootHash =
        match merkleTree.Root with
        | Node (_, h, _, _) -> Ok(h)
        | Empty -> Error "Expected merkle tree with non empty root"

    match (proof, rootHash) with
    | (Ok p, Ok r) ->
        let dataHash = hashFunc data
        let proofHash = p |> List.fold hash2Func dataHash

        if r = proofHash then true else false
    | _ -> false

let validateData (merkleTree: MerkleTree) data =
    let proof = getMerkleProof merkleTree data
    validateWithProof merkleTree data proof
