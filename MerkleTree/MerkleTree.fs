module MerkleTree

open System

type Tree =
    | Empty
    | Node of index: int * value: byte [] * left: Tree * right: Tree

type MerkleTree(leaves: seq<Tree>, hashFunction: byte [] -> byte [], root: Tree) =

    new(leaves: seq<Tree>, hashFunction: byte [] -> byte []) =
        let rec buildTree (nodes: seq<Tree>) : seq<Tree> =
            match nodes with
            | singleOrEmpty when Seq.length singleOrEmpty <= 1 -> singleOrEmpty
            | ns ->
                let pairedNodes =
                    ns
                    |> Seq.chunkBySize 2
                    |> Seq.map
                        (fun m ->
                            match m with
                            | [| Node (i1, a, b, c); Node (i2, d, e, f) |] ->
                                Node(i2, hashFunction (d |> Array.append a), Node(i1, a, b, c), Node(i2, d, e, f))
                            | [| Node (i1, a, b, c) |] ->
                                Node(i1, hashFunction (a |> Array.append a), Node(i1, a, b, c), Empty)
                            | _ -> Empty)

                buildTree pairedNodes

        let newRoot =
            if leaves |> Seq.isEmpty then
                Empty
            else
                buildTree (leaves) |> Seq.head

        MerkleTree(leaves, hashFunction, newRoot)

    member this.Leaves = leaves

    member this.HashFunction = hashFunction

    member this.LeavesCount = this.Leaves |> Seq.length

    member this.Root = root

    member this.Height =
        if this.LeavesCount = 0 then
            0
        else
            this.LeavesCount
            |> System.Math.Log2
            |> System.Math.Ceiling
            |> int32

    member this.PrintTree =

        let rec loopByLevel (node: Tree, level: int, whitespace: string) =
            let (treeString, left, right) =
                match node with
                | Empty -> ("EMPTY", Empty, Empty)
                | Node (_, hash, left', right') -> (System.Text.Encoding.ASCII.GetString(hash), left', right')

            if level <= 0 then
                printf "%s%s" whitespace treeString
            else
                match left with
                | Empty -> ()
                | node ->
                    let nextLevel = level - 1
                    loopByLevel (node, nextLevel, whitespace)

                match right with
                | Empty -> ()
                | node ->
                    let nextLevel = level - 1
                    loopByLevel (node, nextLevel, whitespace)

        let printNodesByDepth (tree: Tree) : Unit =
            for elem in 0 .. this.Height do
                let whitespace =
                    " "
                    |> String.replicate ((this.Height + 1 - elem) * 5)

                loopByLevel (tree, elem, whitespace)
                printfn ""

        printfn "Printing tree:"
        printNodesByDepth (this.Root)

let createMerkleTree dataPoints hashFunction =
    let nodes =
        dataPoints
        |> Seq.map hashFunction
        |> Seq.mapi (fun i h -> Node(i, h, Empty, Empty))

    MerkleTree(nodes, hashFunction)

let rec private addLeafAndDepth hashFunction currentLevel oldRoot (previousNode: Tree) =
    let (index, hash) =
        match previousNode with
        | Node (i, h, _, _) -> (i, h)
        | _ -> failwith "Error: expecting node"

    if currentLevel = 0 then
        match oldRoot with
        | Empty -> previousNode
        | Node (_, rh, _, _) -> Node(index, hashFunction (Array.append rh hash), oldRoot, previousNode)
    else
        addLeafAndDepth
            hashFunction
            (currentLevel - 1)
            oldRoot
            (Node(index, hashFunction (Array.append hash hash), previousNode, Empty))

let rec private insert parentNode leafNode index hashFunction level (path: string) =

    let getHash nonEmptyTree = 
        match nonEmptyTree with
        | Node (_, hash, _, _) -> hash
        | Empty -> failwith "Error: expecting node"

    let leafHash = getHash leafNode

    match parentNode with
    | Empty -> leafNode
    | Node(_, h, Empty, Empty) ->
        if level = 0 then 
            leafNode
        else
            let newNode = insert (Node(index, h, Empty, Empty)) leafNode index hashFunction (level - 1) path.[1..]
            let newHash = getHash newNode
            Node(index, hashFunction (Array.append newHash newHash), newNode, Empty)

    | Node (_, _, Node (li, lh, Empty, Empty), Empty) ->
        Node(index, hashFunction (Array.append lh leafHash), Node(li, lh, Empty, Empty), leafNode)

    | Node(_, _, Node (li, lh, ll, lr), Node(ri, rh, rl, rr)) ->
        let newNode = insert (Node(ri, rh, rl, rr)) leafNode index hashFunction (level - 1) path[1..]
        let newHash = getHash newNode    
        Node(index, hashFunction (Array.append lh newHash), Node(li, lh, ll, lr), newNode)

    | Node(_, _, Node (li, lh, ll, lr), Empty) ->
        if path[0] = '1' then
            let newNode = insert (Node (index, leafHash, Empty, Empty)) leafNode index hashFunction (level - 1) path[1..]
            let newHash = getHash newNode
            Node(index, hashFunction (Array.append lh newHash), Node(li, lh, ll, lr), newNode)
        else
           let newNode = insert (Node (li, lh, ll, lr)) leafNode index hashFunction (level - 1) path[1..]
           let newHash = getHash newNode
           Node(index, hashFunction (Array.append newHash newHash), newNode, Empty)

     | _ -> failwith "Error: provided tree has illegal state"

let addNode (merkleTree: MerkleTree) (data: byte []) =
    let rec intToBinary i =
        match i with
        | 0 | 1 -> string i
        | _ ->
            let bit = string (i % 2)
            (intToBinary (i / 2)) + bit

    let isPowerOfTwo number = number > 0 && (number &&& (number - 1) = 0)

    let index = merkleTree.LeavesCount
    let leafHash = merkleTree.HashFunction data
    let leafNode = Node(index, leafHash, Empty, Empty)

    let newLeaves =
        Seq.append merkleTree.Leaves [ leafNode ]

    let newRoot =
        if isPowerOfTwo index then
            addLeafAndDepth merkleTree.HashFunction merkleTree.Height merkleTree.Root leafNode
        else
            let path = intToBinary index
            insert merkleTree.Root leafNode index merkleTree.HashFunction merkleTree.Height path

    MerkleTree(newLeaves, merkleTree.HashFunction, newRoot)
