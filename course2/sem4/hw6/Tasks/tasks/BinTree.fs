namespace Hw6

module BinTree =
    
    open System.Collections
    open System.Collections.Generic

    /// <summary>
    /// Abstraction of a binary tree node
    /// </summary>
    type BinaryTreeNode<'T> = 
        | BinaryTreeNode of 'T * BinaryTreeNode<'T> * BinaryTreeNode<'T>
        | Null
 
    /// <summary>
    /// Abstraction of a binary tree
    /// </summary>
    type MyBinaryTree<'T when 'T : comparison>() =
        
        /// <summary>
        /// Root of the binary tree
        /// </summary>
        let mutable head = Null

        /// <summary>
        /// Count of elements in the binary tree
        /// </summary>
        let mutable count = 0

        // на самом деле хотел посмотреть, будет ли на F# работать что-то такое
        // и использовать это в методе Remove(), однако в контексте F# это похоже оказалось невозможным
        /// <summary>
        /// Find a necessary node and its' parent
        /// </summary>
        /// <param name="value">Necessary value</param>
        let FindWithParent(value: 'T) =
            let rec FindWithParentIn current value sought parent =
                match current with
                | Null -> (sought, parent)
                | BinaryTreeNode(v, l, r) ->
                    if (v = value) then 
                        FindWithParentIn Null value current parent
                    elif (v > value) then
                        FindWithParentIn l value Null current
                    else
                        FindWithParentIn r value Null current
            FindWithParentIn head value Null Null
                
        /// <summary>
        /// Getter of count
        /// </summary>
        member this.Count
            with get() = count

        /// <summary>
        /// Add a new node to current tree
        /// </summary>
        /// <param name="value">Value to add</param>
        member this.Add(value: 'T) =
            // recursive function for adding an element with a current value
            let rec addTo subTree value =
                match subTree with 
                | Null -> 
                    BinaryTreeNode(value, Null, Null)
                | BinaryTreeNode(v, l, r) ->
                    if (v > value) then
                        BinaryTreeNode(v, addTo l value, r)
                    else 
                        BinaryTreeNode(v, l, addTo r value)
            head <- addTo head value
            count <- count + 1
        
        /// <summary>
        /// Remote a node with current value
        /// </summary>
        /// <param name="value">Remote value</param>
        /// <returns>Return true - the value was deleted, false - not</returns>
        member this.Remove(value: 'T) =
            // find min value in right sub-tree
            let rec findMinAtTree rightTr =
                match rightTr with
                | Null -> failwith "Error, right sub-tree cannot be a Null!"
                | BinaryTreeNode(v, Null, Null) ->
                    v
                | BinaryTreeNode(_, l, _) ->
                    findMinAtTree l
            // recursively removes the value
            let rec removeFrom subTree (value: 'T) =
                match subTree with
                | Null -> Null, false
                | BinaryTreeNode(v, l, r) ->
                    if (v = value) then
                        count <- count - 1
                        match r with
                        | Null -> l, true
                        | BinaryTreeNode(v1, Null, r1) ->
                            BinaryTreeNode(v1, l, r1), true
                        | BinaryTreeNode(v1, l1, r1) ->
                            let minAtRight = findMinAtTree l1
                            BinaryTreeNode(minAtRight, l, fst <| removeFrom r minAtRight), true
                    elif (v > value) then
                        let tempResult = removeFrom l value
                        BinaryTreeNode(v, fst <| tempResult, r), snd <| tempResult
                    else
                        let tempResult = removeFrom r value
                        BinaryTreeNode(v, l, fst <| tempResult), snd <| tempResult
            
            let result = removeFrom head value
            head <- fst <| result
            snd <| result

        /// <summary>
        /// Converts the binary tree to list
        /// </summary>
        member this.ToList() =
            let rec binTreeToList binTr ls = 
                match binTr with
                | Null -> ls
                | BinaryTreeNode(v, l, r) ->
                    let rightTree = binTreeToList r ls
                    let leftTree = binTreeToList l ls
                    List.append leftTree (v :: rightTree)
            binTreeToList head List.Empty
            
        /// <summary>
        /// Check the availability
        /// </summary>
        /// <param name="value">Request value</param>
        member this.Contains(value: 'T) =
            FindWithParent value
            |> fst
            |> (fun n -> n <> Null)
        
        /// <summary>
        /// Clear the binary tree
        /// </summary>
        member this.Clear() =
            head <- Null
            count <- 0
        

        // IEnumerable interface
        interface IEnumerable<'T> with
            member this.GetEnumerator(): IEnumerator<'T> =
                let ls = this.ToList() :> seq<'T>
                ls.GetEnumerator()

            member this.GetEnumerator(): System.Collections.IEnumerator = 
                let ls = this.ToList() :> seq<'T>
                ls.GetEnumerator() :> IEnumerator
    