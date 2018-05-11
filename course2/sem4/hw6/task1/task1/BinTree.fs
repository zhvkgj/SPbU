namespace Hw6

open System
module BinTree =
    
    open System
    open System.Collections
    open System.Collections.Generic

    /// <summary>
    /// Abstraction of a binary tree node
    /// </summary>
    type BinaryTreeNode<'T when 'T :> IComparable> (v: 'T) =
        let mutable value = v
        member val LeftNode = null with get, set
        member val RightNode = null with get, set
        member this.Value 
            with get() =
                value
            and private set(v) =
                value <- v
        interface IComparable with
            member this.CompareTo(other) =
                this.Value.CompareTo(other)
            
    /// <summary>
    /// Abstraction of the binary tree
    /// </summary>
    type MyBinaryTree<'T when 'T :> IComparable>() =
        let mutable head = null
        let mutable count = 0
        member this.Add(value: 'T) =
            if (head = null) then 
                head = new BinaryTreeNode<'T>(value)
            else
                AddTo(head, value)

        
        