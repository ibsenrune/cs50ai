namespace Search

[<AutoOpen>]
module Datastructures =
  open System.Collections.Generic
  type IFrontier<'a> =
    abstract member push : 'a -> unit
    abstract member pop : unit -> 'a
    abstract member isEmpty : unit -> bool

  type StackFrontier<'a>() =
    let stack = new Stack<'a>()
    interface IFrontier<'a> with
      member this.push x = stack.Push(x)
      member this.pop () = stack.Pop()
      member this.isEmpty () = stack.Count = 0

  type QueueFrontier<'a>() =
    let queue = new Queue<'a>()
    interface IFrontier<'a> with
      member this.push x = queue.Enqueue(x)
      member this.pop () = queue.Dequeue()
      member this.isEmpty() = queue.Count = 0

[<AutoOpen>]
module Algorithm =

  type Node<'s,'a> = Node of 's * (('a * Node<'s,'a>) option)
  type Result<'s,'a> = Result of ('s -> 'a -> 's)
  type Actions<'s,'a> = Actions of ('s -> 'a list)
  type GoalTest<'s> = GoalTest of ('s -> bool)
  type SearchType = BreadthFirstSearch | DepthFirstSearch

  let private frontierFactory<'a> : SearchType -> Datastructures.IFrontier<'a> = function
   | BreadthFirstSearch -> Datastructures.QueueFrontier()
   | DepthFirstSearch -> Datastructures.StackFrontier()

  let solve (t : SearchType) (initial) (Result result) (Actions actions) (GoalTest goalTest) =
    let root = Node(initial, None)
    let frontier = frontierFactory t
    frontier.push root
    let emptyExploredSet : Set<'s> = Set.empty
    let rec solve' (frontier : Datastructures.IFrontier<'a>) explored =
      if frontier.isEmpty() then
        None
      else
        let currentNode = frontier.pop()
        let (Node (currentState,_)) = currentNode
        if goalTest currentState then
          Some currentNode
        else
          let explored' = Set.add currentState explored
          let reachableNodes =
            currentState
            |> actions
            |> List.map (fun a -> Node (result currentState a, Some(a, currentNode)))
            |> List.filter (fun (Node (s,_)) -> not <| Set.contains s explored')
          List.iter frontier.push reachableNodes
          solve' frontier explored'
    solve' frontier emptyExploredSet


[<AutoOpen>]
module Game15 =
  let rec swap i ss = 
    match i, ss with
    | n, _ when n < 0 -> failwith (sprintf "Cannot swap at negative index %i" i)
    | _, [] -> failwith (sprintf "Cannot swap element %i in empty list" i)
    | _, [x] -> failwith (sprintf "Cannot swap element %i in %A" i ss)
    | 0, x::y::rest -> y::x::rest
    | n, x::y::rest -> x::(swap (n-1) (y::rest))
  
  let rec isSortedAsc = function
  | [] -> true
  | [x] -> true
  | x::y::rest -> x <= y && isSortedAsc (y::rest)


  let possibleSwaps = function
  | [] -> []
  | [_] -> []
  | ss -> [ 0 .. (List.length ss) - 2]

  let actions = Actions possibleSwaps
  let test = GoalTest isSortedAsc
  let result = Result (fun ss i -> swap i ss)
