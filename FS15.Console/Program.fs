namespace Search

[<AutoOpen>]
module Algorithm =

  type Node<'s,'a> = Node of 's * (('a * Node<'s,'a>) option)
  type Result<'s,'a> = Result of ('s -> 'a -> 's)
  type Actions<'s,'a> = Actions of ('s -> 'a list)
  type GoalTest<'s> = GoalTest of ('s -> bool)
  type Frontier<'s,'a> = Frontier of Node<'s,'a> list
    with
    static member push nodes (Frontier fs) = Frontier (nodes@fs)
    static member pop (Frontier fs) =
      match fs with
      | [] -> failwith "Cannot pop from empty frontier"
      | x::xs -> x, Frontier xs
    static member empty (Frontier fs) = fs = []

  let solve (initial) (Result result) (Actions actions) (GoalTest goalTest) =
    let root = Node(initial, None)
    let frontier = Frontier [root]
    let emptyExploredSet : Set<'s> = Set.empty
    let rec solve' frontier explored =
      if frontier |> Frontier.empty then
        None
      else
        let (currentNode, restFrontier) = Frontier.pop frontier
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
          solve' (Frontier.push reachableNodes restFrontier) explored'
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
