#r @"C:\Users\Gregor\.nuget\packages\aether\8.2.0\lib\netstandard1.6\Aether.dll"
#load @"c:\dev\code\FSharp.FGL\src\FSharp.FGL\Graph.fs"
#load @"c:\dev\code\FSharp.FGL\src\FSharp.FGL\Models.fs"
#load @"c:\dev\code\FSharp.FGL\src\FSharp.FGL\Undirected.fs"
#load @"c:\dev\code\FSharp.FGL\src\FSharp.FGL\Directed.fs"

open FSharp.FGL

let g =
    Graph.empty
    |> Undirected.Vertices.addMany [
        (1, "One")
        (2, "Two")
        (3, "Three")
        (4, "Four")
        (5, "Five")
        (6, "Six")
        (7, "Seven")]
    |> Undirected.Edges.addMany [
        (1, 4, "One Four")
        (1, 6, "One Six")
        (1, 5, "One Five")
        (2, 5, "Two Five")
        (2, 7, "Two Seven")
        (2, 6, "Two Six")
        (3, 7, "Three Seven")
        (3, 6, "Three Six")
        (4, 1, "Four One")
        (4, 7, "Four Seven")]

Undirected.Vertices.count g

let (ctx, g') = Graph.decompose 1 g

let g'' = Undirected.Vertices.map (fun v l -> String.length l) g


let dfs (nodes : 'Vertex list) (graph : Graph<'Vertex, 'Label, 'Edge>) : 'Vertex list =
    let rec dfs (nodes : 'Vertex list) (graph : Graph<'Vertex, 'Label, 'Edge>) : 'Vertex list =
        match nodes, graph with
        | ([], _) -> []
        | (_, g) when Graph.isEmpty g -> []
        | (n::ns, g) ->
            match Graph.tryDecompose n g with
            | (Some c, g') ->
                let neighbours = Undirected.Vertices.neighbours c
                n::(dfs (neighbours @ ns) g')
                // n::(dfs (Undirected.Vertices.neighbours c)::ns g')
            | None, _ -> dfs ns g

    dfs nodes graph


let ns = dfs [3] g