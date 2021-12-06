module Extensions

[<RequireQualifiedAccess>]
module Array =
    let split (separators: #seq<_>) arr =
        let result = ResizeArray()
        let current = ResizeArray()

        for item in arr do
            if separators |> Seq.contains item
            then 
                result.Add (current.ToArray())
                current.Clear()
            else current.Add item

        if current.Count > 0 
        then result.Add (current.ToArray())
        result.ToArray()
