module List.Addendum
    exposing
        ( at
        , chunk
        , chunk_by
        , count
        , dedup
        , dedup_by
        , drop_every
        , drop_while
        , each
        , fetch
        , find
        , find_index
        )

{-|

@docs at, chunk, chunk_by, count, dedup, dedup_by, drop_every, drop_while, each, fetch, find, find_index

-}


{-| -}
at : List a -> Int -> a -> a
at list index default =
    Maybe.withDefault default <| fetch list index


{-| Chunks a List into a list of lists containing `count` elements each, where
    each new chunk starts `step` elements into the List. `step` is optional and,
    if not provided, defaults to `count`. If the final chunk does not have
    `count` elements, and `leftover` is `Nothing`, then the last chunk is
    discarded; however, if `leftover` is not `Nothing` then elements are taken
    as necessary to make the last chunk `count` elements long. If there are not
    enough elements in `leftover` then the chunk is still return with less than
    `count` elements.

    chunk [1, 2, 3, 4, 5, 6] 2 Nothing Nothing == [[1, 2], [3, 4], [5, 6]]
    chunk [1, 2, 3, 4, 5, 6] 3 (Just 2) (Just [7, 8]) == [[1, 2, 3], [3, 4, 5], [5, 6, 7]]

-}
chunk : List a -> Int -> Maybe Int -> Maybe (List a) -> Result String (List (List a))
chunk list count step leftover =
    let
        step' =
            Maybe.withDefault count step
    in
        case ( count > 0, step' > 0 ) of
            ( False, _ ) ->
                Result.Err "Count must be a positive integer greater than 0."

            ( _, False ) ->
                Result.Err "Step must be a positive integer greater than 0."

            ( True, True ) ->
                Result.Ok <| chunk' list count step' leftover


{-| Splits a List into a list of lists on every element for which `fun` returns
    a new value.

    chunk_by (\a -> a `rem` 2 == 1 ) [1, 2, 2, 3, 4, 4, 6, 7, 7] == [[1], [2, 2], [3], [4, 4, 6], [7, 7]]
    chunk_by String.length [ "one", "two", "three", "four", "five", "six" ] == [["one", "two"], ["three"], ["four", "five"], ["six"]]
-}
chunk_by : (a -> b) -> List a -> List (List a)
chunk_by fun list =
    let
        acc' v ( l, old ) =
            let
                result =
                    fun v
            in
                case ( old, l ) of
                    ( Nothing, _ ) ->
                        ( [ v ] :: l, Just result )

                    ( Just _, [] ) ->
                        ( [ [ v ] ], Just result )

                    ( Just old', head :: tail ) ->
                        if result == old' then
                            ( (v :: head) :: tail, Just result )
                        else
                            ( [ v ] :: (List.reverse head :: tail), Just result )
    in
        List.foldl acc' ( [], Nothing ) list
            |> fst
            |> List.reverse


{-| Count of elements in List for which `fun` returns True.

    count (\a -> a `rem` 2 == 0) [1, 2, 3, 4, 5] == 2
-}
count : (a -> Bool) -> List a -> Int
count fun list =
    let
        acc' item val =
            if fun item then
                val + 1
            else
                val
    in
        List.foldl acc' 0 list


{-| List of elements where consecutive duplicates are collapsed to a single
    element.

    dedup [1, 2, 3, 3, 2, 1] == [1, 2, 3, 2, 1]
-}
dedup : List a -> List a
dedup list =
    dedup_by (\a -> a) list


{-| List of elements where consecutive duplicates are collapsed to a single
    element. `fun` is used to create value which is used to determine if two
    elements are equal.

    dedup_by (\a -> a > 2) [5, 1, 2, 3, 2, 1] == [5, 1, 3, 2]
    dedup_by (\{x,y} -> x > y) [{x = 0, y = 3}, {x = 2, y = 1}, {x = 3, y = 2}, {x = 2, y = 2}, {x = 1, y = 2}] == [{x = 0, y = 3}, {x = 2, y = 1}, {x = 2, y = 2}]
-}
dedup_by : (a -> b) -> List a -> List a
dedup_by fun list =
    let
        acc' v ( l, old ) =
            let
                result =
                    fun v
            in
                case old of
                    Nothing ->
                        ( v :: l, Just result )

                    Just old' ->
                        if result == old' then
                            ( l, old )
                        else
                            ( v :: l, Just result )
    in
        List.foldl acc' ( [], Nothing ) list
            |> fst
            |> List.reverse


{-| List of elements with every `step` element dropped, starting with the first
    element. If `step` is 0, then no items are dropped. The `step` parameter
    must be non-negative integer or a `Result.Err` will be returned.

    drop_every 2 [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 ] == Result.Ok [ 2, 4, 6, 8, 10 ]
    drop_every 1 [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 ] == Result.Ok [ ]
    drop_every 0 [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 ] == Result.Ok [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 ]
    drop_every -1 [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 ] == Result.Err "Step must be a positive integer."
-}
drop_every : Int -> List a -> Result String (List a)
drop_every step list =
    let
        acc' v ( l, counter ) =
            if counter == step then
                ( l, 1 )
            else
                ( v :: l, counter + 1 )
    in
        if step < 0 then
            Result.Err "Step must be a positive integer."
        else if step == 0 then
            Result.Ok list
        else
            List.foldl acc' ( [], step ) list
                |> fst
                |> List.reverse
                |> Result.Ok


{-| Drops elements from the beginning of the List so long as `fun` returns
    True.

    drop_while (\a -> a < 3) [1, 2, 3, 4, 5] == [3, 4, 5]
-}
drop_while : (a -> Bool) -> List a -> List a
drop_while fun list =
    case list of
        [] ->
            []

        head :: tail ->
            if fun head then
                drop_while fun tail
            else
                list


{-| Invokes `fun` for each element in List. Always returns `Maybe.Nothing`

    each (\a -> Debug.log  a) ["some", "example"]
-}
each : (a -> b) -> List a -> Maybe c
each fun list =
    case list of
        [] ->
            Nothing

        head :: tail ->
            let
                _ =
                    fun head
            in
                each fun tail


{-| -}
fetch : List a -> Int -> Maybe a
fetch list index =
    case index < 0 of
        True ->
            fetch' (List.reverse list) <| (-index - 1)

        False ->
            fetch' list index


{-| Returns the first element for which `fun` returns `True`. If no such element
    is found, returns `default`.

    find (\a -> a `rem` 2 == 1) Nothing [2, 4, 6] == Nothing
    find (\a -> a `rem` 2 == 1) (Just 0) [2, 4, 6] == (Just 0)
    find (\a -> a `rem` 2 == 1) Nothing [2, 3, 4] == (Just 3)
-}
find : (a -> Bool) -> Maybe a -> List a -> Maybe a
find fun default list =
    case list of
        [] ->
            default

        head :: tail ->
            if fun head then
                Just head
            else
                find fun default tail


{-| Returns the index of the first element for which `fun` return `True`. If no
    such element is found, returns `Nothing`.

    find_index (\a -> a `rem` 2 == 1) [2, 4, 6] == Nothing
    find_index (\a -> a `rem` 2 == 1) [2, 3, 4] == Just 1
-}
find_index : (a -> Bool) -> List a -> Maybe Int
find_index fun list =
    let
        find_index' fun idx list =
            case list of
                [] ->
                    Nothing

                head :: tail ->
                    if fun head then
                        Just idx
                    else
                        find_index' fun (idx + 1) tail
    in
        find_index' fun 0 list



-- PRIVATE


chunk' : List a -> Int -> Int -> Maybe (List a) -> List (List a)
chunk' list count step leftover =
    let
        t =
            List.take count list
    in
        case list of
            [] ->
                []

            val ->
                if List.length t == count then
                    t :: chunk' (List.drop step val) count step leftover
                else
                    case leftover of
                        Nothing ->
                            []

                        Just x ->
                            (x
                                |> List.take (1 + step - List.length t)
                                |> List.append t
                            )
                                :: (chunk' [] 0 0 Nothing)


fetch' : List a -> Int -> Maybe a
fetch' list index =
    case list of
        [] ->
            Nothing

        head :: tail ->
            if index == 0 then
                Just head
            else
                fetch' tail <| index - 1
