module List.Addendum exposing (at, chunk, chunk_by, fetch)

{-|

@docs at, chunk, chunk_by, fetch

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
                Result.Err "Count must be a positive integer greater than 0"

            ( _, False ) ->
                Result.Err "Step must be a positive integer greater than 0"

            ( True, True ) ->
                Result.Ok <| chunk' list count step' leftover


{-| Splits a List into a list of lists on every element for which `fun` returns
    a new value.

    chunk_by (\a -> a `rem` 2 == 1 ) [1, 2, 2, 3, 4, 4, 6, 7, 7]
    chunk_by String.length [ "one", "two", "three", "four", "five", "six" ]
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


{-| -}
fetch : List a -> Int -> Maybe a
fetch list index =
    case index < 0 of
        True ->
            fetch' (List.reverse list) <| (-index - 1)

        False ->
            fetch' list index



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
