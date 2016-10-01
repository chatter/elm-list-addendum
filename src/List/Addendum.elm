module List.Addendum exposing (at, chunk, fetch)


at : List a -> Int -> a -> a
at list index default =
    Maybe.withDefault default <| fetch list index


fetch : List a -> Int -> Maybe a
fetch list index =
    case index < 0 of
        True ->
            fetch_list (List.reverse list) <| (-index - 1)

        False ->
            fetch_list list index


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


fetch_list : List a -> Int -> Maybe a
fetch_list list index =
    case list of
        [] ->
            Nothing

        head :: tail ->
            if index == 0 then
                Just head
            else
                fetch_list tail <| index - 1
