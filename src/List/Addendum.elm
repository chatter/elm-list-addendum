module List.Addendum exposing (at, fetch)


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
