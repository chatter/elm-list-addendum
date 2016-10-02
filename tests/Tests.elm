module Tests exposing (..)

import Test exposing (..)
import Expect
import String
import List.Addendum exposing (..)


{- TODO: Use Fuzzers -}


all : Test
all =
    describe "The List.Addendum module"
        [ at_tests
        , chunk_tests
        , chunk_by_tests
        , count_tests
        , dedup_tests
        , dedup_by_tests
        , drop_every_tests
        , drop_while_tests
        , each_tests
        , fetch_tests
        , find_tests
        ]


at_tests =
    describe "List.Addendum.at/3"
        [ test "returns value at valid positive index" <|
            \() -> Expect.equal 5 <| at [ 1, 3, 5 ] 2 99
        , test "returns value at valid negative index" <|
            \() -> Expect.equal 5 <| at [ 1, 3, 5 ] -1 99
        , test "returns default value at invalid postive index" <|
            \() -> Expect.equal 99 <| at [ 1, 3, 5 ] 3 99
        , test "return default value at invalid negative index" <|
            \() -> Expect.equal 99 <| at [ 1, 3, 5 ] -4 99
        ]


chunk_tests =
    describe "List.Addendum.chunk/4"
        [ test "returns 3 nested lists" <|
            \() ->
                chunk [ 1, 2, 3, 4, 5, 6 ] 2 (Just 2) Nothing
                    |> Result.withDefault []
                    |> Expect.equal [ [ 1, 2 ], [ 3, 4 ], [ 5, 6 ] ]
        , test "returns 2 nested lists" <|
            \() ->
                chunk [ 1, 2, 3, 4, 5, 6 ] 3 (Just 2) Nothing
                    |> Result.withDefault []
                    |> Expect.equal [ [ 1, 2, 3 ], [ 3, 4, 5 ] ]
        , test "returns 3 nested lists" <|
            \() ->
                chunk [ 1, 2, 3, 4, 5, 6 ] 3 (Just 2) (Just [ 7, 8 ])
                    |> Result.withDefault []
                    |> Expect.equal [ [ 1, 2, 3 ], [ 3, 4, 5 ], [ 5, 6, 7 ] ]
        , test "returns 2 nested lists with second only having 1 element" <|
            \() ->
                chunk [ 1, 2, 3, 4 ] 3 (Just 3) (Just [])
                    |> Result.withDefault []
                    |> Expect.equal [ [ 1, 2, 3 ], [ 4 ] ]
        , test "returns an empty list" <|
            \() ->
                chunk [ 1, 2, 3, 4 ] 10 (Just 10) Nothing
                    |> Result.withDefault []
                    |> Expect.equal []
        , test "returns a single nested list of 4 elements" <|
            \() ->
                chunk [ 1, 2, 3, 4 ] 10 (Just 10) (Just [])
                    |> Result.withDefault []
                    |> Expect.equal [ [ 1, 2, 3, 4 ] ]
        , test "returns an error when count is negative" <|
            \() ->
                chunk [ 1, 2, 3, 4 ] -1 (Just 0) Nothing
                    |> Expect.equal (Result.Err "Count must be a positive integer greater than 0.")
        , test "returns an error when step is negative" <|
            \() ->
                chunk [ 1, 2, 3, 4 ] 1 (Just -1) Nothing
                    |> Expect.equal (Result.Err "Step must be a positive integer greater than 0.")
        , test "uses count for step when step is Nothing" <|
            \() ->
                chunk [ 1, 2, 3, 4, 5, 6 ] 2 Nothing Nothing
                    |> Result.withDefault []
                    |> Expect.equal [ [ 1, 2 ], [ 3, 4 ], [ 5, 6 ] ]
        ]


chunk_by_tests =
    describe "List.Addendum.chunk_by/2"
        [ test "returns 3 nested lists" <|
            \() ->
                chunk_by (\a -> a `rem` 2 == 1) [ 1, 2, 2, 3, 4, 4, 6, 7, 7 ]
                    |> Expect.equal [ [ 1 ], [ 2, 2 ], [ 3 ], [ 4, 4, 6 ], [ 7, 7 ] ]
        , test "returns 4 nested lists" <|
            \() ->
                chunk_by String.length [ "one", "two", "three", "four", "five", "six" ]
                    |> Expect.equal [ [ "one", "two" ], [ "three" ], [ "four", "five" ], [ "six" ] ]
        ]


count_tests =
    describe "List.Addendum.count/2"
        [ test "returns 2" <|
            \() ->
                count (\a -> a `rem` 2 == 0) [ 1, 2, 3, 4, 5 ]
                    |> Expect.equal 2
        ]


dedup_tests =
    describe "List.Addendum.dedup/1"
        [ test "returns a 5 element list" <|
            \() ->
                dedup [ 1, 2, 3, 3, 2, 1 ]
                    |> Expect.equal [ 1, 2, 3, 2, 1 ]
        ]


dedup_by_tests =
    describe "List.Addendum.dedup_by/2"
        [ test "returns 4 element list" <|
            \() ->
                dedup_by (\a -> a > 2) [ 5, 1, 2, 3, 2, 1 ]
                    |> Expect.equal [ 5, 1, 3, 2 ]
        , test "returns a 3 element list" <|
            \() ->
                dedup_by (\{ x, y } -> x > y) [ { x = 0, y = 3 }, { x = 2, y = 1 }, { x = 3, y = 2 }, { x = 2, y = 2 }, { x = 1, y = 2 } ]
                    |> Expect.equal [ { x = 0, y = 3 }, { x = 2, y = 1 }, { x = 2, y = 2 } ]
        ]


drop_every_tests =
    describe "List.Addendum.drop_every/2"
        [ test "returns 5 element list" <|
            \() ->
                drop_every 2 [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 ]
                    |> Expect.equal (Result.Ok [ 2, 4, 6, 8, 10 ])
        , test "returns empty list" <|
            \() ->
                drop_every 1 [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 ]
                    |> Expect.equal (Result.Ok [])
        , test "returns same list when step is 0" <|
            \() ->
                drop_every 0 [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 ]
                    |> Expect.equal (Result.Ok [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 ])
        , test "returns an error when step is negative" <|
            \() ->
                drop_every -1 [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 ]
                    |> Expect.equal (Result.Err "Step must be a positive integer.")
        ]


drop_while_tests =
    describe "List.drop_while/2"
        [ test "drops first 2 elements" <|
            \() ->
                drop_while (\a -> a < 3) [ 1, 2, 3, 4, 5 ]
                    |> Expect.equal [ 3, 4, 5 ]
        ]


each_tests =
    describe "List.each/2"
        [ test "runs function for each item in list" <|
            \() ->
                each (\a -> Expect.equal a <| identity a) [ 1, 2, 3, 4, 5 ]
                    |> Expect.equal Maybe.Nothing
        ]


fetch_tests =
    describe "List.Addendum.fetch/2"
        [ test "returns Just value at valid positive index" <|
            \() -> Expect.equal (Just 5) <| fetch [ 1, 3, 5 ] 2
        , test "returns Just value at valid negative index" <|
            \() -> Expect.equal (Just 5) <| fetch [ 1, 3, 5 ] -1
        , test "returns Nothing at invalid positive index" <|
            \() -> Expect.equal Nothing <| fetch [ 1, 3, 5 ] 3
        , test "return Nothings at invalid negative index" <|
            \() -> Expect.equal Nothing <| fetch [ 1, 3, 5 ] -4
        ]


find_tests =
    describe "List.Addendum.find/3"
        [ test "returns Nothing when no match found and no default given" <|
            \() ->
                find (\a -> a `rem` 2 == 1) Nothing [ 2, 4, 6 ]
                    |> Expect.equal Nothing
        , test "returns default when no match found and default given" <|
            \() ->
                find (\a -> a `rem` 2 == 1) 0 [ 2, 4, 6 ]
                    |> Expect.equal 0
        , test "returns first matching element from list when found" <|
            \() ->
                find (\a -> a `rem` 2 == 1) Nothing [ 2, 3, 4, 5 ]
                    |> Expect.equal 3
        ]
