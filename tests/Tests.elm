module Tests exposing (..)

import Test exposing (..)
import Expect
import String
import List.Addendum exposing (..)


all : Test
all =
    describe "The List.Addendum module"
        [ describe "List.Addendum.at/3"
            [ test "returns value at valid positive index" <|
                \() -> Expect.equal 5 <| at [ 1, 3, 5 ] 2 99
            , test "returns value at valid negative index" <|
                \() -> Expect.equal 5 <| at [ 1, 3, 5 ] -1 99
            , test "returns default value at invalid postive index" <|
                \() -> Expect.equal 99 <| at [ 1, 3, 5 ] 3 99
            , test "return default value at invalid negative index" <|
                \() -> Expect.equal 99 <| at [ 1, 3, 5 ] -4 99
            ]
        , describe "List.Addendum.fetch/2"
            [ test "returns Just value at valid positive index" <|
                \() -> Expect.equal (Just 5) <| fetch [ 1, 3, 5 ] 2
            , test "returns Just value at valid negative index" <|
                \() -> Expect.equal (Just 5) <| fetch [ 1, 3, 5 ] -1
            , test "returns Nothing at invalid positive index" <|
                \() -> Expect.equal Nothing <| fetch [ 1, 3, 5 ] 3
            , test "return Nothings at invalid negative index" <|
                \() -> Expect.equal Nothing <| fetch [ 1, 3, 5 ] -4
            ]
        ]
