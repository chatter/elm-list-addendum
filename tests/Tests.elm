module Tests exposing (..)

import Test exposing (..)
import Expect
import String
import List.Addendum exposing (..)


all : Test
all =
    describe "The List.Addendum module"
        [ describe "List.fetch/2"
            [ test "returns value at valid positive index" <|
                \() -> Expect.equal (Just 5) <| fetch [ 1, 3, 5 ] 2
            , test "returns value at valid negative index" <|
                \() -> Expect.equal (Just 5) <| fetch [ 1, 3, 5 ] -1
            , test "returns Nothing at invalid positive index" <|
                \() -> Expect.equal Nothing <| fetch [ 1, 3, 5 ] 3
            , test "return Nothings at invalid negative index" <|
                \() -> Expect.equal Nothing <| fetch [ 1, 3, 5 ] -4
            ]
        ]
