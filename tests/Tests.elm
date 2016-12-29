module Tests exposing (..)

import Test exposing (..)
import Expect
import String


all : Test
all =
    describe "A Test Suite"
        [ test "Setup is completed" <|
            \() ->
                Expect.equal "John" "John"
        ]
