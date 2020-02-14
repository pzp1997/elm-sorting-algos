module SortTests exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (int, list)
import Sort exposing (..)
import Test exposing (Test, describe, fuzz)


suite : Test
suite =
    describe "Sorting Algorithms"
        [ fuzzSortingAlgo "Built-in" List.sort
        , fuzzSortingAlgo "Bubble Sort" bubbleSort
        , fuzzSortingAlgo "Insertion Sort" insertionSort
        , fuzzSortingAlgo "Selection Sort" selectionSort
        , fuzzSortingAlgo "Merge Sort" mergeSort
        , fuzzSortingAlgo "Quicksort" quickSort
        , fuzzSortingAlgo "Tree Sort" treeSort
        , Test.skip <| fuzzSortingAlgo "Heap Sort" heapSort
        ]


fuzzSortingAlgo : String -> SortingAlgo Int -> Test
fuzzSortingAlgo name algo =
    fuzz (list int) name (checkSort algo >> expectSorted)


type SortResult a
    = Sorted
    | NotSorted (List a)


expectSorted : SortResult Int -> Expectation
expectSorted sortResult =
    case sortResult of
        Sorted ->
            Expect.pass

        NotSorted output ->
            output
                |> List.map String.fromInt
                |> String.join ","
                |> (\s -> "[" ++ s ++ "]")
                |> Expect.fail


checkSort : SortingAlgo comparable -> List comparable -> SortResult comparable
checkSort algo originalList =
    let
        sortedList =
            algo originalList
    in
    if isSorted sortedList && isPermutation originalList sortedList then
        Sorted

    else
        NotSorted sortedList


isSorted : List comparable -> Bool
isSorted list =
    case list of
        first :: second :: rest ->
            first <= second && isSorted (second :: rest)

        _ ->
            True


isPermutation : List a -> List a -> Bool
isPermutation list1 list2 =
    case list1 of
        [] ->
            list1 == list2

        first :: rest ->
            removeFirstOccurence first list2
                |> Maybe.map (isPermutation rest)
                |> Maybe.withDefault False


removeFirstOccurence : a -> List a -> Maybe (List a)
removeFirstOccurence elem list =
    case list of
        [] ->
            Nothing

        first :: rest ->
            if first == elem then
                Just rest

            else
                removeFirstOccurence elem rest
                    |> Maybe.map ((::) first)
