module Sort exposing
    ( SortingAlgo
    , bubbleSort
    , heapSort
    , insertionSort
    , mergeSort
    , quickSort
    , selectionSort
    , treeSort
    )


type alias SortingAlgo comparable =
    List comparable -> List comparable



-- BUBBLE SORT


bubbleSort : SortingAlgo comparable
bubbleSort list =
    let
        ( acc, done ) =
            bubbleSortHelp list
    in
    if done then
        acc

    else
        bubbleSort acc


bubbleSortHelp : List comparable -> ( List comparable, Bool )
bubbleSortHelp list =
    case list of
        first :: second :: rest ->
            if first > second then
                let
                    ( acc, _ ) =
                        bubbleSortHelp (first :: rest)
                in
                ( second :: acc, False )

            else
                let
                    ( acc, done ) =
                        bubbleSortHelp (second :: rest)
                in
                ( first :: acc, done )

        _ ->
            ( list, True )



-- INSERTION SORT


insertionSort : SortingAlgo comparable
insertionSort =
    List.foldl insert []


insert : comparable -> List comparable -> List comparable
insert elem sortedList =
    case sortedList of
        [] ->
            [ elem ]

        first :: rest ->
            if first < elem then
                first :: insert elem rest

            else
                elem :: sortedList



-- SELECTION SORT

type alias SelectList a =
    { selected : a
    , others : List a
    }


selectionSort : SortingAlgo comparable
selectionSort list =
    case list of
        [] ->
            list

        first :: rest ->
            let
                acc =
                    selectMinimum { selected = first, others = rest }
            in
            acc.selected :: selectionSort acc.others


selectMinimum : SelectList comparable -> SelectList comparable
selectMinimum selectList =
    case selectList.others of
        [] ->
            selectList

        first :: rest ->
            let
                acc =
                    selectMinimum { selectList | others = rest }
            in
            if first < acc.selected then
                { selected = first, others = acc.selected :: acc.others }

            else
                { acc | others = first :: acc.others }





-- MERGE SORT


mergeSort : SortingAlgo comparable
mergeSort list =
    case list of
        _ :: _ :: _ ->
            let
                ( half1, half2 ) =
                    split list
            in
            merge (mergeSort half1) (mergeSort half2)

        _ ->
            list


split : List a -> ( List a, List a )
split list =
    case list of
        first :: second :: rest ->
            let
                ( half1, half2 ) =
                    split rest
            in
            ( first :: half1, second :: half2 )

        _ ->
            ( list, [] )


merge : List comparable -> List comparable -> List comparable
merge sortedList1 sortedList2 =
    case ( sortedList1, sortedList2 ) of
        ( _, [] ) ->
            sortedList1

        ( [], _ ) ->
            sortedList2

        ( first1 :: rest1, first2 :: rest2 ) ->
            if first1 < first2 then
                first1 :: merge rest1 sortedList2

            else
                first2 :: merge sortedList1 rest2



-- QUICK SORT


quickSort : SortingAlgo comparable
quickSort list =
    case list of
        first :: rest ->
            let
                ( smaller, bigger ) =
                    List.partition (\elem -> elem < first) rest
            in
            quickSort smaller ++ first :: quickSort bigger

        _ ->
            list



-- TREE SORT


treeSort : SortingAlgo comparable
treeSort =
    List.foldl insertBst Empty >> inorder


type BinTree a
    = Empty
    | Node a (BinTree a) (BinTree a)


inorder : BinTree a -> List a
inorder tree =
    case tree of
        Empty ->
            []

        Node root leftTree rightTree ->
            inorder leftTree ++ root :: inorder rightTree


insertBst : comparable -> BinTree comparable -> BinTree comparable
insertBst elem tree =
    case tree of
        Empty ->
            Node elem Empty Empty

        Node root leftTree rightTree ->
            if root < elem then
                Node root leftTree (insertBst elem rightTree)

            else
                Node root (insertBst elem leftTree) rightTree



-- HEAP SORT


heapSort : SortingAlgo comparable
heapSort list =
    -- TODO
    list
