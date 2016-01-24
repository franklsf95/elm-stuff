module DrawTrees where

import ListsAndTrees exposing (..)

import Color exposing (Color)
import Signal
import Window
import Mouse
import Text as T
import Graphics.Collage as C
import Graphics.Element as E
import Graphics.Input as I


---- UTILITIES ----

unmaybe : (a -> Maybe b) -> String -> (a -> b)
unmaybe f crashMsg =
    \x ->
        case f x of
            Just t -> t
            Nothing -> Debug.crash crashMsg

head : List a -> a
head = unmaybe List.head "empty list"

tail : List a -> List a
tail = unmaybe List.tail "empty list"

maxBy : (a -> a -> Bool) -> List a -> a
maxBy greaterThan xs =
    case xs of
        [] ->
            Debug.crash "empty list"
        [x] ->
            x
        x::xs' ->
            let max = maxBy greaterThan xs'
            in if greaterThan x max then x else max


---- MODEL ----

type Input = Next | Select TreeType  -- isomorphic to Maybe TreeType

type alias State =
    { selection : TreeType
    , current : Tree
    , rest : List Tree
    , allTrees : List Tree }

initialState : State
initialState = buildInitialState Complete

buildInitialState : TreeType -> State
buildInitialState selection =
    let
        newTrees = case selection of
            Balanced ->
                balancedTrees 0 9
            Complete ->
                completeTrees 0 4
            AlmostComplete ->
                almostCompleteTrees 0 4
    in
        case newTrees of
            [] ->
                Debug.crash "no tree to show"
            (t::ts) ->
                { current = t
                , rest = ts
                , allTrees = newTrees
                , selection = selection }

upState : Input -> State -> State
upState i st =
    case i of
        Next ->
            case st.rest of
                [] ->
                    buildInitialState st.selection
                (t::ts) ->
                    { st | current = t, rest = ts }
        Select selection ->
            buildInitialState selection


---- MODEL CONTROL ----

type TreeType = Balanced | Complete | AlmostComplete

treeTypeMailbox : Signal.Mailbox TreeType
treeTypeMailbox = Signal.mailbox Balanced

inputSignal : Signal Input
inputSignal =
    Signal.merge
        (Signal.map (\_ -> Next) Mouse.clicks)
        (Signal.map (\t -> Select t) treeTypeMailbox.signal)

stateSignal : Signal State
stateSignal = Signal.foldp upState initialState inputSignal


---- TREE VISUALIZATION ----

-- Tree layout

type ATreeElem = AEmpty | ANode Int ATree ATree
type alias ATree = { t : ATreeElem, h : Int, x : Int, w : Int }

branchSeparation : Int
branchSeparation = 1

annotateWidthHeight : Int -> Tree -> ATree
annotateWidthHeight rootHeight tree =
    case tree of
        Empty ->
            { t = AEmpty, h = 0, x = 0, w = 0 }
        Node v l r ->
            let
                al = annotateWidthHeight (rootHeight + 1) l
                ar = annotateWidthHeight (rootHeight + 1) r
                width = al.w + ar.w + branchSeparation
            in
                { t = ANode v al ar, h = rootHeight, x = 0, w = width }

annotateXPosition : Int -> ATree -> ATree
annotateXPosition startX atree =
    case atree.t of
        AEmpty ->
            atree
        ANode v al ar ->
            let
                al' = annotateXPosition startX al
                x = startX + al.w + branchSeparation
                ar' = annotateXPosition x ar
            in
                { t = ANode v al' ar', h = atree.h, x = x, w = atree.w }

generateEdges : ATree -> List Edge -> List Edge
generateEdges atree acc =
    case atree.t of
        AEmpty ->
            acc
        ANode v al ar ->
            let
                root = (atree.x, atree.h)
                el = (root, (al.x, al.h))
                acc' = if al.t == AEmpty then acc else el :: acc
                er = (root, (ar.x, ar.h))
                acc'' = if ar.t == AEmpty then acc' else er :: acc'
            in
                acc'' |> generateEdges al |> generateEdges ar

generateNodes : ATree -> List Elem -> List Elem
generateNodes atree acc =
    case atree.t of
        AEmpty ->
            acc
        ANode v al ar ->
            let
                node = { label = toString v, rank = atree.h }
                elem = { el = drawNode node, x = atree.x, y = atree.h }
                acc1 = elem :: acc
                acc2 = generateNodes al acc1
                acc3 = generateNodes ar acc2
            in
                acc3

drawTree : Tree -> TreeInfo
drawTree tree =
    let
        atree = tree |> annotateWidthHeight 0 |> annotateXPosition 0
        elems = generateNodes atree []
        edges = generateEdges atree []
    in
        (elems, edges)


---- VIEW ----

type alias CanvasSize = (Int, Int)

themeBlue = Color.rgb 25 151 198
themeGray = Color.rgb 81 88 106
colorNode = Color.rgb 227 49 69
colorEdge = Color.rgb 224 207 49

type alias NodeInfo = { label : String, rank : Int }
type alias Elem = { el : E.Element, x : Int, y : Int }
type alias Edge = ((Int, Int), (Int, Int))
type alias TreeInfo = (List Elem, List Edge)

nodeRadius = 25
edgeWidth = 8

hspace : Int -> E.Element
hspace w = E.spacer w 1

vspace : Int -> E.Element
vspace h = E.spacer 1 h

drawNode : NodeInfo -> E.Element
drawNode nodeInfo =
    let
        r = nodeRadius
        w = r * 2 |> round
        nodeCircle = C.circle r |> C.filled colorNode
        circleElem = C.collage w w [nodeCircle]
    in
        circleElem

drawEdge : Edge -> E.Element
drawEdge ((x0, y0), (x1, y1)) =
    let
        width = x0 - x1 |> abs
        height = y0 - y1 |> abs
        startX = toFloat (if x0 > x1 then width else -width) / 2
        startY = toFloat height / 2
        segment = C.segment (startX, startY) (startX + toFloat (x1 - x0), startY + toFloat (y0 - y1))
        lineStyle = { color = colorEdge, width = edgeWidth, cap = C.Flat
                    , join = C.Smooth, dashing = [], dashOffset = 0 }
        trace = C.traced lineStyle segment
        traceElem = C.collage (width) (height) [trace]
    in
        traceElem

resizeTree : CanvasSize -> TreeInfo -> TreeInfo
resizeTree (w, h) (elems, edges) =
    let
        maxWidthElem = maxBy (\n m -> n.x > m.x) elems
        maxDepthElem = maxBy (\n m -> n.y > m.y) elems
        wLimit = toFloat w - 120
        hLimit = toFloat h - 120
        xFactor = min (wLimit / toFloat maxWidthElem.x) 80
        yFactor = min (hLimit / toFloat maxDepthElem.y) 120
        times : Int -> Float -> Int
        times x y = round (toFloat x * y)
        f : Elem -> Elem
        f n =
            { el = n.el, x = times n.x xFactor, y = times n.y yFactor }
        g : Edge -> Edge
        g ((x0, y0), (x1, y1)) =
            ((times x0 xFactor, times y0 yFactor)
            , (times x1 xFactor, times y1 yFactor))
    in
        (List.map f elems, List.map g edges)

containNode : CanvasSize -> Elem -> E.Element
containNode (w, h) node =
    let
        pos = E.topLeftAt (E.absolute node.x) (E.absolute node.y)
    in
        E.container w h pos node.el

containEdge : CanvasSize -> Edge -> E.Element
containEdge (w, h) ((x0, y0), (x1, y1)) =
    let
        edgeElem = drawEdge ((x0, y0), (x1, y1))
        xBase = x0 + nodeRadius
        x = if x0 > x1 then xBase - (x0 - x1) else xBase
        y = y0 + nodeRadius
        pos = E.topLeftAt (E.absolute x) (E.absolute y)
    in
        E.container w h pos edgeElem

canvasElement : CanvasSize -> Tree -> E.Element
canvasElement (w, h) tree =
    let
        cw = w - 60
        ch = h - 300
        treeInfo = drawTree tree |> resizeTree (cw, ch)
        elems = fst treeInfo
        edges = snd treeInfo
        containedElems = List.map (containNode (cw, ch)) elems
        containedEdges = List.map (containEdge (cw, ch)) edges
        everything = E.layers (List.append containedEdges containedElems)
    in
        everything

-- The toolbar

treeButton : TreeType -> String -> TreeType -> E.Element
treeButton treeType str selection =
    let
        btnW = 160
        btnH = 40
        textHeight = 13
        tintColor = themeBlue
        drawButton : String -> (T.Text -> T.Text) -> E.Element
        drawButton imagePath textStyler =
            E.layers
                [ E.fittedImage btnW btnH imagePath
                , T.fromString str |> textStyler
                    |> T.height textHeight |> E.centered
                    |> E.container btnW btnH E.middle
                ]
        style =
            if treeType == selection then
                { upAsset = "assets/btn_down.png"
                , downAsset = "assets/btn_up.png"
                , upColor = Color.white
                , downColor = tintColor }
            else
                { upAsset = "assets/btn_up.png"
                , downAsset = "assets/btn_down.png"
                , upColor = tintColor
                , downColor = Color.white }
    in
        I.customButton (Signal.message treeTypeMailbox.address treeType)
            (drawButton style.upAsset (T.color style.upColor))
            (drawButton style.upAsset (T.color style.upColor))
            (drawButton style.downAsset (T.color style.downColor))

toolbarElement : TreeType -> E.Element
toolbarElement selection =
    let
        addr = treeTypeMailbox.address
    in
        E.flow E.right
            [ treeButton Balanced "Balanced Trees" selection
            , hspace 20
            , treeButton Complete "Complete Trees" selection
            , hspace 20
            , treeButton AlmostComplete "Almost Complete Trees" selection
            ]

-- The title

fontFamily : List String
fontFamily = ["Roboto", "Helvetica Neue", "Helvetica", "sans-serif"]

titleElement : E.Element
titleElement =
    T.fromString "Binary Trees Visualization"
        |> T.height 24
        |> T.typeface fontFamily
        |> T.bold
        |> T.color Color.white
        |> E.leftAligned

-- The raw tree

rawTreeElement : Tree -> E.Element
rawTreeElement tree =
    toString tree
        |> T.fromString
        |> T.monospace
        |> T.color themeGray
        |> E.leftAligned

-- The footnote

footnoteElement : E.Element
footnoteElement =
    T.fromString "Click the buttons to select a different collection of trees. Click anywhere else to show the next tree in the current collection."
        |> T.height 14
        |> T.typeface fontFamily
        |> T.color themeGray
        |> E.leftAligned

-- The composite view

appView : CanvasSize -> State -> E.Element
appView (w, h) st =
    let
        elements =
            [ vspace 20
            , titleElement
            , vspace 20
            , toolbarElement st.selection
            , vspace 60
            , canvasElement (w, h) st.current
            , vspace 20
            , rawTreeElement st.current
            , vspace 20
            , footnoteElement
            ]
        compositeElements =
            [ hspace 20
            , E.flow E.down elements
            ]
        containerElement =
            E.flow E.right compositeElements
                |> E.container w h E.topLeft
                |> E.color (Color.rgb 37 40 48)
    in
        containerElement


---- MAIN ----

main : Signal E.Element
main =
    Signal.map2 appView Window.dimensions stateSignal


---- UNUSED ----

view : (Int, Int) -> Tree -> E.Element
view _ _ = E.empty

signalTree : Signal Tree
signalTree = sampleListOn Mouse.clicks (almostCompleteTrees 0 4)

{-
    Due to a known issue in the type annotation system in Elm 0.16.0, the inner
    annotations must be commented out, because the compiler cannot figure out
    that the inner "a" is the same type variable as the outer "a".

    https://github.com/elm-lang/elm-compiler/blob/0.16.0/hints/type-annotations.md
-}
sampleListOn : Signal b -> List a -> Signal a
sampleListOn ticker xs =
    let
        -- f : Signal b -> List a -> List a
        f _ ys =
            case ys of
                [] -> Debug.crash "empty list"
                [y] -> xs  -- reset
                y::ys' -> ys'
        -- listSignal : Signal (List a)
        listSignal = Signal.foldp f xs ticker
    in
        Signal.map head listSignal

