module Pi where

import Char
import Color exposing (Color)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Keyboard
import Random
import Signal
import String
import Text
import Time exposing (Time)
import Window


---- UNUSED BOILERPLATE ----

type alias Point = { x: Float, y: Float }
type alias State = ((Int, List Point), (Int, List Point))

initState = ((0,[]), (0,[]))

upstate : Point -> State -> State
upstate pt ((hitCount, hits), (missCount, misses)) =
    if pt.x ^ 2 + pt.y ^ 2 < 1 then
        ((hitCount + 1, pt :: hits), (missCount, misses))
    else
        ((hitCount, hits), (missCount + 1, pt :: misses))

view : (Int, Int) -> State -> Element
view (w, h) st = empty

genPoint : Random.Seed -> (Point, Random.Seed)
genPoint s =
    let
        (x, s')  = Random.generate genCoord s
        (y, s'') = Random.generate genCoord s'
    in
        ({ x = x, y = y }, s'')

signalPointSeed : Signal (Point, Random.Seed)
signalPointSeed =
    Signal.foldp
        (\_ (_, s) -> genPoint s)
        ({x = 0, y = 0}, initSeed)
        signalClock

signalPoint : Signal Point
signalPoint =
    Signal.map
        (\(pt, _) -> pt)
        signalPointSeed


---- MODEL ----

type alias PiPoint =
    { x : Float
    , y : Float
    , t : Int  -- time of birth
    }

type alias PiState =
    { hitCount : Int
    , hits : List PiPoint
    , missCount : Int
    , misses : List PiPoint
    , totalCount : Int
    }

initPiState : PiState
initPiState =
    { hitCount = 0
    , hits = []
    , missCount = 0
    , misses = []
    , totalCount = 0
    }

upPiState : PiPoint -> PiState -> PiState
upPiState pt st =
    if pt.x ^ 2 + pt.y ^ 2 < 1 then
        { hitCount = st.hitCount + 1
        , hits = pt :: st.hits
        , missCount = st.missCount
        , misses = st.misses
        , totalCount = st.totalCount + 1
        }
    else
        { hitCount = st.hitCount
        , hits = st.hits
        , missCount = st.missCount + 1
        , misses = pt :: st.misses
        , totalCount = st.totalCount + 1
        }


---- CONTROLLER ----

initPiPoint : PiPoint
initPiPoint = { x = 0, y = 0, t = 0 }

initSeed : Random.Seed
initSeed = Random.initialSeed 42

genCoord : Random.Generator Float
genCoord = Random.float -1 1

genPiPointSeedPiState : Time -> (PiPoint, Random.Seed) -> (PiPoint, Random.Seed)
genPiPointSeedPiState t (pt, s) =
    let
        (x, s')  = Random.generate genCoord s
        (y, s'') = Random.generate genCoord s'
    in
        ({ x = x, y = y, t = pt.t + 1 }, s'')

signalPiPointSeed : Signal (PiPoint, Random.Seed)
signalPiPointSeed =
    Signal.foldp
        genPiPointSeedPiState
        (initPiPoint, initSeed)
        signalClock

signalPiPoint : Signal PiPoint
signalPiPoint =
    Signal.map
        (\(pt, _) -> pt)
        signalPiPointSeed


---- TIME SIGNAL ----

fps : Int
fps = 600

signalClock : Signal Time
signalClock = Time.fpsWhen fps signalRunning


---- RUNNING STATE CONTROLLER ----

initRunning : Bool
initRunning = True

updateRunning : Char.KeyCode -> Bool -> Bool
updateRunning k r =
    if k == Char.toCode ' ' then
        not r
    else
        r

signalRunning : Signal Bool
signalRunning = Signal.foldp updateRunning initRunning Keyboard.presses


---- VIEW ----

type alias CanvasSize = (Int, Int)

type alias RGB = (Int, Int, Int)

-- The points

canvasSize : CanvasSize
canvasSize = (400, 400)

pointRadius : CanvasSize -> Float
pointRadius (w, _) =
    toFloat w / 100

pointToCircle : PiPoint -> PiState -> CanvasSize -> RGB -> Form
pointToCircle pt st (w, h) (r, g, b) =
    let
        x = pt.x * toFloat w / 2
        y = pt.y * toFloat h / 2
        threshold = 40
        age = toFloat (st.totalCount - pt.t)
        alpha = if age < threshold then 1 - 0.5 / threshold * age else 0.5
        color = Color.rgba r g b alpha
        rad = pointRadius (w, h)
        coef = if age < threshold then 1 - 0.1 / threshold * age else 0.9
        radius = rad * coef
    in
        circle radius
            |> filled color
            |> move (x, y)

pointsToCircles : PiState -> CanvasSize -> List Form
pointsToCircles st (w, h) =
    let
        hitColor = (230, 71, 89)
        missColor = (28, 168, 221)
        f = \pt -> pointToCircle pt st (w, h) hitColor
        g = \pt -> pointToCircle pt st (w, h) missColor
        hitCircles = List.map f st.hits
        missCircles = List.map g st.misses
        circles = hitCircles ++ missCircles
    in
        circles

canvasBackground : CanvasSize -> Form
canvasBackground (w, h) =
    rect (toFloat w) (toFloat h)
        |> filled (Color.rgb 26 28 34)

circleBackground : PiState -> CanvasSize -> Form
circleBackground st (w, h) =
    oval (toFloat w) (toFloat h)
        |> filled (Color.rgba 255 255 255 0.05)

canvasElement : PiState -> CanvasSize -> Element
canvasElement st windowSize =
    let
        forms = canvasBackground canvasSize
                :: circleBackground st canvasSize
                :: pointsToCircles st canvasSize
    in
        collage (fst canvasSize) (snd canvasSize) forms

-- The labels

piApprox : PiState -> Float
piApprox st =
    let
        h = toFloat st.hitCount
        t = toFloat st.totalCount
    in
        h / t * 4

piError : PiState -> Float
piError st =
    abs (pi - piApprox st) / pi

piString : PiState -> String
piString st =
    piApprox st
        |> toString
        |> String.left 8
        |> String.append "π ≈ "

countString : PiState -> String
countString st =
    String.append
        "\nNumber of trials = "
        (toString st.totalCount)

fontFamily : List String
fontFamily = ["Roboto", "Helvetica Neue", "Helvetica", "sans-serif"]

labelElement : PiState -> Element
labelElement st =
    let
        piText = Text.fromString (piString st)
        countText = Text.fromString (countString st)
        allText = Text.concat [piText, countText]
    in
        allText
            |> Text.height 18
            |> Text.typeface fontFamily
            |> leftAligned

-- The title

titleElement : Element
titleElement =
    Text.fromString "Monte Carlo Pi Approximation"
        |> Text.height 24
        |> Text.typeface fontFamily
        |> Text.bold
        |> leftAligned

-- The footnote

footnoteElement : Element
footnoteElement =
    Text.fromString "Press space key to pause/resume simulation."
        |> Text.height 14
        |> Text.typeface fontFamily
        |> leftAligned


-- The composite view

appView : CanvasSize -> PiState -> Element
appView (w, h) st =
    let
        elements =
            [ spacer 1 20
            , titleElement
            , spacer 1 10
            , canvasElement st canvasSize
            , spacer 1 10
            , labelElement st
            , spacer 1 10
            , footnoteElement
            ]
        allElements =
            [ spacer 20 1
            , flow down elements
            ]
    in
        flow right allElements


---- DRIVER ----

main : Signal Element
main =
    Signal.map2 appView Window.dimensions
        (Signal.foldp upPiState initPiState signalPiPoint)

