module Symbol

open Fable.React
open Fable.React.Props
open Browser
open Elmish
open Elmish.React
open Helpers
open CommonTypes

type Msg = Unit // no messages needed

type Model = Unit // No model needed

//-------------------------Helper functions for Tick 3-------------------------------------//

// Helper functions that are useful should normally be put into Helpers module
// For Tick3 only put Helper functions here for ease of assessment and feedback
// The obvious helpers to simplify solution are those that create and manipulate SVG elements.
// SVG boilerplate should be greatly reduced in a well-written system.

let posOf x y = { X = x; Y = y } // helper

// add your own functions as needed


//-----------------------------------------------------------------------------------------//


/// write this for Tick3 using your modified ComponentType
/// you may add to type definition in CommonTypes
let makeBusDecoderComponent (pos: XYPos) (w: int) (a: int) (n: int) =
    { X = int pos.X
      Y = int pos.Y
      Type = BusDecoder(w, a, n)
      W = 0
      H = 150 }

/// demo function - not needed for Tick3 answer
//dummy component deleted as well

//-----------------------Elmish functions with no content in Tick3----------------------//

/// For this program init() generates the required result
let init () = (), Cmd.none

/// update function does nothing!
let update (msg: Msg) (model: Model): Model * Cmd<'a> =
    match msg with
    | () -> model, Cmd.none // do nothing if we receive the (only) message

//----------------------------View Function for Symbol----------------------------//



/// Tick3 answer
let busDecoderView (comp: Component) =
    let fX = float comp.X
    let fY = float comp.Y
    let height = comp.H
    let width = comp.W

    let tup1 =
        match comp.Type with
        | BusDecoder (x, y, z) -> [ x; y; z ]
        | _ -> failwithf "wont ever hit this, but for the compiler"

    let a = tup1.[1]
    let w = tup1.[0]
    let n = tup1.[2]

    let out_list = [ a .. a + (n - 1) ]


    let allout out_list =

        let printTxt index element =
            (text [ X 130.
                    Y(45 + (index * 20))
                    Style [ TextAnchor "middle"
                            DominantBaseline "hanging"
                            FontSize "15px"
                            FontWeight "Bold"
                            Fill "Black" ] ] [
                str <| sprintf "%A" element
             ])

        let std =
            [ polygon [ SVGAttr.Points "5 0,5 200,150 200,150 0"
                        SVGAttr.StrokeWidth "1px"
                        SVGAttr.Stroke "Black"
                        SVGAttr.FillOpacity 0.3
                        SVGAttr.Fill "Grey" ] []

              text [ X 80.
                     Y 5.
                     Style [ TextAnchor "middle"
                             DominantBaseline "hanging"
                             FontSize "15px"
                             FontWeight "Bold"
                             Fill "Black" ] ] [
                  str <| sprintf "Bus Decode"
              ]

              text [ X 15.
                     Y(height / 2)
                     Style [ TextAnchor "middle"
                             DominantBaseline "hanging"
                             FontSize "20px"
                             FontWeight "Bold"
                             Fill "Black" ] ] [
                  str <| sprintf "In"
              ] ]

        List.mapi (printTxt) out_list |> List.append std


    //
    // This code demonstrates svg transformations, not needed for Tick3 but useful.
    // The elmish react syntax here uses CSS style transforms, not SVG attribute transforms. They are different.
    // In addition, svg elements transform under css differently from html.
    // See https://css-tricks.com/transforms-on-svg-elements/ for details if needed.
    //
    let scaleFactor = 1.0 // to demonstrate svg scaling
    let rotation = 0 // to demonstrate svg rotation (in degrees)


    g
        [ Style [ TransformOrigin "0px 50px"
                  Transform(sprintf "translate(%fpx,%fpx) rotate(%ddeg) scale(%f) " fX fY rotation scaleFactor) ] ]
        (Seq.ofList (allout out_list))

/// demo function has been deleted



/// View function - in this case view is independent of model
let view (model: Model) (dispatch: Msg -> unit) =
    [ makeBusDecoderComponent { X = 100.; Y = 500. } 4 3 5 // for Tick 3 two components
      makeBusDecoderComponent { X = 100.; Y = 100. } 3 0 8 ]
    |> List.map busDecoderView // change for Tick3 answer
    |> (fun svgEls ->
        svg
            [ Style [ Border "3px solid green"
                      Height 1000.
                      Width 500. ] ]
            svgEls)


type ValidateError =
    | WIsInvalid // ignoring a,n
    | AIsInvalid // for given w, ignoring n
    | NIsInvalid // for given a,w

/// Tick3 answer
let busDecoderValidate (comp: Component): Result<Component, ValidateError * string> =
    match comp.Type with
    | BusDecoder (w, a, n) ->
        match w, a, n with
        | w, a, n when w <= 0 -> Error(WIsInvalid, "w less than 0")
        | w, a, n when a < 0 || (a + 1) > int (2.0 ** float w) -> Error(AIsInvalid, "a not in range")
        | w, a, n when n <= 0 || (n + a) > int (2.0 ** float w) -> Error(NIsInvalid, "n not in range")
        | _ -> Ok comp
    | _ -> failwithf "this is not a busdecoder"
