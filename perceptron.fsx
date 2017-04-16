#I "paket-files/include-scripts/net46"
#load "include.xplot.plotly.fsx"
#load "packages/Deedle/Deedle.fsx"
#load "include.mathnet.numerics.fsharp.fsx"

open System
open XPlot.Plotly
open Deedle
open MathNet.Numerics
open MathNet.Numerics.LinearAlgebra
open MathNet.Numerics.Distributions

let dots number mu variance = Normal.Samples(mu, variance) |> Seq.truncate number |> Seq.toList

type DotType = 
    | Circle = -1
    | Cross = 1

type Dot = { X : float ; Y : float; DotType : DotType}

//(20, [15,10], 15, 30, [0,0], 15)
//(20, [15,10], 30, 30, [0,0], 30)
let data2d = 
    let x1 = dots 20 15. 5.
    let y1 = dots 20 10. 5.
    let x2 = dots 30 0. 5.
    let y2 = dots 30 0. 5.
    //[Scatter(x = x1, y = y1, mode = "markers"); Scatter(x = x2, y = y2, mode = "markers")]|> Chart.Plot |> Chart.Show
    (List.zip x2 y2 |> List.map (fun xy -> { X = fst xy; Y = snd xy; DotType = DotType.Cross}))
    @ (List.zip x1 y1 |> List.map (fun xy -> { X = fst xy; Y = snd xy; DotType = DotType.Circle}))
    
let unpack3 tup3 ind =
    match ind, tup3 with
    | 0, (a,_,_) -> a
    | 1, (_,b,_) -> b
    | 2, (_,_,c) -> c
    | _, _ -> failwith (sprintf "Trying to access item %i of tuple with 3 entries." ind) 

let history (data : Dot) (bias : float) (parameters : float * float * float) =
    let t = float data.DotType
    let w0 = unpack3 parameters 0
    let w1 = unpack3 parameters 1
    let w2 = unpack3 parameters 2
    if  (bias * w0 + data.X * w1 + data.Y * w2) * t <= 0.0 then (w0 + t * bias, w1 + t * data.X, w2 + t * data.Y)
    else parameters

let run (train: Dot list) (w : float * float * float ) = 
    let bias = (List.averageBy(fun d -> d.X) train + List.averageBy(fun d -> d.Y) train ) * 0.5
    List.fold (fun acc t -> history t bias acc) w train
    
let runMany train =
    let mutable a = (0., 0., 0.)
    let mutable list = List.empty
    list <- list @ [a]
    for i in 0 .. 29 do
        a <- run train a
        list <- list @ [a]
    list

let errorRate (bias: float) (w: float * float * float) (data: Dot list) = 
    let err = List.sumBy (fun d -> if (float d.DotType) * ((unpack3 w 0) * bias + (unpack3 w 1) * d.X + (unpack3 w 2) * d.Y) <= 0. then 1. else 0.) data
    err * 100. / float data.Length

let showResult = 
    let data = data2d
    let bias = (List.averageBy(fun d -> d.X) data + List.averageBy(fun d -> d.Y) data ) * 0.5
    let results = runMany data
    let result = results.[29]
    let w0 = unpack3 result 0
    let w1 = unpack3 result 1
    let w2 = unpack3 result 2
    let circles = Scatter(x = (data |> List.filter(fun d -> d.DotType = DotType.Circle) |> List.map (fun d -> d.X)), y = (data |> List.filter(fun d -> d.DotType = DotType.Circle) |> List.map (fun d -> d.Y)), mode = "markers")
    let crosses = Scatter(x = (data |> List.filter(fun d -> d.DotType = DotType.Cross) |> List.map (fun d -> d.X)), y = (data |> List.filter(fun d -> d.DotType = DotType.Cross) |> List.map (fun d -> d.Y)), mode = "markers")
    let xmax = List.maxBy (fun x -> x.X) data
    let xmin = List.minBy (fun x -> x.X) data
    let xrange = [xmin.X .. xmax.X]
    let yrange = xrange |> List.map (fun x -> -x * w1 / w2 - bias * w0 / w2)
    let line = Scatter(x = xrange, y = yrange)
    [circles; crosses; line] |> Chart.Plot |> Chart.Show
    let eRange = [0..29]
    let w i = List.map (fun r -> (unpack3 r i)) results
    [Scatter(x = eRange, y = (w 0)); Scatter(x = eRange, y = (w 1));Scatter(x = eRange, y = (w 2))] |> Chart.Plot |> Chart.Show