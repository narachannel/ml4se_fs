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
let x = [0.0 .. 0.1 .. 1.0]
let y = x |> List.map (fun ls -> sin (2.0 * Math.PI * ls) + Normal(0.0, 0.3).Sample())

let resolve (x: float list)(y: float list) (m: float) =
    let t = DenseVector.ofList y
    let phi = matrix [for i in 0.0 .. m -> (x |> List.map (fun j -> j ** float i))]
    let temp = phi * phi.Transpose()
    temp.Inverse() * phi * t |> Vector.toList


let fx (x: float) (ws: float list) =
    ws |> List.mapi (fun i w -> w * (x ** float i)) |> List.sum 

let rmsError x y m =
    let ws = resolve x y m
    let err = List.zip x y |> List.sumBy(fun a -> (snd a - (fx (fst a) ws)) ** 2.)
    sqrt (err / float x.Length) 

let showGraphs m =
    let ranges = [0.0 .. 0.01 .. 1.0]
    let ws = resolve x y m
    let correct = Scatter(x = ranges, y = (ranges |> List.map (fun ls -> sin (2.0 * Math.PI * ls))))
    let dots = Scatter(x = x, y = y, mode = "markers")
    let fits = Scatter(x = x, y = (x |> List.map (fun xi -> fx xi ws)))
    [correct; dots; fits] |> Chart.Plot |> Chart.WithWidth 700 |> Chart.WithHeight 500 |> Chart.Show

[0.; 1.; 3.; 9.] |> List.map showGraphs

let showRms = 
    let y2 = x |> List.map (fun ls -> sin (2.0 * Math.PI * ls) + Normal(0.0, 0.3).Sample())
    let ms = [0. .. 10.]
    let train = ms |> List.map (fun m -> rmsError x y m)
    let test = ms |> List.map (fun m -> rmsError x y2 m)
    let trainChart = Scatter(x = ms, y = train)
    let testChart = Scatter(x = ms, y = test)
    [trainChart; testChart] |> Chart.Plot |> Chart.WithWidth 700 |> Chart.WithHeight 500 |> Chart.Show