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

let resolve (x: float list)(y: float list) (m: float) =
    let t = DenseVector.ofList y
    let phi = matrix [for i in 0.0 .. m -> (x |> List.map (fun j -> j ** float i))]
    let temp = phi * phi.Transpose()
    temp.Inverse() * phi * t |> Vector.toList

let fx (x: float) (ws: float list) =
    ws |> List.mapi (fun i w -> w * (x ** float i)) |> List.sum 

let sigma x y w= 
    let err = List.zip x y |> List.sumBy(fun a -> (snd a - (fx (fst a) w)) ** 2.)
    sqrt (err / float x.Length) 

let logLikelihood (x: float list)  y w = 
    let n = float x.Length
    let dev = List.zip x y |> List.sumBy(fun a -> (snd a - (fx (fst a) w)) ** 2.)
    - n * 0.5 * (1.0 - log (0.5 * n / (dev * Math.PI)))


let showResult m = 
    let x = [0.0 .. 0.1 .. 1.0]
    let xp = [0. .. 0.01 .. 1.0]
    let y = x |> List.map (fun ls -> sin (2.0 * Math.PI * ls) + Normal(0.0, 0.3).Sample())
    let chartSin = Scatter(x = xp, y = (xp |> List.map (fun ls -> sin (2.0 * Math.PI * ls))))
    let dots = Scatter(x = x, y = y, mode = "markers")
    let w = resolve x y m
    let train s = Scatter(x = x, y = (x |> List.map(fun xi -> (fx xi w) + s)))
    let s = sigma x y w
    [chartSin; dots;(train 0.);(train s);(train -s)] |> Chart.Plot |> Chart.WithWidth 700 |> Chart.WithHeight 500 |> Chart.Show

[0.0; 1.0; 3.0; 9.0] |> List.map showResult
 
let showLogLikelihoodTrend x train test =
    let w = resolve x train
    let ms = [0. .. 10.]
    let trainMlh = ms |> List.map (fun m -> logLikelihood x train (resolve x train m))
    let testMlh = ms |> List.map(fun m -> logLikelihood x test (resolve x train m))
    [Scatter(x = ms, y = trainMlh); Scatter(x = ms, y = testMlh)] |> Chart.Plot |> Chart.WithWidth 700 |> Chart.WithHeight 500 |> Chart.Show

let x = [0.0 .. 0.1 .. 1.0]
let train = x |> List.map (fun ls -> sin (2.0 * Math.PI * ls) + Normal(0.0, 0.3).Sample())
let test = x |> List.map (fun ls -> sin (2.0 * Math.PI * ls) + Normal(0.0, 0.3).Sample())
showLogLikelihoodTrend x train test