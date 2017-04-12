#I "paket-files/include-scripts/net46"

//#load "include.accrod.csx"
//#load "include.xplot.plotly.fsx"
#load "packages/Deedle/Deedle.fsx"
#load "include.mathnet.numerics.fsharp.fsx"
//#load "packages/FSharp.Charting.Gtk/FSharp.Charting.Gtk.fsx"
#r "packages/XPlot.GoogleCharts/lib/net45/XPlot.GoogleCharts.dll"

open System
//open XPlot.Plotly
open Deedle
open MathNet.Numerics
open MathNet.Numerics.LinearAlgebra
open MathNet.Numerics.Distributions
//open FSharp.Charting
open XPlot.GoogleCharts

//let dataset num = 
let x = [0.0 .. 0.1 .. 1.0]
let y = x |> List.map (fun ls -> sin (2.0 * Math.PI * ls) + Normal(0.0, 0.3).Sample())
//    Series(x, y)

//dataset 10.0

Chart.Line(Series.observations (dataset 10.0))

let resolve (x: float list)(y: float list) (m: float) =
    let t = DenseVector.ofList y
    let phi = matrix [for i in 0.0 .. m -> (x |> List.map (fun j -> j ** float i))]
    let temp = phi.Transpose() * phi
    temp.Inverse() * phi.Transpose() * t |> Vector.toList


let fx (x: float) (ws: float list) =
    ws |> List.mapi (fun i w -> w * (x ** float i)) |> List.sum 

let rmsError x y m =
    let ws = resolve x y m
    let err = List.zip x y |> List.sumBy(fun a -> (snd a - (fx (fst a) ws)) ** 2.)
    sqrt (err / float x.Length) 

