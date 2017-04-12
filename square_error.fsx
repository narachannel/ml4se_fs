#I "paket-files/include-scripts/net46"

//#load "include.accrod.csx"
//#load "include.xplot.plotly.fsx"
#load "packages/Deedle/Deedle.fsx"
#load "include.mathnet.numerics.fsharp.fsx"
#load "packages/FSharp.Charting.Gtk/FSharp.Charting.Gtk.fsx"

open System
//open XPlot.Plotly
open Deedle
open MathNet.Numerics
open MathNet.Numerics.LinearAlgebra
open MathNet.Numerics.Distributions
open FSharp.Charting

let dataset num = 
    let x = [for i in 0.0 .. num - 1.0 -> i / (num - 1.0)]
    let y = x |> List.map (fun ls -> sin (2.0 * Math.PI * ls) + Normal(0.0, 0.3).Sample())
    Series(x, y)

//dataset 10.0

Chart.Point(Series.observations (dataset 10.0))

let resolve (x: float list)(y: float list) (m: float) =
    let t = DenseVector.ofList y
    let phi = matrix [for i in 0.0 .. m -> (x |> List.map (fun j -> j ** float i))]
    let temp = phi.Transpose() * phi
    temp.Inverse() * phi.Transpose() * t


let fx (x: float) (ws: float list) =
    ws |> List.mapi (fun i w -> w * (x ** float i)) |> List.sum 