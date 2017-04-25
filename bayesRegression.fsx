#I "paket-files/include-scripts/net46"
#load "include.xplot.plotly.fsx"
#load "packages/Deedle/Deedle.fsx"
#load "include.mathnet.numerics.fsharp.fsx"

open System
open System.IO
open System.Drawing
open XPlot.Plotly
open Deedle
open MathNet.Numerics
open MathNet.Numerics.LinearAlgebra
open MathNet.Numerics.Distributions


let x = [0.0; 0.3333; 0.6666; 1.0]
let y = x |> List.map (fun ls -> sin (2.0 * Math.PI * ls) + Normal(0.0, 0.3).Sample())

let resolve m = 
    let beta = 1.0 / (0.3) **2.0
    let alpha = 1.0 / 100.0 ** 2.0
    let phi = matrix [for i in 0.0 .. m -> (x |> List.map (fun j -> j ** float i))]

    beta

