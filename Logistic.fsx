#I "paket-files/include-scripts/net46"
#load "include.xplot.plotly.fsx"
#load "packages/Deedle/Deedle.fsx"
#load "include.mathnet.numerics.fsharp.fsx"

open System
open FSharp.Core
open XPlot.Plotly
open Deedle
open MathNet.Numerics
open MathNet.Numerics.LinearAlgebra
open MathNet.Numerics.Distributions

let dots number mu variance = Normal.Samples(mu, variance) |> Seq.truncate number |> Seq.toList

type DotType = 
    | Circle = 0
    | Cross = 1

type Dot = { X : float ; Y : float; DotType : DotType}

//(80, [9,9], 50, 200, [-3,-3], 50)
//(80, [9,9], 150, 200, [-3,-3], 150)
let data2d v= 
    let x1 = dots 80 9. v
    let y1 = dots 80 9. v
    let x2 = dots 200 -3. v
    let y2 = dots 200 -3. v
    (List.zip x2 y2 |> List.map (fun xy -> { X = fst xy; Y = snd xy; DotType = DotType.Cross}))
    @ (List.zip x1 y1 |> List.map (fun xy -> { X = fst xy; Y = snd xy; DotType = DotType.Circle}))

let logistic (data : Dot list)= 
    let mutable w = vector [0.; 0.1; 0.1]
    let phi = matrix [for d in data -> [1.0; d.X; d.Y]]
    let t = vector [for d in data -> float d.DotType]
    let solveW (w : Vector<float>) = 
        let y = vector [for d in data -> 1. / (1. + exp (-w.[0] - w.[1] * d.X - w.[2] * d.Y))]
        let r = DiagonalMatrix.ofDiagArray [|for i in 0 .. data.Length - 1 -> y.[i] * (1. - y.[i])|]
        let tmp1 = (phi.Transpose() * r) * phi
        let tmp2 = phi.Transpose() * (y - t)
        w - tmp1.Inverse() * tmp2
    let mutable wNew = w
    let mutable continueLooping = true
    let mutable loopCount = 0
    while continueLooping do
        wNew <- solveW w
        if (wNew - w) * (wNew - w) < 0.001 * (w * w) then 
            continueLooping <- false
        if loopCount > 29 then
            continueLooping <- false
        loopCount <- loopCount + 1
        w <- wNew
    wNew

let error (w: Vector<float>) (data : Dot list)= 
    let err = data |> List.sumBy (fun d -> if 1. / (1. + exp (- w.[0] - w.[1] * d.X - w.[2] * d.Y)) * (float d.DotType * 2.0 - 1.0) - 0.5 < 0. then 1. else 0.)
    err * 100.0 / float data.Length

let show =
    let data = data2d 6.
    let ws = logistic data
    let data1 = List.filter (fun d -> d.DotType = DotType.Circle) data
    let data2 = List.filter (fun d -> d.DotType = DotType.Cross) data
    let range = [-20. .. 20.]
    [Scatter(x= List.map (fun d -> d.X) data1, y= List.map (fun d -> d.Y) data1, mode = "markers"); 
    Scatter(x = List.map (fun d -> d.X) data2, y = List.map (fun d -> d.Y) data2, mode = "markers");
    Scatter(x = range, y = (List.map(fun r -> -r * ws.[1] / ws.[2] - ws.[0] / ws.[2]) range))]|> Chart.Plot |> Chart.Show
