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

let sigma ws = 
    let err = List.zip x y |> List.sumBy(fun a -> (snd a - (fx (fst a) ws)) ** 2.)
    sqrt (err / float x.Length) 

(*
    def log_likelihood(dataset, f):
    dev = 0.0
    n = float(len(dataset))
    for index, line in dataset.iterrows():
        x, y = line.x, line.y
        dev += (y - f(x))**2
    err = dev * 0.5
    beta = n / dev
    lp = -beta*err + 0.5*n*np.log(0.5*beta/np.pi)
    return lp
*)
// let logLikelihood = 
//     let n = float x.Length

