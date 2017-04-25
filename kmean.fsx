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


//Todo: Separate pixels into RGBs
let imageToArray = 
    let image = Image.FromFile("photo.jpg")
    let ms = new MemoryStream()
    image.Save(ms, System.Drawing.Imaging.ImageFormat.Jpeg)
    ms.ToArray() |> Array.map float



imageToArray