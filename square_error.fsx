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
open FSharp.Charting

let dataset num = 
    let rnd = Random()
    let x = [for i in 0.0 .. num - 1.0 -> i / (num - 1.0)]
    let y = x |> List.map (fun ls -> sin (2.0 * Math.PI * ls) + rnd.NextDouble() / 2.0 - 0.25)
    Series(x, y)

//dataset 10.0

Chart.Point(Series.observations (dataset 10.0))

(*
def resolve(dataset, m):
    t = dataset.y
    phi = DataFrame()
    for i in range(0,m+1):
        p = dataset.x**i
        p.name="x**%d" % i
        phi = pd.concat([phi,p], axis=1)
    tmp = np.linalg.inv(np.dot(phi.T, phi))
    ws = np.dot(np.dot(tmp, phi.T), t)

    def f(x):
        y = 0
        for i, w in enumerate(ws):
            y += w * (x ** i)
        return y

    return f
*)

//let resolve (dataset: Series) (m: int) =
    