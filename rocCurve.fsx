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
    | Circle = 0
    | Cross = 1

type Dot = { X : float ; Y : float; DotType : DotType}

//(80, [9,9], 50, 200, [-3,-3], 50)
//(80, [9,9], 150, 200, [-3,-3], 150)
let data2d = 
    let x1 = dots 80 9. 3.
    let y1 = dots 80 9. 3.
    let x2 = dots 200 -3. 3.
    let y2 = dots 200 -3. 3.
    [Scatter(x = x1, y = y1, mode = "markers"); Scatter(x = x2, y = y2, mode = "markers")]|> Chart.Plot |> Chart.Show
    (List.zip x2 y2 |> List.map (fun xy -> { X = fst xy; Y = snd xy; DotType = DotType.Cross}))
    @ (List.zip x1 y1 |> List.map (fun xy -> { X = fst xy; Y = snd xy; DotType = DotType.Circle}))

(*
def run_logistic(train_set):
    w0, w1, w2 = w[0], w[1], w[2]
    err = 0
    for index, line in train_set.iterrows():
        a = np.dot(np.array([1, line.x, line.y]), w)
        p = 1.0/(1.0+np.exp(-a))
        train_set.ix[index, 'probability'] = p
        if (p-0.5)*line.type < 0:
            err += 1
    err_rate = err * 100 / len(train_set)
    result = train_set.sort_values(by=['probability'], ascending=[False]).reset_index()
    
    return w0, w1, w2, err_rate, result
*)
let unpack3 tup3 ind =
    match ind, tup3 with
    | 0, (a,_,_) -> a
    | 1, (_,b,_) -> b
    | 2, (_,_,c) -> c
    | _, _ -> failwith (sprintf "Trying to access item %i of tuple with 3 entries." ind) 

let logistic (data : Dot list)= 
    let mutable w = vector [0.; 0.1; 0.1]
    let phi = matrix [for d in data -> [1.0; d.X; d.Y]]
    let t = vector [for d in data -> float d.DotType]
    
    
    let y = [for d in data -> 1. / (1. + exp (-w.[0] - w.[1] * d.X - w.[2] * d.Y))]
    //let r = y * (1.0 - y)
    // r = np.diag(y*(1-y)) 
    //     y = y[np.newaxis,:].T
    //     tmp1 = np.linalg.inv(np.dot(np.dot(phi.T, r),phi))
    //     tmp2 = np.dot(phi.T, (y-t))
    //     w_new = w - np.dot(tmp1, tmp2)
    //     # パラメータの変化が 0.1% 未満になったら終了
    //     if np.dot((w_new-w).T, (w_new-w)) < 0.001 * np.dot(w.T, w):
    //         w = w_new
    //         break
    //     w = w_new
    w
