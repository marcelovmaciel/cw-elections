import Pkg

Pkg.activate("./CWElectionsBR")

using CWElectionsBR
using DataFrames
using RCall


foo = read_sav("./04700/04700.SAV") |> rcopy |> DataFrame

foo  |> names  |> 

'a':'m' .|> string

pairwise_comparisons = [Symbol("p4", string(x)) for x in 'a':'m']

foo[!, pairwise_comparisons]



foo |> names |> y->filter(x-> occursin("p4", x), y)
