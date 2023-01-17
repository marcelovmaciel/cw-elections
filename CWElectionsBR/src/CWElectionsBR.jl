module CWElectionsBR



using CairoMakie
using Combinatorics
using DataFrames
using Distances
import CSV
using Chain
using GeometryBasics
using Meshes
using MeshViz
using Pipe
using RCall
using StatsBase
using StatFiles
import SymPy as sp


# using NamedArrays

candidates = ["Bolsonaro", "Haddad", "Ciro", "Alckmin"] |> sort

dfspath= "../rscripts/dfs/"

include("preprocessing_utils.jl")
include("transfer_algorithm.jl")
include("geometric_musings.jl")
include("preprocess_before_plot.jl")


export  representation△,
       sweep_transfer,
       drop_candidate_plot_triangle


end # module
