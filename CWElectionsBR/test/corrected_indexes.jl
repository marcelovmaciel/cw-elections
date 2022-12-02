

import Pkg
Pkg.activate("./")

using CWElectionsBR
using RCall
import CSV
using DataFrames
dfspath = "../rscripts/dfs/"


candidates = ["bolsonaro", "haddad", "ciro", "alckmin"] |> sort


mincw1 =  CSV.read(dfspath * "min_c1_raw.csv",DataFrame)

mincw2 =  CSV.read(dfspath * "min_c2_raw.csv",DataFrame)

function get_corrected_indexes(corrected_raw)
    turnedintovecs_c1 = Vector{String}.(eachrow(corrected_raw))
    filterothers = filter(x->all(y-> y != "other", x), turnedintovecs_c1)
    get_index_candidate(candidate,vecss) = map(y->findfirst(x-> x== candidate,y), vecss)
    DataFrame([(candidate=>get_index_candidate(candidate,filterothers)) 
    for candidate in candidates]...)
end


CSV.write(dfspath * "corrected1_indexes.csv", get_corrected_indexes(mincw1))
CSV.write(dfspath * "corrected2_indexes.csv", get_corrected_indexes(mincw2))

