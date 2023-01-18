

import Pkg
Pkg.activate("./")

import  CWElectionsBR as cw
using RCall
import CSV
using DataFrames
dfspath = "../rscripts/dfs/"


candidates = cw.candidates

mincw1 =  CSV.read(dfspath * "min_raw_1.csv",DataFrame)
mincw2 =  CSV.read(dfspath * "min_raw_2.csv",DataFrame)
mincw3 =  CSV.read(dfspath * "min_raw_3.csv",DataFrame)
mincw4 =  CSV.read(dfspath * "min_raw_4.csv",DataFrame)



function get_corrected_indexes(corrected_raw)
    turnedintovecs_c1 = Vector{String}.(eachrow(corrected_raw))
    filterothers = filter(x->all(y-> y != "other", x), turnedintovecs_c1)
    get_index_candidate(candidate,vecss) = map(y->findfirst(x-> x== candidate,y), vecss)
    DataFrame([(candidate=>get_index_candidate(candidate,filterothers)) 
    for candidate in candidates]...)
end

get_corrected_indexes(mincw1)



for (fn,f) in zip(map(x->"corrected_indexes_$x.csv", 1:4),
     [mincw1,mincw2,mincw3,mincw4])
     CSV.write(dfspath * fn, get_corrected_indexes(f))
end


