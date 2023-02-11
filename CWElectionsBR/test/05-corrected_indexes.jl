

import Pkg
Pkg.activate("./")

import  CWElectionsBR as cw
using RCall
import CSV
using DataFrames
dfspath = "../rscripts/dfs/"


candidates = cw.candidates

dfs_names = readdir(cw.dfspath)

imputted_poly = filter(x->occursin("poly_imp_min_raw",x), dfs_names)
imp_poly_dfs = map(x->cw.CSV.read(cw.dfspath * x, DataFrame), imputted_poly)

imputted_poly[1]


function get_corrected_indexes(corrected_raw)
    turnedintovecs_c1 = Vector{String}.(eachrow(corrected_raw))
    filterothers = filter(x->all(y-> y != "other", x), turnedintovecs_c1)
    get_index_candidate(candidate,vecss) = map(y->findfirst(x-> x== candidate,y), vecss)
    DataFrame([(candidate=>get_index_candidate(candidate,filterothers)) 
    for candidate in candidates]...)
end

corrected_stuff = map(get_corrected_indexes,imp_poly_dfs)

for (index,file) in enumerate(corrected_stuff)
    CSV.write(dfspath * "corrected_indexes_$index.csv", file)
end    
