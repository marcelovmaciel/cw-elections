import Pkg

Pkg.activate(".")

import CWElectionsBR as cw
using DataFrames
import Base.Filesystem as fl 
using Pipe 


dfspath = "../rscripts/dfs/"

#simpler_rankings = cw.CSV.read(dfspath * "simpler_rankings.csv", cw.DataFrame)

function make_freq_ranks_inferred(df)
    orderings = @pipe (
    DataFrame(ranking_vectors = 
    [string.(map(y->findfirst(x->x==y, r), [1,2,3,4]))
             for r in eachrow(df)]) |>
             groupby(_, :ranking_vectors) |>
             combine(_, nrow => :count) |>
              rename(_, :count => :freq) |> 
              sort(_, :freq, rev = true ) )
    append!(orderings,
                        DataFrame(Dict("ranking_vectors" => [["other", "other", "other","other"]], 
                        "freq" => 228 )))          
    orderings.prop = map(x -> round(x / sum(orderings.freq), digits = 3),
                        orderings.freq)
    
    c1,c2,c3,c4  = map(y->map(x->x[y], orderings.ranking_vectors), 1:4)
    freq_ranks_inferred = DataFrame(Dict("1"=> c1,
                                         "2" => c2,
                                         "3" => c3,
                                         "4" => c4,
                                         "freq"=> orderings.freq,
                                         "prop" => orderings.prop))
    return(orderings,freq_ranks_inferred)
end                                     

dfs_names = readdir(dfspath)
imputted_dfs = filter(x->occursin("imputted",x), dfs_names)


for imp in imputted_dfs
    df_base_name =  fl.splitext(imp)[1]
    dfname = split(df_base_name, "_")[2] * split(split(df_base_name, "_")[3],".")[1]
    dfpath = joinpath(dfspath,imp)
    df = cw.CSV.read(dfpath, cw.DataFrame)
    orderingname = dfname * "_oi.csv"
    freq_ranks_inferred_name = dfname * "_fri.csv"
    oi,fri = make_freq_ranks_inferred(df)    
    cw.CSV.write("../rscripts/dfs/" * orderingname,oi )
    cw.CSV.write("../rscripts/dfs/" * freq_ranks_inferred_name, fri)
end    




##cw.CSV.write("../rscripts/dfs/orderings_inferred.csv",orderings )
##cw.CSV.write("../rscripts/dfs/freq_ranks_inferred.csv",freq_ranks_inferred )


## BUG: fixing the props bug I've introduced 

#= cart_imp = cw.CSV.read(cw.dfspath * "imputted_polyreg_1.csv", cw.DataFrame)


orderings = @pipe (
    DataFrame(ranking_vectors = 
    [string.(map(y->findfirst(x->x==y, r), [1,2,3,4]))
             for r in eachrow(cart_imp)]) |>
             groupby(_, :ranking_vectors) |>
             combine(_, nrow => :count) |>
              rename(_, :count => :freq) |> 
              sort(_, :freq, rev = true ) )
              

orderings |> first |> println

append!(orderings,
 DataFrame(Dict("ranking_vectors" => [["other", "other", "other","other"]], 
 "freq" => 228 )))

DataFrame(Dict("ranking_vectors" => [["other", "other", "other","other"]], 
"freq" => 228 ))



dfs_names = readdir(cw.dfspath)
oi_dfs = filter(x->occursin("_oi",x), dfs_names)
fri_dfs = filter(x->occursin("_fri",x), dfs_names)
for imp in oi_dfs 
    dfpath = joinpath(cw.dfspath,imp)
    rm(dfpath)
end

for imp in fri_dfs 
    dfpath = joinpath(cw.dfspath,imp)
    rm(dfpath)
end =#





