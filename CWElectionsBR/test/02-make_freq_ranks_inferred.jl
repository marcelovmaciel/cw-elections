import Pkg

Pkg.activate(".")

import CWElectionsBR as cw
using DataFrames
import Base.Filesystem as fl 
using Pipe 




dfspath = "../rscripts/dfs/"

simpler_rankings = cw.CSV.read(dfspath * "simpler_rankings.csv", cw.DataFrame)

orderings = @pipe (
    DataFrame(ranking_vectors = 
    [string.(map(y->findfirst(x->x==y, r), [1,2,3,4]))
             for r in eachrow(simpler_rankings)]) |>
             groupby(_, :ranking_vectors) |>
             combine(_, nrow => :count) |>
              rename(_, :count => :freq) |> 
              sort(_, :freq, rev = true ) )

orderings.prop = map(x -> round(x / sum(orderings.freq), digits = 3),
 orderings.freq)


c1,c2,c3,c4  = map(y->map(x->x[y], orderings.ranking_vectors), 1:4)

freq_ranks_inferred = DataFrame(Dict("1"=> c1,
                                     "2" => c2,
                                     "3" => c3,
                                     "4" => c4,
                                     "freq"=> orderings.freq,
                                     "prop" => orderings.prop))

freq_ranks_inferred
 
cw.CSV.write("../rscripts/dfs/orderings_inferred.csv",orderings )


cw.CSV.write("../rscripts/dfs/freq_ranks_inferred.csv",freq_ranks_inferred )





 

