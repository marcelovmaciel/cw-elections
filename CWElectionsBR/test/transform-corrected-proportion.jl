import Pkg

Pkg.activate("../../CWElectionsBR")

using CWElectionsBR
using DataFrames
using RCall
import CSV
using DataFrames
import Base.Filesystem as fl
using Pipe
using FloatingTableView
@rimport tidyr

reval("load('../rscripts/dta_objects/corrected_freq_ranks.RData')")



@rget corrected_freq_ranks


corrected_freq_ranks


acc = []

for i in 1:25
    push!(acc, Vector(corrected_freq_ranks[i,1:4]))
end

foo = DataFrame(ranking_vectors = acc,
          freq  = corrected_freq_ranks[!, "freq"] )

# foo.ranking_vectors =  map(x-> join(x, " "),foo[!, :ranking_vectors])



@rput foo


corrected_freq_ranks

baz = tidyr.uncount(foo, foo.freq) |> rcopy

baz[!,1]


dfspath= "../../rscripts/dfs/"

CSV.write(dfspath * "corrected_freq_raw.csv",baz[!,1])
