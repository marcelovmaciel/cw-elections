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


reval("load('../../rscripts/dta_objects/global_pairwise_comparisons.RData')")

@rget global_pairwise_comparisons 

global_pairwise_comparisons[1]

function renaming(x)
    if x == "3"
    "ciro"
    elseif x == "9"
        "bolsonaro"
    elseif x == "5"
        "haddad"
    elseif x == "6"
        "alckmin"
    else 
        "white_null"
    end    
end    

global_pairwise_comparisons[1]

function number_to_candidate_col!(x)
      x[!, :Var1] = renaming.(x[!, :Var1])
end

foreach(number_to_candidate_col!, global_pairwise_comparisons)
global_pairwise_comparisons = map(x->sort(x, :Freq, rev = true  ),
 global_pairwise_comparisons)

pairwise_winners  = map(x-> x[1, :Var1], global_pairwise_comparisons)
pairwise_losers =  map(x-> x[2, :Var1], global_pairwise_comparisons)
margins  =  map(x-> round((x[1, :Freq] - x[2, :Freq]) * 100;
digits = 3 ), global_pairwise_comparisons)

pairwise_table_raw = DataFrame(pairwise_winners=pairwise_winners, 
pairwise_losers = pairwise_losers, margins= margins)

CSV.write("../../rscripts/dfs/pairwise_table_raw.csv",pairwise_table_raw )
