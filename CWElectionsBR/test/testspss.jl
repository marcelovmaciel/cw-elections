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

dfspath= "../../rscripts/dfs/"

foo = CSV.read(dfspath * "freq_ranks_inferred.csv",DataFrame)


browse(foo)


drop_candidate(acc, candidate) = map(a->filter(x->x≠ candidate,a), acc)

function freq_without_candidate(candidate, df )

    acc = []
    for i in 1:24 
    push!(acc, Vector(df[i,1:4]))    
    end    
    @pipe (DataFrame(ranking_vectors =  drop_candidate(acc,candidate),
                        freq = foo[!,"freq"] ) |> 
                        groupby(_, :ranking_vectors) |>
                        combine(_, :freq => sum))
end

function make_pretty_ranking_vector(df)
    df.ranking_vectors = map(x-> join(x, " > "),df[!, :ranking_vectors])    
    return(df)
end

function finaldf_without_candidate(candidate,df) 
    df = make_pretty_ranking_vector(freq_without_candidate(candidate, df))
    combine(df,:ranking_vectors => :ranking_vectors,
                :freq_sum => (x -> x / sum(x)) => :prop)
    
end    

noalckmin_df = finaldf_without_candidate("alckmin", foo)
nohaddad_df = finaldf_without_candidate("haddad", foo)
nociro_df = finaldf_without_candidate("ciro", foo)
nobolsonaro_df = finaldf_without_candidate("bolsonaro", foo) 

nobolsonaro_df

browse(noalckmin_df) 

browse(nohaddad_df) 

browse(nociro_df) 

browse(nobolsonaro_df) 




CSV.write(dfspath * "noalckmin_df.csv", noalckmin_df)
CSV.write(dfspath * "nohaddad_df.csv", nohaddad_df)
CSV.write(dfspath * "nociro_df.csv", nociro_df)
CSV.write(dfspath * "nobolsonaro_df.csv", nobolsonaro_df)
