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
using PrettyTables

dfspath= "../../rscripts/dfs/"

foo = CSV.read(dfspath * "freq_ranks_inferred.csv",DataFrame)

browse(foo)

drop_candidate(acc, candidate) = map(a->filter(x->x≠ candidate,a), acc)

function freq_without_candidate(candidate, df )

    acc = []
    for i in 1:size(df)[1]
    push!(acc, Vector(df[i,1:4]))    
    end    
    @pipe (DataFrame(ranking_vectors =  drop_candidate(acc,candidate),
                        freq = df[!,"freq"] ) |>
                        groupby(_, :ranking_vectors) |>
                        combine(_, :freq => sum))
end


function make_pretty_ranking_vector(df)
    df.ranking_vectors = map(x-> join(x, " > "),df[!, :ranking_vectors])    
    return(df)
end


function cleaned_df(df)
    acc = []
    for i in 1:24 
    push!(acc, Vector(df[i,1:4]))    
    end
    @pipe (DataFrame(ranking_vectors = acc,
    freq = df[!,"freq"] ) |> 
    groupby(_, :ranking_vectors) |>
    combine(_, :freq => sum) |> 
    make_pretty_ranking_vector(_) |>    
    combine(_,:ranking_vectors => :ranking_vectors, 
    :freq_sum => (x -> round(100 * x / sum(x),digits = 2)) => :prop)
    )
end    

@pipe cleaned_df(foo)  |> pretty_table(_, backend = :latex)


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





reval("load('../../rscripts/dta_objects/corrected_freq_ranks.RData')")

@rget corrected_freq_ranks

print(corrected_freq_ranks)

noalckmin_df = finaldf_without_candidate("alckmin", corrected_freq_ranks)

freq_without_candidate("alckmin", corrected_freq_ranks)


nohaddad_df = finaldf_without_candidate("haddad", corrected_freq_ranks)
nociro_df = finaldf_without_candidate("ciro", corrected_freq_ranks)
nobolsonaro_df = finaldf_without_candidate("bolsonaro", corrected_freq_ranks)
