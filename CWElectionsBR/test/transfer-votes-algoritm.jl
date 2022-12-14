import Pkg
using Revise 

Pkg.activate("./")



using CWElectionsBR
#= 
using DataFrames 
import CSV

freq_ranks_inferred = CSV.read("../rscripts/dfs/" * "freq_ranks_inferred.csv", DataFrame)

names(freq_ranks_inferred) =#


CWElectionsBR.undercandidates

transferss = sweep_transfer(CWElectionsBR.undercandidates,CWElectionsBR.overcandidates)

transferss[1]

map(x->x[:eudist_to_target],transferss) |> CWElectionsBR.countmap


dists = map(x->x[:eudist_to_target], transferss) 

minimum_transfer_indexes = findall(x-> x== findmin(dists)[1], dists)

minimum_transfers =  transferss[minimum_transfer_indexes]

for x in minimum_transfers
    x[:transferred_df] = reduce(vcat, map(x->x["candidate_freq_rank"], values(x[:transfer_dicts])))
end


findall(y->  y==map(x->x[:transferred_df], minimum_transfers)[1],
   map(x->x[:transferred_df], minimum_transfers))
   
findall(y->  y==map(x->x[:transferred_df], minimum_transfers)[4],
map(x->x[:transferred_df], minimum_transfers))   


# So, there are 6 transfers that minimize the euclidean Distance 
# However, the first three lead to the same positional matrix 
# While the last 3 lead to another positional matrix.
# Thus, we have two different transfers that minimize the euclidean distance to the top proportion 
# My intuition that this was necessary was correct. 

# Another sequence 

minimum_transfers[1][:transferred_df] |> println

minimum_transfers[4][:transferred_df] |> println 

min_transfer_c1 = minimum_transfers[1][:transferred_df]

#append!(min_transfer_c1, other_proxy)

min_transfer_c2 = minimum_transfers[4][:transferred_df]

#append!(min_transfer_c2, other_proxy)

CSV.write(dfspath * "min_transfer_c1.csv", min_transfer_c1)
CSV.write(dfspath * "min_transfer_c2.csv", min_transfer_c2)


min_transfer_c1


function glue_candidates_into_single_vec(df)
    acc = []

    for i in 1:25
        push!(acc, Vector(df[i,1:4]))
    end
    
    foo = DataFrame(ranking_vectors = acc,
              freq  = df[!, "freq"] )
              return(foo)
end    

min_c1_glue_vecs = glue_candidates_into_single_vec(min_transfer_c1)
min_c2_glue_vecs = glue_candidates_into_single_vec(min_transfer_c2)

@rput min_c1_glue_vecs
@rput min_c2_glue_vecs




CSV.write(dfspath * "min_c1_glueVecs.csv", min_c1_glue_vecs)
CSV.write(dfspath * "min_c2_glueVecs.csv", min_c2_glue_vecs)


min_c1_glue_vecs



min_c1_raw = tidyr.uncount(min_c1_glue_vecs,  min_c1_glue_vecs.freq) |> rcopy
min_c2_raw = tidyr.uncount(min_c2_glue_vecs,  min_c2_glue_vecs.freq) |> rcopy






min_c1_raw_cleaned = begin 
    DataFrame(:choice1 => map(x->x[1], min_c1_raw[!,:ranking_vectors]),
         :choice2 => map(x->x[2], min_c1_raw[!,:ranking_vectors]),
         :choice3 => map(x->x[3], min_c1_raw[!,:ranking_vectors]),
         :choice4 => map(x->x[4], min_c1_raw[!,:ranking_vectors]))    
end    

min_c2_raw_cleaned = begin 
    DataFrame(:choice1 => map(x->x[1], min_c2_raw[!,:ranking_vectors]),
         :choice2 => map(x->x[2], min_c2_raw[!,:ranking_vectors]),
         :choice3 => map(x->x[3], min_c2_raw[!,:ranking_vectors]),
         :choice4 => map(x->x[4], min_c2_raw[!,:ranking_vectors]))    
end    




CSV.write(dfspath * "min_c1_raw.csv", min_c1_raw_cleaned)
CSV.write(dfspath * "min_c2_raw.csv", min_c2_raw_cleaned)



# Here I'll test other stuff --------------------------------------------------------------------------------------------


# Sanity checking 




