import Pkg
Pkg.activate("./")

using Combinatorics
using CWElectionsBR
using Chain
using DataFrames
using Distances 
using NamedArrays
using RCall 
import CSV


@rlibrary rankdist 

@rimport tidyr
@rimport votesys

reval("load('../rscripts/dta_objects/freq_ranks_inferred.RData')")
@rget freq_ranks_inferred
dfspath = "../rscripts/dfs/"


other_proxy =  DataFrame(Dict("1"=>"other", "2"=>"other", "3"=>"other", "4"=>"other", "freq"=>228.))

append!(freq_ranks_inferred, other_proxy)

prop_df = @chain  freq_ranks_inferred begin 
    groupby(:1)
    combine(:freq => sum )
    sort(:1)
end


prop_df[!, :prop] =  map(x-> round(x/sum(prop_df[!, :freq_sum]),digits= 3), prop_df[!, :freq_sum])
prop_df[!,:1]= string.(prop_df[!,:1])

sort!(prop_df,"1")

#= 
Actual voting tally proportions: bolsonaro:haddad:ciro:alckmin:other = 46.3:29.28:12.47:4.76:7.19

What I have: 36.7:24.7:17.3:14.1:7.19  =#


actual_first_round = @chain begin 
    zip(["bolsonaro", "haddad", "ciro", "alckmin", "other"], 
        [0.463, 0.2928, 0.1247, 0.0476, 0.072])    
    DataFrame
    sort(:1)
    rename(:1=> :candidates, :2 => :actual_proportions)
end

prop_df[!, :actual_proportions] = actual_first_round[!,:actual_proportions]

overvoted = prop_df[prop_df[!, :prop] .> prop_df[!, :actual_proportions],:]

undervoted = prop_df[prop_df[!, :prop] .< prop_df[!, :actual_proportions],:]

prop_df[!, :dist_proportion] = (prop_df[!,:prop] .- prop_df[!,:actual_proportions] .|>
x-> round(x, digits = 3) .|> 
 abs)

total_tallies = sum(prop_df[!, :freq_sum])
prop_df[!, :dist_freqs] = round.(prop_df[!,:dist_proportion] .*  total_tallies, digits = 0)


function make_base_split_freq_ranks(candidate, freq_ranks)
    filter(x-> x[:1] == candidate, freq_ranks)
end

overcandidates = [j[1] for j in eachrow(overvoted)]
undercandidates = [j[1] for j in eachrow(undervoted)]

discrepancy(candidate) = Int(first(filter(x-> x[:1] == candidate, prop_df)[!,:dist_freqs]))    

    
                        
function preprocess_over_under!(overcandidate, undercandidate,
     over_freq_ranks= over_freq_rankss[overcandidate]["candidate_freq_rank"],
     under_freq_ranks= under_freq_rankss[undercandidate]["candidate_freq_rank"])
     
    under_freq_ranks[!, :dist_to_over] = [(findfirst(x-> x == overcandidate, Vector(j[1:4]))-1) for j in eachrow(under_freq_ranks)]
    over_freq_ranks[!, :dist_to_under] = [(findfirst(x-> x == undercandidate, Vector(j[1:4]))-1) for j in eachrow(over_freq_ranks)]
    over_freq_rankss[overcandidate]["candidate_freq_rank"] = sort(over_freq_ranks, :dist_to_under)
end


#last_giving_index = findfirst(x->x>= over_can_give, cumsum(over_freq_ranks[!,:freq]))

function dist_sourcerank_targetranks(sourcerank, targetranks)
[ Int(rcopy(DistancePair(Vector(sourcerank[1:4]),j)))
for j in  (map(x-> Vector(x[1:4]), eachrow(targetranks)))] 
end

function find_targetRankIndex_given_sourceRank(source_rank, target_ranks)
    dist_sourcerank_targetranks(source_rank, target_ranks) |> argmin  
end    

function transfer_rankings(source_rankings, target_rankings, can_give, must_receive)
    given = 0
    giving_bound = copy(can_give)
    needs_to_give = copy(must_receive)

    source_rankings_copy = copy(source_rankings)
    target_rankings_copy = copy(target_rankings)

    for source_rank in eachrow(source_rankings_copy)
        row_can_give = source_rank[:freq]
        if needs_to_give == 0
            will_give = 0 
        else 
            will_give = min(row_can_give, needs_to_give, giving_bound)
        end
        
        target_index = find_targetRankIndex_given_sourceRank(source_rank, target_rankings_copy)
        source_rank[:freq] -= will_give 
        
        target_rankings_copy[target_index, :freq] += will_give
        given += will_give 
        needs_to_give -= will_give
        giving_bound -= will_give
    end
    
    yet_needed = must_receive - given 

    source_can_still_give = can_give - given 

    return(updated_source = source_rankings_copy, 
    updated_target = target_rankings_copy,
     yet_needed = yet_needed,
     source_can_still_give = source_can_still_give)
end


function update_dicts!(over_freq_ranks, under_freq_ranks, valueholder)                        
    over_freq_ranks["candidate_freq_rank"] = valueholder.updated_source
    under_freq_ranks["candidate_freq_rank"] = valueholder.updated_target
    over_freq_ranks["can_transfer"] = valueholder.source_can_still_give
    under_freq_ranks["needs_transfer"] = valueholder.yet_needed
end


function make_over_rankss(overcandidates,freq_ranks_inferred)
    over_freq_rankss = Dict(zip(overcandidates,
                              [Dict("candidate_freq_rank" => make_base_split_freq_ranks(j, freq_ranks_inferred), 
                              "can_transfer" => discrepancy(j)) for j in overcandidates]))
                          return(over_freq_rankss)
end

function make_under_rankss(undercandidates,freq_ranks_inferred)
    under_freq_rankss = Dict(zip(undercandidates,
    [Dict("candidate_freq_rank" => make_base_split_freq_ranks(j, freq_ranks_inferred), 
    "needs_transfer" => discrepancy(j))
                        for j in undercandidates]))
                return(under_freq_rankss)
end


function transfer!(candidate_to_give, candidate_to_receive, 
        over_freq_rankss,
        under_freq_rankss)                        
    preprocess_over_under!(candidate_to_give, candidate_to_receive)
    bar = transfer_rankings(over_freq_rankss[candidate_to_give]["candidate_freq_rank"],
                        under_freq_rankss[candidate_to_receive]["candidate_freq_rank"],
                        over_freq_rankss[candidate_to_give]["can_transfer"],
                        under_freq_rankss[candidate_to_receive]["needs_transfer"])
   update_dicts!(over_freq_rankss[candidate_to_give],
             under_freq_rankss[candidate_to_receive], 
             bar)
end



function get_new_prop_from_mutated_dict(merged_result,total_tallies)
    newpropdf= [(candidate,
    round(sum(merged_result[candidate]["candidate_freq_rank"][!,:freq])/total_tallies,
     digits = 3)) 
     for candidate in keys(merged_result)] |> DataFrame
      rename!(newpropdf, Dict("1" => "candidates", "2" => "new_proportions"))
    otherdf = DataFrame(:candidates => "other",
                        :new_proportions => round(1-(newpropdf[!,:new_proportions] |> sum),digits = 3))
    append!(newpropdf, otherdf)
    sort!(newpropdf, :candidates)
    return(newpropdf)
end


function sweep_transfer(undercandidates,overcandidates,
                        freq_ranks_inferred = freq_ranks_inferred,
                        total_tallies=total_tallies,
                        prop_df = prop_df)
    transferss_acc = []                        
    pab = ("alckmin", "bolsonaro")
    pah = ("alckmin", "haddad")
    pcb = ("ciro", "bolsonaro")
    pch = ("ciro", "haddad")
    perms = permutations([pab,pah,pcb,pch])
    
    for perm in perms 
        under_freq_rankss = make_under_rankss(undercandidates,freq_ranks_inferred)
        over_freq_rankss = make_over_rankss(overcandidates, freq_ranks_inferred)    
        for pair in perm 
            transfer!(pair..., over_freq_rankss,under_freq_rankss)    
        end
        merged_result  = merge(under_freq_rankss,over_freq_rankss)

        newprops= get_new_prop_from_mutated_dict(merged_result,total_tallies)
        eudist= euclidean(prop_df[!,:actual_proportions], newprops[!,:new_proportions])
        transfers_info = Dict(:permutation => perm , :transfer_dicts => merged_result , 
        :newprops=>newprops, :eudist_to_target => eudist)
        push!(transferss_acc, transfers_info)
    end
    return(transferss_acc)
end


transferss = sweep_transfer(undercandidates,overcandidates)

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




