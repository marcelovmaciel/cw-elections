import Pkg
Pkg.activate("./")

using CWElectionsBR
using Chain
using DataFrames
using NamedArrays
using RCall 

@rlibrary rankdist 

reval("load('../rscripts/dta_objects/freq_ranks_inferred.RData')")
@rget freq_ranks_inferred



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

over_freq_rankss = Dict(zip(overcandidates,
                          [Dict("candidate_freq_rank" => make_base_split_freq_ranks(j, freq_ranks_inferred), 
                          "can_transfer" => discrepancy(j))
                          for j in overcandidates]))
                          
under_freq_rankss = Dict(zip(undercandidates,
                        [Dict("candidate_freq_rank" => make_base_split_freq_ranks(j, freq_ranks_inferred), 
                        "needs_transfer" => discrepancy(j))
                        for j in undercandidates]))



function global_transfer!(candidate_to_give, candidate_to_receive)                        
    preprocess_over_under!(candidate_to_give, candidate_to_receive)
    bar = transfer_rankings(over_freq_rankss[candidate_to_give]["candidate_freq_rank"],
                        under_freq_rankss[candidate_to_receive]["candidate_freq_rank"],
                        over_freq_rankss[candidate_to_give]["can_transfer"],
                        under_freq_rankss[candidate_to_receive]["needs_transfer"])
   update_dicts!(over_freq_rankss[candidate_to_give],
             under_freq_rankss[candidate_to_receive], 
             bar)
end

over_freq_rankss["ciro"]

under_freq_rankss["haddad"]

global_transfer!("ciro", "haddad")

over_freq_rankss["ciro"]

under_freq_rankss["haddad"]

under_freq_rankss["bolsonaro"]

global_transfer!("ciro", "bolsonaro")

global_transfer!("alckmin", "bolsonaro")

# ISSUE: in this sequence, bolsonaro still needs 4 votes. take them from others  

# sum(bar.updated_target[!,:freq])/total_tallies



# Another sequence 

over_freq_rankss["ciro"]

under_freq_rankss["bolsonaro"]

global_transfer!("ciro", "bolsonaro")

over_freq_rankss["ciro"]

under_freq_rankss["haddad"]



global_transfer!("ciro", "haddad")

global_transfer!("alckmin", "bolsonaro")

global_transfer!("alckmin", "haddad")

# Sanity checking 

prop_df

for candidate in ["alckmin", "bolsonaro", "ciro", "haddad"]
try
    println(candidate, " ", sum(under_freq_rankss[candidate]["candidate_freq_rank"][!,:freq])/total_tallies)        
catch
    println(candidate, " ", sum(over_freq_rankss[candidate]["candidate_freq_rank"][!,:freq])/total_tallies)        
end    
end



