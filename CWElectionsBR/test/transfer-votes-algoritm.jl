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

ideological_ordering = ["haddad", "ciro", "alckmin", "bolsonaro"]

over1 = overvoted[1,:][1]

under1 = undervoted[1,:][1]

over1_freq_ranks  = filter(x-> x[:1] == over1, freq_ranks_inferred)

under1_freq_ranks  = filter(x-> x[:1] == under1, freq_ranks_inferred)

findfirst(x-> x == under1, Vector(over1_freq_ranks[1,:][1:4]))

over1_freq_ranks[!, :dist_to_under] = [(findfirst(x-> x == under1, Vector(j[1:4]))-1) for j in eachrow(over1_freq_ranks)]

under1_freq_ranks[!, :dist_to_over] = [(findfirst(x-> x == over1, Vector(j[1:4]))-1) for j in eachrow(under1_freq_ranks)]

#prop_df[!,:dist_proportion] = 

prop_df[!, :dist_proportion] = (prop_df[!,:prop] .- prop_df[!,:actual_proportions] .|>
x-> round(x, digits = 3) .|> 
 abs)

total_tallies = sum(prop_df[!, :freq_sum])

prop_df[!, :dist_freqs] = round.(prop_df[!,:dist_proportion] .*  total_tallies, digits = 0)

under1_needs = Int(first(filter(x-> x[:1] == under1, prop_df)[!,:dist_freqs]))
over1_can_give = Int(first(filter(x-> x[:1] == over1, prop_df)[!,:dist_freqs]))


over1_freq_ranks = sort(over1_freq_ranks, :dist_to_under)

last_giving_index = findfirst(x->x>= over1_can_give, cumsum(over1_freq_ranks[!,:freq]))

receiving_rankings = copy(under1_freq_ranks)

over1_freq_ranks


giving_rankings


function dist_sourcerank_targetranks(sourcerank, targetranks)
[ Int(rcopy(DistancePair(Vector(sourcerank[1:4]),j)))
for j in  (map(x-> Vector(x[1:4]), eachrow(targetranks)))]
end


function find_targetRankIndex_given_sourceRank(source_rank, target_ranks)
    dist_sourcerank_targetranks(source_rank, target_ranks) |> x-> findfirst(foo-> foo == 1, x)    
end    



function transfer_rankings(source_rankings, target_rankings, can_give, must_receive)
    given = 0
    needs_to_give = can_give
    source_rankings_copy = copy(source_rankings)
    target_rankings_copy = copy(target_rankings)
    for source_rank in eachrow(source_rankings_copy)
        row_can_give = source_rank[:freq]
        if row_can_give > needs_to_give
        will_give = needs_to_give
        else 
            will_give = row_can_give
        end
        target_index = find_targetRankIndex_given_sourceRank(source_rank, target_rankings_copy)
        source_rank[:freq] -= will_give 
        target_rankings_copy[target_index, :freq] += will_give
        given += will_give 
        needs_to_give -= will_give
    end
    yet_needed = must_receive - given 
    return(updated_source = source_rankings_copy, updated_target = target_rankings_copy, yet_needed = yet_needed)
end

giving_rankings

bar = transfer_rankings(over1_freq_ranks,
    receiving_rankings,
    over1_can_give,
    under1_needs)


sum(bar.updated_source[!,:freq])/total_tallies


over1_freq_ranks
giving_rankings
receiving_rankings
