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

over1 = overvoted[2,:][1]

overvoted


under1 = undervoted[1,:][1]




function preprocess_over_under(freq_ranks_inferred, prop_df, overcandidate, undercandidate)
    over_freq_ranks  = filter(x-> x[:1] == overcandidate, freq_ranks_inferred)
    under_freq_ranks  = filter(x-> x[:1] == undercandidate, freq_ranks_inferred)

    over_freq_ranks[!, :dist_to_under] = [(findfirst(x-> x == undercandidate, Vector(j[1:4]))-1) for j in eachrow(over_freq_ranks)]
    under_freq_ranks[!, :dist_to_over] = [(findfirst(x-> x == overcandidate, Vector(j[1:4]))-1) for j in eachrow(under_freq_ranks)]
    

    over_can_give = Int(first(filter(x-> x[:1] == overcandidate, prop_df)[!,:dist_freqs]))    
    under_needs = Int(first(filter(x-> x[:1] == undercandidate, prop_df)[!,:dist_freqs]))

    over_freq_ranks = sort(over_freq_ranks, :dist_to_under)
    return(over_freq_ranks = over_freq_ranks,
           under_freq_ranks = under_freq_ranks,
           over_can_give = over_can_give,
           under_needs = under_needs)
end

foo = preprocess_over_under(freq_ranks_inferred,prop_df,over1,under1)






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
    needs_to_give = copy(can_give)
    source_rankings_copy = copy(source_rankings)
    target_rankings_copy = copy(target_rankings)
    for source_rank in eachrow(source_rankings_copy)
        row_can_give = source_rank[:freq]
        if row_can_give > needs_to_give
        will_give = needs_to_give
        else 
            will_give = row_can_give
        end
        println(source_rank)
        println(target_rankings_copy)
        target_index = find_targetRankIndex_given_sourceRank(source_rank, target_rankings_copy)
        source_rank[:freq] -= will_give 
        println(target_index)
        target_rankings_copy[target_index, :freq] += will_give
        given += will_give 
        needs_to_give -= will_give
    end
    yet_needed = must_receive - given 
    source_can_still_give = can_give - given 
    return(updated_source = source_rankings_copy, 
    updated_target = target_rankings_copy,
     yet_needed = yet_needed,
     source_can_still_give = source_can_still_give)
end



foo.over_freq_ranks


bar = transfer_rankings(foo.over_freq_ranks,
                        foo.under_freq_ranks,
                        foo.over_can_give,
                        foo.under_needs)




foo.over_can_give                        

foo.under_needs 


bar.yet_needed                        

bar.updated_source
                        
bar.source_can_still_give                        
                        
sum(bar.updated_target[!,:freq])/total_tallies

