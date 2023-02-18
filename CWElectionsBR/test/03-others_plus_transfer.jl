import Pkg

Pkg.activate(".")

using Revise 

import CWElectionsBR as cw
using DataFrames
import Base.Filesystem as fl 
using Pipe 
using Chain
using RCall


@rimport tidyr

actual_first_round = @chain begin
    zip(["Bolsonaro", "Haddad", "Ciro", "Alckmin", "other"],
        [0.463, 0.2928, 0.1247, 0.0476, 0.072])
    DataFrame
    sort(:1)
    rename(:1=> :candidates, :2 => :actual_proportions)
end


function makeDict_infos_freq(freq_ranks_inferred)
    prop_df = @chain  freq_ranks_inferred begin
        groupby(:1)
        combine(:freq => sum )
        sort(:1)
    end
    
    prop_df[!, :prop] =  map(x-> round(x/sum(prop_df[!, :freq_sum]),digits= 4),
    prop_df[!, :freq_sum])
    prop_df[!,:1]= string.(prop_df[!,:1])
    sort!(prop_df,"1")
    prop_df[!, :actual_proportions] = actual_first_round[!,:actual_proportions]
    
    overvoted = prop_df[prop_df[!, :prop] .> prop_df[!, :actual_proportions],:]
    undervoted = prop_df[prop_df[!, :prop] .< prop_df[!, :actual_proportions],:]

    prop_df[!, :dist_proportion] = (prop_df[!,:prop] .- prop_df[!,:actual_proportions] .|>
    x-> round(x, digits = 3) .|> abs)

    total_tallies = sum(prop_df[!, :freq_sum])

    prop_df[!, :dist_freqs] = round.(prop_df[!,:dist_proportion] .*  total_tallies, digits = 0)

    overcandidates = [j[1] for j in eachrow(overvoted)]
    undercandidates = [j[1] for j in eachrow(undervoted)]

    infos_fri = Dict("prop_df" => prop_df,
                             "total_tallies" => total_tallies, 
                             "overvoted" => overvoted,
                             "undervoted" => undervoted, 
                             "overcandidates" => overcandidates,
                             "undercandidates" => undercandidates)
    return(infos_fri)
end



#=
Actual voting tally proportions: bolsonaro:haddad:ciro:alckmin:other = 46.3:29.28:12.47:4.76:7.19

What I have: 36.7:24.7:17.3:14.1:7.19  =#

function make_base_split_freq_ranks(candidate, freq_ranks)
    filter(x-> x[:1] == candidate, freq_ranks)
end
 

discrepancy(candidate,info) = Int(first(filter(x-> x[:1] == candidate, info["prop_df"])[!,:dist_freqs]))


function preprocess_over_under!(overcandidate,
                               undercandidate, over_freq_rankss,under_freq_rankss)

    over_freq_ranks= over_freq_rankss[overcandidate]["candidate_freq_rank"]
    under_freq_ranks = under_freq_rankss[undercandidate]["candidate_freq_rank"]
    under_freq_ranks[!, :dist_to_over] = [(findfirst(x-> x == overcandidate, Vector(j[1:4]))-1) for j in eachrow(under_freq_ranks)]
    over_freq_ranks[!, :dist_to_under] = [(findfirst(x-> x == undercandidate, Vector(j[1:4]))-1) for j in eachrow(over_freq_ranks)]
    over_freq_rankss[overcandidate]["candidate_freq_rank"] = sort(over_freq_ranks, :dist_to_under)
end


#last_giving_index = findfirst(x->x>= over_can_give, cumsum(over_freq_ranks[!,:freq]))

## In which I actually define the transfer 


function dist_sourcerank_targetranks(sourcerank, targetranks)

    # BUG: this shit is giving me a type error. It was not !!!
     rankdist = cw.rimport("rankdist")
    function candidate_list_into_number(candidate_list)
    # TODO: check if this doesn't lead to an error, compare to older result
    candidate_number = Dict(zip(["Alckmin","Bolsonaro", "Ciro", "Haddad"], collect(1:4)))
        # println(candidate_list)
        map(x-> candidate_number[x], String.(candidate_list))
    end
    #robject(Vector(sourcerank[1:4])) |> println
[ Int(cw.rcopy(rankdist.DistancePair(candidate_list_into_number(Vector(sourcerank[1:4]) ),
                                  candidate_list_into_number(j))))
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


function make_over_rankss(overcandidates,freq_ranks_inferred,info)
    over_freq_rankss = Dict(zip(overcandidates,
                              [Dict("candidate_freq_rank" => make_base_split_freq_ranks(j, freq_ranks_inferred),
                              "can_transfer" => discrepancy(j,info)) for j in overcandidates]))
                          return(over_freq_rankss)
end

function make_under_rankss(undercandidates,freq_ranks_inferred, info)
    under_freq_rankss = Dict(zip(undercandidates,
    [Dict("candidate_freq_rank" => make_base_split_freq_ranks(j, freq_ranks_inferred),
    "needs_transfer" => discrepancy(j,info))
                        for j in undercandidates]))
                return(under_freq_rankss)
end

function transfer!(candidate_to_give,
                   candidate_to_receive,
                   over_freq_rankss,
                   under_freq_rankss)

    preprocess_over_under!(candidate_to_give,
                           candidate_to_receive,
                           over_freq_rankss,
                           under_freq_rankss)

    bar = transfer_rankings(over_freq_rankss[candidate_to_give]["candidate_freq_rank"],
                        under_freq_rankss[candidate_to_receive]["candidate_freq_rank"],
                        over_freq_rankss[candidate_to_give]["can_transfer"],
                        under_freq_rankss[candidate_to_receive]["needs_transfer"])
   update_dicts!(over_freq_rankss[candidate_to_give],
             under_freq_rankss[candidate_to_receive],
             bar)
end


function get_new_prop_from_mutated_dict(merged_result,total_tallies)
#    println(merged_result)
    #println(keys(merged_result))
    newpropdf= [(candidate,
    round(sum(merged_result[candidate]["candidate_freq_rank"][!,:freq])/total_tallies,
     digits = 4))
     for candidate in keys(merged_result)] |> DataFrame
      rename!(newpropdf, Dict("1" => "candidates", "2" => "new_proportions"))
    # TODO : check if this new_proportions is correct
  otherdf = DataFrame(:candidates => "other",
                        :new_proportions => round(1-(newpropdf[!,:new_proportions] |> sum),digits = 4))
    if length(newpropdf[!,:candidates]) == 4
                        append!(newpropdf, otherdf)
     end
 
    sort!(newpropdf, :candidates)
    return(newpropdf)
end


function sweep_transfer(info,df)

    undercandidates = info["undercandidates"]
    overcandidates = info["overcandidates"]
    freq_ranks_inferred = df 
    total_tallies = info["total_tallies"]
    prop_df = info["prop_df"]

    transferss_acc = []
    pab = ("Alckmin", "Bolsonaro")
    pah = ("Alckmin", "Haddad")
    pcb = ("Ciro", "Bolsonaro")
    pch = ("Ciro", "Haddad")
    perms = cw.permutations([pab,pah,pcb,pch])

    for perm in perms
        under_freq_rankss = make_under_rankss(undercandidates,freq_ranks_inferred, info)
        over_freq_rankss = make_over_rankss(overcandidates, freq_ranks_inferred,info)
        for pair in perm
            transfer!(pair..., over_freq_rankss,under_freq_rankss)
        end
        merged_result  = merge(under_freq_rankss,over_freq_rankss)

        newprops= get_new_prop_from_mutated_dict(merged_result,total_tallies)
        
        println("newprops:  ", newprops)

        eudist= cw.euclidean(prop_df[!,:actual_proportions], newprops[!,:new_proportions])
        println("eudist:  ", eudist)
        transfers_info = Dict(:permutation => perm , :transfer_dicts => merged_result ,
        :newprops=>newprops, :eudist_to_target => eudist)
        push!(transferss_acc, transfers_info)
    end
    return(transferss_acc)
end

## TODO: note the selection of the minimized transfers are still not here. Gotta write that 


function get_min_transfers(transferss)
    dists = map(x->x[:eudist_to_target], transferss) 
    minimum_transfer_indexes = findall(x-> x== findmin(dists)[1], dists)
    minimum_transfers =  transferss[minimum_transfer_indexes]

    for x in minimum_transfers
        x[:transferred_df] = reduce(vcat,
        map(x->(x["candidate_freq_rank"])[!,["1", "2", "3", "4", "freq", "prop"]],
        values(x[:transfer_dicts])))
    end

    mindfs = map(x->x[:transferred_df],minimum_transfers)
    unique_min_dfs = unique(mindfs)
    return(unique_min_dfs)
end

        
function glue_candidates_into_single_vec(df)
    acc = []

    for i in 1:size(df)[1]
        push!(acc, Vector(df[i,1:4]))
    end
    
    foo = DataFrame(ranking_vectors = acc,
              freq  = df[!, "freq"] )
              return(foo)
end    


function clean_raw(raw)
    DataFrame(:First => map(x->x[1], raw[!,:ranking_vectors]),
         :Second => map(x->x[2], raw[!,:ranking_vectors]),
         :Third => map(x->x[3], raw[!,:ranking_vectors]),
         :Fourth => map(x->x[4], raw[!,:ranking_vectors]))    
end    



function min_transfer_classes(df)
    info = makeDict_infos_freq(df)
    transferss = sweep_transfer(info,df)
    min_transfers = get_min_transfers(transferss)
    return(min_transfers)
end


function get_min_raws_cleaned(min_transfers) 
    min_glues =  map(glue_candidates_into_single_vec,min_transfers)
    min_raws = map(x-> (tidyr.uncount(x, x.freq) |> rcopy), 
    min_glues)
    min_raws_cleaned = map(clean_raw, 
                           min_raws)
    return(min_raws_cleaned)
end


function save_mins_csvs!(dfs,basename,appendname = "")
    for (fname,f) in zip(map(x-> appendname * basename * "_$x.csv",
         1:length(dfs)),
        dfs)
        cw.CSV.write(cw.dfspath * fname, f)
    end 
end


function transfer_and_save(dfname_toread, csvname_toappend)
    df = cw.CSV.read(cw.dfspath * dfname_toread,
    cw.DataFrame)
    
    min_transfers = min_transfer_classes(df)
    
    min_raws_cleaned = get_min_raws_cleaned(min_transfers) 
    
    save_mins_csvs!(min_raws_cleaned, "min_raw", csvname_toappend)
    save_mins_csvs!(min_transfers, "min_transfer", csvname_toappend)
    
end


## Actual computation starts here ----------------------------------------------------------------
dfs_names = readdir(cw.dfspath)



transfer_and_save("meanofmeansfri.csv", "avg_imp_")
transfer_and_save("avgpolyfri.csv", "poly_imp_")



 