
import Pkg
Pkg.activate("./")

using CWElectionsBR
using RCall
import CSV
using DataFrames
dfspath = "../rscripts/dfs/"

plotspath ="../rscripts/plots/"




c_freq_raw = CSV.read(dfspath * "corrected_freq_raw.csv", DataFrame)
@rimport tidyr
reval("load('../rscripts/dta_objects/corrected_freq_ranks.RData')")
@rget corrected_freq_ranks

corrected_freq_ranks[!, :freq] = convert.(Int, corrected_freq_ranks[!, :freq])
corrected_freq_ranks[!, :prop] = round.(corrected_freq_ranks[!, :freq] ./ sum(corrected_freq_ranks[!, :freq]),
    digits=3)

deleteat!(corrected_freq_ranks, findfirst(corrected_freq_ranks[!, 1] .== "other"))


candidates = ["bolsonaro", "haddad", "ciro", "alckmin"] |> sort

candidate_key_dict = zip(candidates, ("A", "B", "C", "D")) |> Dict

# A,B,C,D | B,A,C,D | C,A,B,D | A,C,B,D | B,C,A,D | C,B,A,D | C,B,D,A | B,C,D,A | D,C,B,A | C,D,B,A | B,D,C,A | D,B,C,A | D,A,C,B | A,D,C,B | C,D,A,B | D,C,A,B | A,C,D,B | C,A,D,B | B,A,D,C | A,B,D,C | D,B,A,C | B,D,A,C | A,D,B,C | D,A,B,C |

data_permutations = map(i -> string.([i[1], i[2], i[3], i[4]]),
    eachrow(corrected_freq_ranks[!, 1:4]))

permutations_vector = [
    ["A", "B", "C", "D"], ["B", "A", "C", "D"], ["C", "A", "B", "D"], ["A", "C", "B", "D"],
    ["B", "C", "A", "D"], ["C", "B", "A", "D"], ["C", "B", "D", "A"], ["B", "C", "D", "A"],
    ["D", "C", "B", "A"], ["C", "D", "B", "A"], ["B", "D", "C", "A"], ["D", "B", "C", "A"],
    ["D", "A", "C", "B"], ["A", "D", "C", "B"], ["C", "D", "A", "B"], ["D", "C", "A", "B"],
    ["A", "C", "D", "B"], ["C", "A", "D", "B"], ["B", "A", "D", "C"], ["A", "B", "D", "C"],
    ["D", "B", "A", "C"], ["B", "D", "A", "C"], ["A", "D", "B", "C"], ["D", "A", "B", "C"]]

indices_for_p = [findfirst(permutation -> permutation == map(i -> candidate_key_dict[i],
        j), permutations_vector) for j in data_permutations]

p = corrected_freq_ranks[indices_for_p, :prop]



# -----------------------------------------------------------------------------

using StatsBase

dfspath= "../rscripts/dfs/"

mincw1 =  CSV.read(dfspath * "min_c1_raw.csv",DataFrame)

mincw2 =  CSV.read(dfspath * "min_c2_raw.csv",DataFrame)

function props_without_candidate(min_raw, candidate)

    turnedintovecs = Vector{String}.(eachrow(min_raw))
    filterothers = filter(x->all(y-> y != "other", x), turnedintovecs)
    filtercandidate = map(x->filter(y-> y!= candidate,x),filterothers)
    filteredproportions = collect(proportionmap(filtercandidate))
    filtered_df_props = DataFrame(:ranking_vectors => map(first, filteredproportions),
                                  :props => map(x->x[2], filteredproportions))
    return(filtered_df_props)
end

candidates = ["bolsonaro", "haddad", "ciro", "alckmin"] |> sort




#= function get_borda(candidate)
candidate => sum(
    [findfirst(x->x == candidate, 
    row.ranking_vectors ) * row.props 
    for row in eachrow(Vector{String}.(eachrow(mincw1)) |> 
         w->filter(x->all(y-> y != "other", x),w) |>
         countmap |> collect |> 
         x-> DataFrame(:ranking_vectors => map(first, x),
         :props => map(x->x[2], x)))]) end


get_borda.(candidates) =#



function candidate_key_dict_3_candidates(without_candidate_vec)
    zip(without_candidate_vec, ("A", "B", "C")) |> Dict
end


function getp_candidate_list_without_candidate(df, candidate_to_drop, 
    candidates = candidates )
    without_candidate_props = props_without_candidate(df, candidate_to_drop)
    candidates = ["bolsonaro", "haddad", "ciro", "alckmin"] |> sort
    without_candidate_vec(candidate_to_drop) = filter(x-> x!= candidate_to_drop,
                                                             candidates)
    without_candidate_vecc = without_candidate_vec(candidate_to_drop)
    candidate_key_dict = candidate_key_dict_3_candidates(without_candidate_vecc)
    println(candidate_key_dict)
    data_permutations = without_candidate_props[!,:ranking_vectors] 
    permutation_vectors_3c =  [["A", "B", "C"],
                               ["A", "C", "B"],
                               ["C", "A", "B"],
                               ["C", "B", "A"],
                               ["B", "C", "A"],
                               ["B", "A", "C"]]
                               
    indices_for_p = [findfirst(permutation -> permutation == map(i -> candidate_key_dict[i],
                           j), permutation_vectors_3c) for j in data_permutations]
    without_candidate_props[!,:index_in_p] = indices_for_p
    resorted_filtered = sort(without_candidate_props,:index_in_p)
    p = resorted_filtered.props     
    return(p,without_candidate_vecc)                     
end

# TODO :  Double check this !!!!!!! 
# TODO : also double check if the proportions are in the correct place. 

cw1_nota = representation△(getp_candidate_list_without_candidate(mincw1,"alckmin")...) 
cw1_notb = representation△(getp_candidate_list_without_candidate(mincw1,"bolsonaro")...)
cw1_notc = representation△(getp_candidate_list_without_candidate(mincw1,"ciro")...)
cw1_noth = representation△(getp_candidate_list_without_candidate(mincw1,"haddad")...)

cw1_names = map(x-> plotspath * x, 
["cw1_nota.png","cw1_notb.png", "cw1_notc.png", "cw1_noth.png"])


map(save, cw1_names, [cw1_nota, cw1_notb, cw1_notc, cw1_noth])

cw2_nota = representation△(getp_candidate_list_without_candidate(mincw2,"alckmin")...) 
cw2_notb = representation△(getp_candidate_list_without_candidate(mincw2,"bolsonaro")...)
cw2_notc = representation△(getp_candidate_list_without_candidate(mincw2,"ciro")...)
cw2_noth = representation△(getp_candidate_list_without_candidate(mincw2,"haddad")...)

cw2_names = map(x-> plotspath * x, 
["cw2_nota.png","cw2_notb.png", "cw2_notc.png", "cw2_noth.png"])


map(save, cw2_names, [cw2_nota, cw2_notb, cw2_notc, cw2_noth])



#= TODO : turn this into tests! I've got a bug from these tests below!!!!!! 
foo = props_without_candidate(mincw1,"haddad")

borda_without_haddad = [(candidate => ([findfirst(x->x == candidate, 
    row.ranking_vectors ) * row.props for row in eachrow(foo)] |> sum))
    for candidate in ["alckmin", "bolsonaro", "ciro"]]

    
plurality_without_haddad = [
    (candidate =>
    map(x->x.props,filter(x->x.ranking_vectors[1] == candidate, eachrow(foo))) |> sum )
    for candidate in ["alckmin", "bolsonaro", "ciro"]]


antiplurality_without_haddad = [
    (candidate =>
    map(x->x.props,filter(x->x.ranking_vectors[3] == candidate, eachrow(foo))) |> sum )
    for candidate in ["alckmin", "bolsonaro", "ciro"]]




        
nothaddadp= getp_candidate_list_without_candidate(mincw1,"haddad")

nothaddadp[2]=>antiplurality_3a(nothaddadp[1])
antiplurality_without_haddad

plurality_without_haddad
nothaddadp[2]=>plurality_3a(nothaddadp[1])

borda_without_haddad
nothaddadp[2]=>borda_3a(nothaddadp[1])


begin  
    df = mincw1
    candidate_to_drop = "haddad"
    

    without_candidate_props = props_without_candidate(df, candidate_to_drop)
    candidates = ["bolsonaro", "haddad", "ciro", "alckmin"] |> sort
    without_candidate_vec(candidate_to_drop) = filter(x-> x!= candidate_to_drop,
                                                             candidates)
    without_candidate_vecc = without_candidate_vec(candidate_to_drop)
    candidate_key_dict = candidate_key_dict_3_candidates(without_candidate_vecc)
    println(candidate_key_dict)
    data_permutations = without_candidate_props[!,:ranking_vectors] 
    permutation_vectors_3c =  [["A", "B", "C"],
                               ["A", "C", "B"],
                               ["C", "A", "B"],
                               ["C", "B", "A"],
                               ["B", "C", "A"],
                               ["B", "A", "C"]]
    println(data_permutations)

      indices_for_p = [findfirst(permutation -> permutation == map(i -> candidate_key_dict[i],
                           j), permutation_vectors_3c) for j in data_permutations] 
    without_candidate_props[!,:index_in_p] = indices_for_p
    without_candidate_props = sort(without_candidate_props,:index_in_p)
    println(without_candidate_props)    
#= 
        resorted_filtered = without_candidate_props[indices_for_p,:]
    p = resorted_filtered.props     
 =#

end
 =#