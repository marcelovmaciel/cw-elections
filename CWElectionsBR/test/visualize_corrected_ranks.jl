
import Pkg
Pkg.activate("./")

using CWElectionsBR
using RCall
import CSV
using DataFrames
dfspath = "../rscripts/dfs/"

using FreqTables

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







