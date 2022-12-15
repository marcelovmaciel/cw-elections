using Revise
import Pkg
Pkg.activate("./")


import CWElectionsBR as cw



dfspath = "../rscripts/dfs/"

mincw1 = cw.CSV.read(dfspath * "min_c1_raw.csv", cw.DataFrame)

p4c = cw.getp_4candidates(mincw1)


cw.plurality_four_candidates()


antiplurality_result = cw.get_positional_voting_numeric_vectors(cw.antiplurality_four_candidates(),
 p4c) |> Vector{Float64}


plurality_result = cw.get_positional_voting_numeric_vectors(cw.plurality_four_candidates(), 
p4c) |> Vector{Float64}
vote_for_two_result = cw.get_positional_voting_numeric_vectors(cw.vote_for_two_four_candidates(), p4c) 
|> Vector{Float64}
