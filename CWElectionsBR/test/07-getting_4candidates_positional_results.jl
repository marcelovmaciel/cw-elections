using Revise

import Pkg

Pkg.activate("./")


import CWElectionsBR as cw
using PrettyTables
using Suppressor

dfspath = "../rscripts/dfs/"
dfs_names = readdir(cw.dfspath)

imputted_poly = filter(x->occursin("poly_imp_min_raw",x), dfs_names)
imp_poly_dfs = map(x->cw.CSV.read(cw.dfspath * x, DataFrame), imputted_poly)



mincw1 = imp_poly_dfs[1]

p4c = cw.getp_4candidates(mincw1)

 
wscw1 = cw.get_4c_wₛ(p4c) 


plurality_result = cw.plurality_4c_wₛ_num(p4c)
vote_for_two_result = cw.vote_for_two_4c_wₛ_num(p4c)
antiplurality_result = cw.antiplurality_4c_wₛ_num(p4c)

p_results_df = cw.DataFrame(:candidates => ["Alckmin", "Bolsonaro", "Ciro", "Haddad"],
                         :antiplurality => antiplurality_result, 
                         :vote_for_two_result => vote_for_two_result,
                         :plurality_result => plurality_result
                         )

225 + 171 + 62 + 649 + 83


cw.get_method_4c_wₛ_numeric(p4c,0.4,0.1) 
                         

ws_tallies = @capture_out pretty_table(cw.DataFrame("candidates"=> cw.candidates,
                         "w_s tallies"=>  wscw1 ),backend = Val(:latex)) 


open("../writing/images/ws_tallies.tex", "w") do file 
write(file, ws_tallies)
end        


positional_rankings = @capture_out pretty_table(p_results_df, backend = Val(:latex))                         

open("../writing/images/positional_rankings.tex", "w") do file 
        write(file, positional_rankings)
end        
        


##  BUG: This does not match the plurality result   
## I can simply get the general_svector 
## And replace on it! 
## They don't match because those are not ws, but qs!!!!                   





#= function cleaned_up_result_for_p(vm, p = p4c) 
        Vector{Float64}(cw.get_positional_voting_numeric_vectors(vm, p)) |>
        v-> round.(v, digits = 4)
end
 =#

