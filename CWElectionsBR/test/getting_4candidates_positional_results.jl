using Revise

import Pkg



Pkg.activate("./")



import CWElectionsBR as cw

dfspath = "../rscripts/dfs/"

mincw1 = cw.CSV.read(dfspath * "min_c1_raw.csv", cw.DataFrame)

p4c = cw.getp_4candidates(mincw1)

#= function cleaned_up_result_for_p(vm, p = p4c) 
        Vector{Float64}(cw.get_positional_voting_numeric_vectors(vm, p)) |>
        v-> round.(v, digits = 4)
end
 =#

 
wscw1 = cw.get_general_wₛ(p4c) 


plurality_result = cw.plurality_4c_wₛ_num(p4c)
vote_for_two_result = cw.vote_for_two_4c_wₛ_num(p4c)
antiplurality_result = cw.antiplurality_4c_wₛ_num(p4c)

p_results_df = cw.DataFrame(:candidates => ["Alckmin", "Bolsonaro", "Ciro", "Haddad"],
                         :antiplurality => antiplurality_result, 
                         :vote_for_two_result => vote_for_two_result,
                         :plurality_result => plurality_result
                         )

ws_tallies = pretty_table(cw.DataFrame("candidates"=> cw.candidates,
                         "w_s tallies"=>  wscw1 ),backend = Val(:latex))

positional_rankings = pretty_table(p_results_df, backend = Val(:latex))                         
                         

##  BUG: This does not match the plurality result   
## I can simply get the general_svector 
## And replace on it! 
## They don't match because those are not ws, but qs!!!!                   

                           


cw.get_method_wₛ(p4c,0,0)    


# This matches the proper plurality result 



p_results_df


counterfactuals = [0.	0.0495357	0.	0.251126;
                   0.950464	0.	0.69011	1.;
                   1.	0.30989	0.	0.755121;
                   0.748874	0.	0.244879	0.] .|> x-> round(x, digits = 2)

counterfactuals = cw.DataFrame(counterfactuals, :auto)

cw.rename!(counterfactuals, cw.candidates)

counterfactuals[!,:candidates] = cw.candidates 

cw.select!(counterfactuals, [:candidates, Symbol.(cw.candidates)...])

table_counterfactuals = pretty_table(counterfactuals ,backend=Val(:latex))