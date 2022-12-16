using Revise
import Pkg
Pkg.activate("./")

import CWElectionsBR as cw

dfspath = "../rscripts/dfs/"

mincw1 = cw.CSV.read(dfspath * "min_c1_raw.csv", cw.DataFrame)

p4c = cw.getp_4candidates(mincw1)

function cleaned_up_result_for_p(vm, p = p4c) 
        Vector{Float64}(cw.get_positional_voting_numeric_vectors(vm, p)) |>
        v-> round.(v, digits = 4)
end






antiplurality_result = cleaned_up_result_for_p(cw.antiplurality_four_candidates()) 
plurality_result = cleaned_up_result_for_p(cw.plurality_four_candidates())
vote_for_two_result = cleaned_up_result_for_p(cw.vote_for_two_four_candidates())

borda_result = cleaned_up_result_for_p(cw.borda_four_candidates())

p_results_df = cw.DataFrame(:candidates => ["Alckmin", "Bolsonaro", "Ciro", "Haddad"],
                         :antiplurality => antiplurality_result, 
                         :vote_for_two_result => vote_for_two_result,
                         :plurality_result => plurality_result
                         )


##  BUG: This does not match the plurality result   
## I can simply get the general_svector 
## And replace on it! 
## They don't match because those are not ws, but qs!!!!                   
positional_rankings = pretty_table(p_results_df, backend = Val(:latex))
                         
 p_results_df      |> println                    


general_svector = begin  
    s1 = 0;s2=0
    s₁ = cw.sp.symbols("s₁")
    s₂ = cw.sp.symbols("s₂")
    replacing_dict = Dict(zip([s₁, s₂], [s1,s2]))
    foo = cw.get_general_positional_vec(p4c) 
    ## .|> x-> x//(1 + s₁ + s₂) 
end    


s₁ = cw.sp.symbols("s₁")
s₂ = cw.sp.symbols("s₂")



# This matches the proper plurality result 
ws_tallies = pretty_table(cw.DataFrame("candidates"=> cw.candidates,
"w_s tallies"=>  general_svector ),backend = Val(:latex))




p_results_df


counterfactuals = [0.	0.0495357	0.	0.251126;
                   0.950464	0.	0.69011	1.;
                   1.	0.30989	0.	0.755121;
                   0.748874	0.	0.244879	0.] .|> x-> round(x, digits = 2)

counterfactuals = cw.DataFrame(counterfactuals, :auto)

cw.rename!(counterfactuals, cw.candidates)

counterfactuals[!,:candidates] = cw.candidates 

cw.select!(counterfactuals, [:candidates, Symbol.(cw.candidates)...])



counterfactuals |> browse 