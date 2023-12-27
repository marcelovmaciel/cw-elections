import Pkg

Pkg.activate("./")

import CWElectionsBR as cw
using PrettyTables
using Suppressor
using DataFrames
using RCall
using LinearAlgebra




dfspath = "../rscripts/dfs/"
dfs_names = readdir(cw.dfspath)

imputted_poly = filter(x->occursin("poly_imp_min_raw",x), dfs_names)
imp_poly_dfs = map(x->cw.CSV.read(cw.dfspath * x, DataFrame), imputted_poly)
mincw1 = imp_poly_dfs[1]

marginsdf = cw.make_cw_table(mincw1)
    

p4c = cw.getp_4candidates(mincw1,"freq")
 
wscw1 = cw.get_4c_wₛ(p4c) 

p4cforqs = cw.getp_4candidates(mincw1)

standardized_borda = cw.borda_4c_wₛ_num(p4cforqs)

helper_matrix = hcat(cw.candidates, standardized_borda)

std_borda_df = DataFrame("candidates" => helper_matrix[:,1],
"Standardized Borda Score" =>  helper_matrix[:,2]
)


open("../writing/images/std_borda.tex", "w") do file 
    stuff = @capture_out pretty_table(std_borda_df, backend = Val(:latex))    
    write(file, stuff)
end        

open("../writing/images/cw_margins.tex", "w") do file 
    stuff = @capture_out pretty_table(marginsdf, backend = Val(:latex))    
    write(file, stuff)
end        



#= 
votesys = rimport("votesys")
created_vote_object = votesys.create_vote(mincw1, xtype = 2, candidate= cw.candidates)

cdc1 = rcopy(votesys.cdc_simple(created_vote_object))
cdc_result = cdc1[:cdc]

cdc_result

(1654 - 1283)/2937

bar = cdc_result - cdc_result' .|>
x-> round(x/first(size(mincw1)) * 100, digits = 2)

bar

for i in 1:4
sum(bar[i,:])     |> println
end
 =#
