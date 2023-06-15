
import Pkg
Pkg.activate("./")

using CWElectionsBR
using RCall
import CSV
using DataFrames

dfspath = "../rscripts/dfs/"
plotspath ="../writing/images/"
dfs_names = readdir(cw.dfspath)

imputted_poly = filter(x->occursin("poly_imp_min_raw",x), dfs_names)
imp_poly_dfs = map(x->cw.CSV.read(cw.dfspath * x, DataFrame), imputted_poly)

df1 = imp_poly_dfs[1]

function getdropped_p_thenplot◬(raw_df, who_is_dropped)
cw.representation△(cw.getp_candidate_list_without_candidate(raw_df,who_is_dropped)...) 
end

cw1_nota,
cw1_notb,
cw1_notc,
cw1_noth = map(who_is_dropped->getdropped_p_thenplot◬(df1,who_is_dropped), cw.candidates)


cw1_names = map(x-> plotspath * x, 
["cw1_nota.png","cw1_notb.png", "cw1_notc.png", "cw1_noth.png"])

map(save, cw1_names, [cw1_nota, cw1_notb, cw1_notc, cw1_noth])

