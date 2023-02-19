
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


#= cwcw2_nota,
cwcw2_notb,
cwcw2_notc,
cwcw2_noth = map(who_is_dropped->getdropped_p_thenplot◬(mincwcw2,who_is_dropped), cw.candidates)


cw2_names = map(x-> plotspath * x, 
["cw2_nota.png","cw2_notb.png", "cw2_notc.png", "cw2_noth.png"])

map(save, cw2_names, [cw2_nota, cw2_notb, cw2_notc, cw2_noth])

cw3_nota,
cw3_notb,
cw3_notc,
cw3_noth = map(who_is_dropped->getdropped_p_thenplot◬(mincw3,who_is_dropped), cw.candidates)

cw4_nota,
cw4_notb,
cw4_notc,
cw4_noth = map(who_is_dropped->getdropped_p_thenplot◬(mincw4,who_is_dropped), cw.candidates)

cw3_names = map(x-> plotspath * x, 
["cw3_nota.png","cw3_notb.png", "cw3_notc.png", "cw3_noth.png"])

map(save, cw3_names, [cw3_nota, cw3_notb, cw3_notc, cw3_noth])

cw4_names = map(x-> plotspath * x, 
["cw4_nota.png","cw4_notb.png", "cw4_notc.png", "cw4_noth.png"])

map(save, cw4_names, [cw4_nota, cw4_notb, cw4_notc, cw4_noth])
 =#