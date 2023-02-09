import Pkg

Pkg.activate(".")

using Revise 

import CWElectionsBR as cw
using DataFrames
import Base.Filesystem as fl 
using Pipe 
using Chain
using RCall
using Statistics
@rimport tidyr

dfs_names = readdir(cw.dfspath)
oi_dfs = filter(x->occursin("_oi",x), dfs_names)
pmm_oi = filter(x->occursin("pmm",x), oi_dfs)
cart_oi = filter(x->occursin("cart",x), oi_dfs)

function get_avg_std(df_list)
    oi_base = cw.CSV.read(joinpath(cw.dfspath, df_list[1]), cw.DataFrame)
    for imp in df_list[2:end]
        app = cw.CSV.read(joinpath(cw.dfspath, imp), cw.DataFrame)    
        append!(oi_base,app)
    end
    test = groupby(oi_base, :ranking_vectors)  |>
    x-> combine(x, :freq => mean, :freq => std, :prop => mean, :prop => std) 
    test.freq_mean = map(x->Int(round(x,digits = 0)),test.freq_mean)
    test.freq_std = map(x->Int(round(x,digits = 0)),test.freq_std)
    return(test)
end

avg_pmm = get_avg_std(pmm_oi)
avg_cart = get_avg_std(cart_oi)
avg_poly = rename(cw.CSV.read(cw.dfspath * "polyreg1_oi.csv", cw.DataFrame), :freq => :freq_mean, 
:prop => :prop_mean )

cw.CSV.write("../rscripts/dfs/" * "avgpmmoi.csv", avg_pmm)
cw.CSV.write("../rscripts/dfs/" * "avgcartoi.csv", avg_cart)
cw.CSV.write("../rscripts/dfs/" * "avgpolyoi.csv", avg_poly)

baseavg = select(avg_pmm, :ranking_vectors, :freq_mean, :prop_mean)

append!(baseavg, select(avg_cart, :ranking_vectors, :freq_mean, :prop_mean))

append!(baseavg, avg_poly)

meanofmeans = groupby(baseavg, :ranking_vectors) |> 
x-> combine(x, :freq_mean => mean, :prop_mean => mean)

meanofmeans.freq_mean_mean = map(x->Int(round(x,digits = 0)),
meanofmeans.freq_mean_mean)

cw.CSV.write("../rscripts/dfs/" * "meanofmeansoi.csv", meanofmeans)

c1,c2,c3,c4  = map(y->map(x->x[y], meanofmeans.ranking_vectors), 1:4)

avg_pmm.ranking_vectors[1]

foo = split(meanofmeans.ranking_vectors[1],",")

foo[1]

foo = map(x-> replace(x,"[" => "") , foo)
foo = map(x-> replace(x,"]" => "") , foo)
foo = map(x->strip(x, '"'), foo)
foo = map(x->strip(x, ' '), foo)
foo = map(x->strip(x, '"'), foo)






freq_ranks_inferred = DataFrame(Dict("1"=> c1,
                                     "2" => c2,
                                     "3" => c3,
                                     "4" => c4,
                                     "freq"=> meanofmeans.freq_mean_mean,
                                     "prop" => meanofmeans.prop_mean_mean))

cw.CSV.write("../rscripts/dfs/" * "meanofmeansfri.csv",
 freq_ranks_inferred)