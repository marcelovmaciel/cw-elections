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

df = cw.CSV.read(cw.dfspath * "polyreg1_oi.csv", cw.DataFrame)

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


