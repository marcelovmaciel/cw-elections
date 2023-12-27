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


function fixranking_vectors(rv)
    foo = split(rv,",")
    
    foo = map(x-> replace(x,"[" => "") , foo)
    foo = map(x-> replace(x,"]" => "") , foo)
    foo = map(x->strip(x, '"'), foo)
    foo = map(x->strip(x, ' '), foo)
    foo = map(x->strip(x, '"'), foo)
    return(foo)
end

function makefri(df, freq_col,prop_col, fixrankbool)
if fixrankbool    
    df.ranking_vectors = map(fixranking_vectors, df.ranking_vectors)
end
    c1,c2,c3,c4  = map(y->map(x->x[y], df.ranking_vectors), 1:4)
    freq_ranks_inferred = DataFrame(Dict("1"=> c1,
                                     "2" => c2,
                                     "3" => c3,
                                     "4" => c4,
                                     "freq"=> df[!,freq_col],
                                     "prop" => df[!,prop_col]))
return(freq_ranks_inferred)
end

meanofmeansfri = makefri(meanofmeansoi,"freq_mean_mean", "prop_mean_mean", true)

cw.CSV.write("../rscripts/dfs/" * "meanofmeansfri.csv",
 meanofmeansfri)



polyoi = cw.CSV.read("../rscripts/dfs/" * "avgpolyoi.csv", cw.DataFrame)



polyfri = makefri(polyoi, "freq_mean", "prop_mean", false)

cw.CSV.write("../rscripts/dfs/" * "avgpolyfri.csv",
 polyfri)


