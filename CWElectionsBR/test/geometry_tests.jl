import Pkg
Pkg.activate("./")

#ENV["LD_PRELOAD"]="/usr/lib/x86_64-linux-gnu/libstdc++.so.6"

using Revise 
using CWElectionsBR
#CairoMakie.activate!()

# CairoMakie.activate!(type = "png")

CWElectionsBR.make_basic_3_candidate△(["alckmin", "bolsonaro","ciro"])





#  Nurmi Uncertainty book page 122 example
#=  This Does match!!!!! 

plurality_share = [0.444, 0.256, 0.291]
antiplurality_share = [0.314, 0.335, 0.351]
borda_share = [0.357, 0.311, 0.331]

bar = representation△(plurality_share,
                         antiplurality_share,
                         borda_share)
 =#
# save("foo.png", bar)

# Tabarrok 2001 Figure 1
#   
#

#p = [0, .419,0., .258,.322, 0. ]


#= plurality_result = plurality_3a(p)
antiplurality_result = antiplurality_3a(p)
borda_result = borda_3a(p)

[turn_into_euclideanpoint(fn(p)) for fn in [plurality_3a,
        antiplurality_3a,
        borda_3a]]
 =#

 baz = representation△(p_without_alckmin,without_alckmin_list)




#save("baz.png", baz)
