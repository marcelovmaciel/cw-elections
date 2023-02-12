import Pkg

Pkg.activate("./")

import CWElectionsBR as cw
using PrettyTables
using Suppressor
using DataFrames

dfspath = "../rscripts/dfs/"
dfs_names = readdir(cw.dfspath)

imputted_poly = filter(x->occursin("poly_imp_min_raw",x), dfs_names)
imp_poly_dfs = map(x->cw.CSV.read(cw.dfspath * x, DataFrame), imputted_poly)

mincw1 = imp_poly_dfs[1]

p4c = cw.getp_4candidates(mincw1, "freq")

wscw1 = cw.get_4c_wₛ(p4c) 
plurality_result = cw.plurality_4c_wₛ_num(p4c)
vote_for_two_result = cw.vote_for_two_4c_wₛ_num(p4c)
antiplurality_result = cw.antiplurality_4c_wₛ_num(p4c)


T = 1/24 .* [0  0  -1 -2 -2 -1 2 1  -1 -2 -1 1  2  1  0 0  1  2  1  -1 -2 -1 1  2;
-1 -2 -2 -1 0  0  2 1  0  0  1  2  1  -1 -2 -1 1   2  2  1  -1 -2 -1  1;
-2 -1 0  0  -1 -2 1 -1 -2 -1 1  2  2  1  -1 -2 -1  1  2  1  0  0 1  2;
0  -4 4  0  -4 4  -4 -4 -4 4  4  4  0  0  0 -4 -4 -4 4  4  4  0 0  0;
4  -4 0  4  -4 0  -4 -4 -4 0  0  0  4  4  4 -4 -4 -4 0  0  0  4 4  4;
0  0  4  4  4  0  0 -4 4  0  -4 4  -4 -4 0 0  0  -4 -4 4  4  4 -4 -4;
4  4  0  0  0  4  4 -4 0  4  -4 0  -4 -4 4 4  4  -4 -4 0  0  0 -4 -4;
4  0  0  0  4  4  -4 4  4  4  -4 -4 0  -4 4 0  -4 4  -4 -4 0  0 0  -4;
0  4  4  4  0  0  -4 0  0  0  -4 -4 4  -4 0 4  -4 0  -4 -4 4  4 4  -4;
4  4  4  0  0  0  -4 -4 0  0  0  -4 -4 4  4 4  -4 -4 0  -4 4  0 -4 4;
0  0  0  4  4  4  -4 -4 4  4  4  -4 -4 0  0 0  -4 -4 4  -4 0  4 -4 0;
3  3  2  1  1  2  2 1  -1 -2 -1 1  -1  -2  -3 -3   -2 -1 1  -1 -2 -1 1 2;
2  1  1  2  3  3  -1  -2 -3  -3  -2  -1  1  -1 -2 -1  1  2  2  1  -1 -2 -1 1;
1  2  3  3  2  1  1 -1 -2 -1 1  2  2  1  -1 -2 -1 1  -1  -2  -3  -3  -2 -1;
3  0  0  -3 0  0  0 -3 0  0  3  0  0  0  -3 0   0  3  -3 0  0  3 0  0;
0  0  3  0  0  -3 -3 0   0  3  0  0  0  -3 0 0  3  0  0  0  -3 0 0  3;
0  3  0  0  -3 0  0 0  3  0  0  -3 3  0  0 -3 0  0  0  3  0  0 -3 0;
1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 ;
3  6  -6 -3 -6 6  -6 -3 -6 6  3  6  0  0  3 0  0  -3 3  0  0  -3 0 0;
-6 6  3  6  -6 -3 3 0  0  -3 0  0  -6 -3 -6 6  3  6  0  0  3  0 0  -3;
6  3  6  -6 -3 -6 0 0  -3 0  0  3  -3 0  0 3  0  0  6  3  6  -6 -3 -6;
0  -3 0  0  3  0  -6 6  3  6  -6 -3 3  6  -6 -3 -6 6  0  -3 0  0 3  0;
3  0  0  -3 0  0  0 -3 0  0  3  0  -6 6  3 6  -6 -3 3  6  -6 -3 -6 6;
0  0  3  0  0  -3 3 6  -6 -3 -6 6  0  -3 0 0  3  0  -6 6  3  6 -6 -3]

decomp = T * p4c

Da, Db, Dc = decomp[1], decomp[2], decomp[3]

departure_profiles = decomp[4:11] # Relevant for rankings of SUBSETS !!!!! 
#=
Basically, If I knew this decomposition for any n, 
I could in principle analyze the alternative set stability analytically!!!!!!
=#

Ba,Bb,Bc = decomp[12], decomp[13], decomp[14] # Basic profile differential 

Cabcd, Cabdc, Cacbd = decomp[15], decomp[16], decomp[17]

K = decomp[18] # Kernel 

double_reversal_ac = decomp[19] 
double_reversal_cb = decomp[20]
double_reversal_ab = decomp[21]
double_reversal_cd = decomp[22]
double_reversal_bd = decomp[23]
double_reversal_ad = decomp[24]
