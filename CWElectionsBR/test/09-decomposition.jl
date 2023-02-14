import Pkg

Pkg.activate("./")

import CWElectionsBR as cw
using PrettyTables

using DataFrames
import SymPy as sp 


dfspath = "../rscripts/dfs/"
dfs_names = readdir(cw.dfspath)

imputted_poly = filter(x->occursin("poly_imp_min_raw",x), dfs_names)
imp_poly_dfs = map(x->cw.CSV.read(cw.dfspath * x, DataFrame), imputted_poly)

mincw1 = imp_poly_dfs[1]

p4c = cw.getp_4candidates(mincw1, "freq") .|>  Int

wscw1 = cw.get_4c_wₛ(p4c) 
plurality_result = cw.plurality_4c_wₛ_num(p4c)
vote_for_two_result = cw.vote_for_two_4c_wₛ_num(p4c)
antiplurality_result = cw.antiplurality_4c_wₛ_num(p4c)


T = 1//24 .* [0  0  -1 -2 -2 -1 2 1  -1 -2 -1 1  2  1  0 0  1  2  1  -1 -2 -1 1  2;
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


A = T^(-1) .|> Int

coeffs = sp.Matrix(T) * p4c

Da, Db, Dc = A[:,1], A[:,2], A[:,3] # departure differentials 


departure_profiles = A[:,4:11] # Relevant for rankings of SUBSETS !!!!! 

#=
Basically, If I knew this decomposition for any n, 
I could in principle analyze the alternative set stability analytically!!!!!!
=#

Ba,Bb,Bc = A[:, 12], A[:, 13], A[:,14] # Basic profile differential 

Cabcd, Cabdc, Cacbd = A[:,15], A[:,16], A[:,17]

K = A[:,18] # Kernel 

double_reversal_ac = A[:,19] 
double_reversal_cb = A[:,20]
double_reversal_ab = A[:,21]
double_reversal_cd = A[:,22]
double_reversal_bd = A[:,23]
double_reversal_ad = A[:,24]


reversal_component =  reduce(+,[A[:,coef] .* coeffs[coef] for coef in 19:24])  .|> float  .|> x-> round(x,digits = 2)

condorcet_component = reduce(+,[A[:,coef] .* coeffs[coef] for coef in 15:17])  .|> float  .|> x-> round(x,digits = 2)



cleaned_profile = p4c - (reversal_component + condorcet_component)

cw.plurality_4c_wₛ_num(cleaned_profile)


function prettify_component(component, coeff_index)

(component .* coeffs[coeff_index]
    .|> float  .|> x-> round(x,digits = 2))
    
end    



reptetra_basic = cw.representation_tetrahedron_freqs(
prettify_component(Ba,12) +
prettify_component(Bb,13) + 
prettify_component(Bc,14) .|> x->round(x, digits = 2),
 coerce_to_int = false, textsize = 12, title = "Basic")



 
reptetra_kernel = cw.representation_tetrahedron_freqs(
    prettify_component(K,18),
     coerce_to_int = false, textsize = 13, title = "Kernel") 

    
roundit(stuff)=  stuff   .|> x->round(x, digits = 2)

cw.representation_tetrahedron_freqs(
cleaned_profile |> roundit,
         coerce_to_int = false, 
         textsize = 13,
          title = "Profile - (Condorcet + Reversal Components)") 

