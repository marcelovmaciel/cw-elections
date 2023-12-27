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



#reversal_component =  reduce(+,[A[:,coef] .* coeffs[coef] for coef in 19:24])  .|> float  .|> x-> round(x,digits = 2)

#condorcet_component = reduce(+,[A[:,coef] .* coeffs[coef] for coef in 15:17])  .|> float  .|> x-> round(x,digits = 2)

#cleaned_profile = (p4c - (condorcet_component)) + ones(24,1) * 9.75


function prettify_component(component, coeff_index)

(component .* coeffs[coeff_index]
    .|> float  .|> x-> round(x,digits = 2))
    
end    


function componentplots()
    titles = [
    "Basic Profile A (Ba)",
    "Basic Profile B (Bb)",
    "Basic Profile C (Bc)",
    "Condorcet (ABCD)",
    "Condorcet (ABDC)",
    "Condorcet (ACBD)", 
    "Kernel", 
    "Double Reversal ac", 
    "Double Reversal cb",
    "Double Reversal ab",
    "Double Reversal cd",   
    "Double Reversal bd",
    "Double Reversal ad",  
    ]
    A_parts = [Ba, Bb, Bc, Cabcd, Cabdc, 
              Cacbd, K, double_reversal_ac,
              double_reversal_cb,
              double_reversal_ab,
              double_reversal_cd,
              double_reversal_bd,
              double_reversal_ad]
    ks = [ "Ba", "Bb", "Bc", "Cabcd", "Cabdc", "Cacbd", "K",
              "double_reversal_ac", "double_reversal_cb", "double_reversal_ab",
              "double_reversal_cd", "double_reversal_bd", "double_reversal_ad"]
    idxs = 12:24
    abase_plts = [ cw.representation_tetrahedron_freqs(i,
                 coerce_to_int = false, 
                 textsize = 13,
                  title = j)  for (i,j) in 
                  zip(A_parts,titles)]
    comp_plts = [cw.representation_tetrahedron_freqs(
        prettify_component(i,k),
    coerce_to_int = false, 
    textsize = 13,
     title = j)  for (i,j,k) in 
     zip(A_parts,titles,idxs)]
    basedict = Dict(zip(ks,abase_plts ))
    compdict = Dict(zip(ks,comp_plts))
    return(basedict, compdict)
end    


base_dict,  comp_dict  = componentplots()


p4c = cw.getp_4candidates(mincw1)

reptetra = cw.representation_tetrahedron_freqs(p4c, textsize = 18)


cw.save("../writing/images/representation_tetrahedron.png", reptetra)

possible_condorcet_component_base = cw.representation_tetrahedron_freqs(
            Cabcd  +
        Cabdc + 
        Cacbd ,
            coerce_to_int = false, 
            textsize = 13,
             title = "Condorcet Component?") 

possible_condorcet_component = cw.representation_tetrahedron_freqs(
                Cabcd * coeffs[15] +
            Cabdc * coeffs[16] + 
            Cacbd * coeffs[17],
                coerce_to_int = false, 
                textsize = 13,
                 title = "Condorcet Component?") 
             

possible_reversal_a_base = cw.representation_tetrahedron_freqs(
                double_reversal_ac  +
                double_reversal_ab + 
                double_reversal_ad ,
                    coerce_to_int = false, 
                    textsize = 13,
                     title = "Reversal A?")              


## Discussion precomputed             


reptetra
             

base_dict["Ba"]

base_dict["Bb"]

base_dict["Bc"]

base_dict["Cabcd"]

base_dict["Cabdc"]

base_dict["Cacbd"]

possible_condorcet_component_base

base_dict["K"]

base_dict["double_reversal_ac"]

base_dict["double_reversal_cb"]

base_dict["double_reversal_ab"]

base_dict["double_reversal_cd"]

base_dict["double_reversal_bd"]

base_dict["double_reversal_ad"]

possible_reversal_a_base


comp_dict["Ba"]

comp_dict["Bb"]

comp_dict["Bc"]

comp_dict["Cabcd"]

comp_dict["Cabdc"]

comp_dict["Cacbd"]

possible_condorcet_component

comp_dict["K"]

comp_dict["double_reversal_ac"]

comp_dict["double_reversal_cb"]

comp_dict["double_reversal_ab"]

comp_dict["double_reversal_cd"]

comp_dict["double_reversal_bd"]

comp_dict["double_reversal_ad"]


cw.representation_tetrahedron_freqs(
double_reversal_ac  +
double_reversal_ab + 
double_reversal_ad ,
    coerce_to_int = false, 
    textsize = 13,
     title = "Reversal A?") 


## Other stuff here 

condorcet_component =  (Cabcd * coeffs[15] +
Cabdc * coeffs[16] + 
Cacbd * coeffs[17])






