import Pkg

Pkg.activate("./")

import CWElectionsBR as cw
using PrettyTables
using Suppressor
using DataFrames
using FloatingTableView

dfspath = "../rscripts/dfs/"
dfs_names = readdir(cw.dfspath)

imputted_poly = filter(x->occursin("poly_imp_min_raw",x), dfs_names)
imp_poly_dfs = map(x->cw.CSV.read(cw.dfspath * x, DataFrame), imputted_poly)

mincw1 = imp_poly_dfs[1]



function get_wholecountsdf_and_fcdf(mincw1, cdrop)
turnedintovecs = Vector{String}.(eachrow(mincw1))
wholecounts = collect(countmap(turnedintovecs))

wholecountsdf = DataFrame(:ranking_vectors => map(first, wholecounts),
:props => map(x->x[2], wholecounts))


sort!(wholecountsdf, :ranking_vectors)


filtered_candidate_counts = map(y->filter(x-> x!= cdrop,y), wholecountsdf.ranking_vectors)

filtered_candidate_counts

fcdf = DataFrame(Dict(:ranking_vectors => filtered_candidate_counts, :prop => wholecountsdf.props))
return(wholecountsdf,fcdf)
end

function get_grouped_fcdf(mincw1, cdrop) 
    groupby(get_wholecountsdf_and_fcdf(mincw1,cdrop)[2],
     :ranking_vectors) |> x-> combine(x,:prop => sum)
end

groupedfcdf = get_grouped_fcdf(mincw1)


wholecountsdf,fcdf = get_wholecountsdf_and_fcdf(mincw1)


function getp_candidate_list_without_candidate(df, candidate_to_drop,
    candidates = candidates )
    without_candidate_props = get_grouped_fcdf(df, candidate_to_drop)
    candidates = ["Bolsonaro", "Haddad", "Ciro", "Alckmin"] |> sort
    without_candidate_vec(candidate_to_drop) = filter(x-> x!= candidate_to_drop,
                                                             candidates)
    without_candidate_vecc = without_candidate_vec(candidate_to_drop)
    candidate_key_dict = cw.candidate_key_dict_3_candidates(without_candidate_vecc)
    println(candidate_key_dict)
    data_permutations = without_candidate_props[!,:ranking_vectors]
    println(data_permutations) # Here is already wrong !!!! 
    permutation_vectors_3c =  [["A", "B", "C"],
                               ["A", "C", "B"],
                               ["C", "A", "B"],
                               ["C", "B", "A"],
                               ["B", "C", "A"],
                               ["B", "A", "C"]]

    indices_for_p = [findfirst(permutation -> permutation == map(i -> candidate_key_dict[i],
                           j), permutation_vectors_3c) for j in data_permutations]
    without_candidate_props[!,:index_in_p] = indices_for_p
    resorted_filtered = sort(without_candidate_props,:index_in_p)
    p = resorted_filtered.prop_sum
    return(p,without_candidate_vecc)
end


p3 = getp_candidate_list_without_candidate(mincw1, "Haddad", cw.candidates)


bar = cw.make_basic_3_candidate△(cw.candidates)

positions_triangle = [(permutation = "A,B,C" , position =   (0.37, 0.1)),
(permutation = "A,C,B" , position =  (0.27, 0.27)),
(permutation = "C,A,B" , position =  (0.37,0.53)),
(permutation = "C,B,A" , position =  (0.57 , 0.53)),
(permutation = "B,C,A" , position =  (0.67 , 0.27)),
(permutation = "B,A,C" , position =  (0.63, 0.1)),
]

for (p,pos) in zip(p3[1], positions_triangle)
text!(string(Int(p)), position = pos.position, textsize = 15)
end    

save("../writing/images/representation_triangle_noth.png", bar)