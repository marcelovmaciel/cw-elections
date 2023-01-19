# 3 candidates stuff  

function props_without_candidate(min_raw, candidate)

    turnedintovecs = Vector{String}.(eachrow(min_raw))
    filterothers = filter(x->all(y-> y != "other", x), turnedintovecs)
    filtercandidate = map(x->filter(y-> y!= candidate,x),filterothers)
    filteredproportions = collect(proportionmap(filtercandidate))
    filtered_df_props = DataFrame(:ranking_vectors => map(first, filteredproportions),
                                  :props => map(x->x[2], filteredproportions))
    return(filtered_df_props)
end



#= function get_borda(candidate)
candidate => sum(
    [findfirst(x->x == candidate,
    row.ranking_vectors ) * row.props
    for row in eachrow(Vector{String}.(eachrow(mincw1)) |>
         w->filter(x->all(y-> y != "other", x),w) |>
         countmap |> collect |>
         x-> DataFrame(:ranking_vectors => map(first, x),
         :props => map(x->x[2], x)))]) end


get_borda.(candidates) =#



function candidate_key_dict_3_candidates(without_candidate_vec)
    zip(without_candidate_vec, ("A", "B", "C")) |> Dict
end


function getp_candidate_list_without_candidate(df, candidate_to_drop,
    candidates = candidates )
    without_candidate_props = props_without_candidate(df, candidate_to_drop)
    candidates = ["Bolsonaro", "Haddad", "Ciro", "Alckmin"] |> sort
    without_candidate_vec(candidate_to_drop) = filter(x-> x!= candidate_to_drop,
                                                             candidates)
    without_candidate_vecc = without_candidate_vec(candidate_to_drop)
    candidate_key_dict = candidate_key_dict_3_candidates(without_candidate_vecc)
    println(candidate_key_dict)
    data_permutations = without_candidate_props[!,:ranking_vectors]
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
    p = resorted_filtered.props
    return(p,without_candidate_vecc)
end


function drop_candidate_plot_triangle(df, candidate_to_drop)
representation△(getp_candidate_list_without_candidate(df, candidate_to_drop)...)
end


# 4 candidates stuff 
