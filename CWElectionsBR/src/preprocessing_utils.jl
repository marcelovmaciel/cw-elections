drop_candidate(acc, candidate) = map(a->filter(x->x≠ candidate,a), acc)

function freq_without_candidate(candidate, df )

    acc = []
    for i in 1:size(df)[1]
    push!(acc, Vector(df[i,1:4]))
    end
    @pipe (DataFrame(ranking_vectors =  drop_candidate(acc,candidate),
                        freq = df[!,"freq"] ) |>
                        groupby(_, :ranking_vectors) |>
                        combine(_, :freq => sum))
end


function make_pretty_ranking_vector(df)
    df.ranking_vectors = map(x-> join(x, " > "),df[!, :ranking_vectors])
    return(df)
end


function cleaned_df(df)
    acc = []
    for i in 1:25
    push!(acc, Vector(df[i,1:4]))
    end
    @pipe (DataFrame(ranking_vectors = acc,
    freq = df[!,"freq"] ) |>
    groupby(_, :ranking_vectors) |>
    combine(_, :freq => sum) |>
    make_pretty_ranking_vector(_) |>
    combine(_,:ranking_vectors => :ranking_vectors,
    :freq_sum => (x -> round(100 * x / sum(x),digits = 2)) => :prop)
    )
end

function fix_raw_into_vecs(min_raw)
turnedintovecs = Vector{String}.(eachrow(min_raw))
filterothers = filter(x->all(y-> y != "other", x), turnedintovecs)
filteredproportions = collect(proportionmap(filterothers))
filtered_df_props = DataFrame(:ranking_vectors => map(first, filteredproportions),
                                  :percentage => map(x->round(100 * x[2],digits = 2), filteredproportions))
    return(sort(filtered_df_props,:percentage, rev = true))
end



function finaldf_without_candidate(candidate,df)
    df = make_pretty_ranking_vector(freq_without_candidate(candidate, df))
    combine(df,:ranking_vectors => :ranking_vectors,
                :freq_sum => (x -> x / sum(x)) => :prop)

end
