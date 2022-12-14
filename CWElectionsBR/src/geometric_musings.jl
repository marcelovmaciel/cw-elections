

## i dont remeber how I calculated that translation LOL
tern2cart(a, b, c) = (1 / 2 * (2b + c) / (a + b + c), √3 / 2 * (c / (a + b + c)))
midpoint(p1,p2) = ( (p1[1] + p2[1] )/2, (p1[2] + p2[2])/2)

function turn_into_euclideanpoint(point)
    tern2cart(point...) |> GeometryBasics.Point2
end

function make_basic_3_candidate△(sorted_candidate_list)


    plainpoints = [(0.,0.), (1.,0.), (0.5,sqrt(3)/2)]

    external_triangle_centroid = (
        Meshes.centroid(Meshes.Triangle(plainpoints...)) |>
        x-> x.coords |>
        Tuple |>
        GeometryBasics.Point2
    )

    externalpoints = GeometryBasics.Point2.(plainpoints)

    midpoints = GeometryBasics.Point2.(map(x->(midpoint(x...)),
                                           combinations(plainpoints, 2) ))
    #midpoints = GeometryBasics.Point2.([(0.5, 0.0), (0.25, 0.5 ), (0.75, 0.5)])

    propercombinations = [[i,j,external_triangle_centroid] for j in externalpoints
                            for i in midpoints]
    # Meshes.centroid(Meshes.Triangle((0.,0.), (1.,0.), (0.5,1.)))
    mypoly = GeometryBasics.Polygon(externalpoints, propercombinations)

    barepoints = map(x-> convert(Vector{Vector{Float64}}, x) , propercombinations )
    barepoints = map(x-> x.coords,
                     map(point -> Meshes.centroid(Meshes.Triangle(map(x-> Tuple(x),
                                                                      point))),
                         barepoints))

    basic_3candidate_triangle = poly(mypoly,
            color = :white, strokecolor = :black, strokewidth = 1)

    weirdcentroid(x,y)= ((y <= 0.4) && (y>=0.2)) && ((x <= 0.6) && (x>=0.4))
    weirdcentroid(x) = weirdcentroid(x[1], x[2])

    #scatter!(barepoints)
    #scatter!(filter(!weirdcentroid,barepoints))

    hidespines!(basic_3candidate_triangle.axis)
    hidexdecorations!(basic_3candidate_triangle.axis)
    hideydecorations!(basic_3candidate_triangle.axis)

    text!(sorted_candidate_list[1], position = (-0.03,-0.05))
    text!(sorted_candidate_list[2], position = (0.9,-0.05))
    text!(sorted_candidate_list[3], position = (0.48,0.867))

    return(basic_3candidate_triangle)
end


function plot_point_in_triangle(p, fig)
    scatter!(fig.axis, [turn_into_euclideanpoint(p)], color = :black)
end


function connect_plurality_antiplurality_points(p,ap, fig)
lines!(fig.axis, [turn_into_euclideanpoint(p),
                  turn_into_euclideanpoint(ap)])
end


function representation△(plurality_share,
                         antiplurality_share,
                         borda_share, sorted_candidate_list)
    basic△ = make_basic_3_candidate△(sorted_candidate_list)
    foreach(x-> plot_point_in_triangle(x,basic△),
            [plurality_share,
             antiplurality_share,
             borda_share])
    connect_plurality_antiplurality_points(plurality_share,
                                       antiplurality_share, basic△)
    return(basic△)

end

function positional_method_3a(s,p)
        [p[1] + p[2] + (-p[1]-p[2]+p[3]+p[6])*s,
         p[6] + p[5] + (p[4] - p[5] + p[1] - p[6])*s,
         p[3] + p[4] + (p[2]-p[3]-p[4] + p[5])*s ]
end

plurality_3a(p) = positional_method_3a(0,p)
borda_3a(p) = positional_method_3a(1/3,p)
antiplurality_3a(p) = positional_method_3a(1/2,p)


function representation△(voter_profile,sorted_candidate_list)
    positional_results = map(fn->fn(voter_profile),
       [plurality_3a,
        antiplurality_3a,
        borda_3a])

    representation△(positional_results..., sorted_candidate_list)
end


## Four candidates stuff 



p_twentyfour() = [sp.Sym("p$i") for i in 1:24]


## TODO: double check this matrix for christ sake
function standard_vote_matrix()
    s₁ = sp.symbols("s₁")
    s₂ = sp.symbols("s₂")

 [1 s₁ s₁ 1 s₂ s₂ 0 0 0 0 0 0 s₁ 1 s₂ s₂ 1 s₁ s₁ 1 s₂ s₂ 1 s₁;
                         s₁ 1 s₂ s₂ 1 s₁ s₁ 1 s₂ s₂ 1 s₁ 0 0 0 0 0 0 1 s₁ s₁ 1 s₂  s₂;
                         s₂ s₂ 1 s₁ s₁ 1 1 s₁ s₁ 1 s₂ s₂ s₂ s₂ 1 s₁ s₁ 1 0 0 0 0 0 0;
                         0 0 0 0 0 0 s₂ s₂ 1 s₁ s₁ 1 1 s₁ s₁ 1 s₂ s₂ s₂ s₂ 1 s₁ s₁ 1]
end

general_positional_vs() = standard_vote_matrix() * p_twentyfour()

function positional_voting_method_4candidates(concrete_s1, concrete_s2)
        s₁ = sp.symbols("s₁")
        s₂ = sp.symbols("s₂")
    map(x -> sp.simplify(1//(1 + concrete_s1 + concrete_s2) * sp.subs(x, zip((s₁,s₂),
                                                                (concrete_s1,concrete_s2))...)) ,
        general_positional_vs())
end


plurality_four_candidates() =  positional_voting_method_4candidates(0,0)

antiplurality_four_candidates() = positional_voting_method_4candidates(1,1)

vote_for_two_four_candidates() =  positional_voting_method_4candidates(1,0)


function get_positional_voting_numeric_vectors(symbolic_positional_vector,
                                              ps, baseline_sym_ps = p_twentyfour)
        replacing_dict = Dict(zip(baseline_sym_ps, ps))
        map(x-> x.subs(replacing_dict), symbolic_positional_vector)
end


function props_raw_choice_cols(min_raw)

  turnedintovecs = Vector{String}.(eachrow(min_raw))
  filterothers = filter(x->all(y-> y != "other", x), turnedintovecs)
    filteredproportions = collect(proportionmap(filterothers))
  filtered_df_props = DataFrame(:ranking_vectors => map(first, filteredproportions),
                                :props => map(x->x[2], filteredproportions))
  return(filtered_df_props)
end



function getp_4candidates(df)
  props = props_raw_choice_cols(df)
  candidate_key_dict = zip(candidates, ("A", "B", "C", "D")) |> Dict
  key_candidate_dict = zip( ("A", "B", "C", "D"), candidates) |> Dict


# A,B,C,D | B,A,C,D | C,A,B,D | A,C,B,D | B,C,A,D | C,B,A,D | C,B,D,A | B,C,D,A | D,C,B,A | C,D,B,A | B,D,C,A | D,B,C,A | D,A,C,B | A,D,C,B | C,D,A,B | D,C,A,B | A,C,D,B | C,A,D,B | B,A,D,C | A,B,D,C | D,B,A,C | B,D,A,C | A,D,B,C | D,A,B,C |
  data_permutations = props[!,:ranking_vectors]

  permutations_vector = [
      ["A", "B", "C", "D"], ["B", "A", "C", "D"], ["C", "A", "B", "D"], ["A", "C", "B", "D"],
      ["B", "C", "A", "D"], ["C", "B", "A", "D"], ["C", "B", "D", "A"], ["B", "C", "D", "A"],
      ["D", "C", "B", "A"], ["C", "D", "B", "A"], ["B", "D", "C", "A"], ["D", "B", "C", "A"],
      ["D", "A", "C", "B"], ["A", "D", "C", "B"], ["C", "D", "A", "B"], ["D", "C", "A", "B"],
      ["A", "C", "D", "B"], ["C", "A", "D", "B"], ["B", "A", "D", "C"], ["A", "B", "D", "C"],
      ["D", "B", "A", "C"], ["B", "D", "A", "C"], ["A", "D", "B", "C"], ["D", "A", "B", "C"]]

  indices_for_p = [findfirst(permutation -> permutation == map(i -> candidate_key_dict[i],
          j), permutations_vector) for j in data_permutations]

  idx_permutations_not_in_df = setdiff(1:24, indices_for_p)

  baseline_permutations_not_in_df = map(x->permutations_vector[x],
                             idx_permutations_not_in_df)

  permutations_not_in_df = map(y->(x-> key_candidate_dict[x]).(y), baseline_permutations_not_in_df)

  zeroes_props = zeros(length(permutations_not_in_df))

  missing_rankings = DataFrame(:ranking_vectors => permutations_not_in_df,
                             :props => zeroes_props,
                             :index_in_p => idx_permutations_not_in_df)

  ## TODO: Check if these missing permutations are not some awful bug!!!

  props[!,:index_in_p] = indices_for_p

  append!(props, missing_rankings)
  resorted_filtered = sort(props,:index_in_p)
  p = Vector{Float64}(resorted_filtered.props) .|> x->round(x,digits=4)
  return(p)
end

function baseline_tetrahedron()
    mysimplex = rcopy(R"""
simplex <- function(n) {
  qr.Q(qr(matrix(1, nrow=n)) ,complete = TRUE)[,-1]
}
""")
    mysimplex(4)
end


midpoint3d(p1,p2) = ( (p1[1] + p2[1] )/2, (p1[2] + p2[2])/2, (p1[3] + p2[3])/2 )


function cart_of_method(method, p)
    geometry = rimport("geometry")
  normalp = Vector{Float64}(get_positional_voting_numeric_vectors(method,
  p))
  cartp = rcopy(geometry.bary2cart(baseline_tetrahedron(),normalp)) |> Tuple
  return(cartp)
end
