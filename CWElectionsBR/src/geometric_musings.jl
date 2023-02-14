## i dont remeber how I calculated that translation LOL -> took it from the julia pkg for ternary plots!!!
tern2cart(a, b, c) = (1 / 2 * (2b + c) / (a + b + c), √3 / 2 * (c / (a + b + c)))
midpoint(p1,p2) = ( (p1[1] + p2[1] )/2, (p1[2] + p2[2])/2)

function turn_into_euclideanpoint(point)
    tern2cart(point...) |> GeometryBasics.Point2
end

function plain_triangle()


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


    return(basic_3candidate_triangle)
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


# This gives the q_s!!!!
function qₛ_4candidates(concrete_s1, concrete_s2)
        s₁ = sp.symbols("s₁")
        s₂ = sp.symbols("s₂")
    map(x -> sp.simplify(1//(1 + concrete_s1 + concrete_s2) * sp.subs(x, zip((s₁,s₂),
                                                                (concrete_s1,concrete_s2))...)) ,
        general_positional_vs())
end


plurality_4c_qₛ() =  qₛ_4candidates(0,0)

antiplurality_4c_qₛ() = qₛ_4candidates(1,1)

vote_for_two_4c_qₛ() =  qₛ_4candidates(1,0)

borda_4c_qₛ() = qₛ_4candidates(2//3,1//3)


function get_4c_wₛ(ps)
        replacing_dict = Dict(zip(p_twentyfour(), ps))
        map(x-> x.subs(replacing_dict), general_positional_vs())
end

function get_method_4c_wₛ_numeric(ps, vs1,vs2) ## note I already am replacing both ps and s here
    ## this is different from what I'm doing with qs
    ws = get_4c_wₛ(ps)
    s₁ = sp.symbols("s₁")
    s₂ = sp.symbols("s₂")
    map(x -> sp.simplify(sp.subs(x, zip((s₁,s₂),
                                        (vs1,vs2))...)),
        ws)
end


plurality_4c_wₛ_num(ps) =  get_method_4c_wₛ_numeric(ps,0,0) |> Vector{Float64} |> v-> round.(v, digits = 4)
antiplurality_4c_wₛ_num(ps) = get_method_4c_wₛ_numeric(ps,1,1) |> Vector{Float64} |> v-> round.(v, digits = 4)
vote_for_two_4c_wₛ_num(ps) =  get_method_4c_wₛ_numeric(ps,1,0) |> Vector{Float64} |> v-> round.(v, digits = 4)
borda_4c_wₛ_num(ps) = get_method_4c_wₛ_numeric(ps, 2//3,1//3)  |> Vector{Float64} |> v-> round.(v, digits = 4)





function get_numericOf_qₛ(symbolic_vector,
                                              ps, baseline_sym_ps = p_twentyfour())
        replacing_dict = Dict(zip(baseline_sym_ps, ps))
        map(x-> x.subs(replacing_dict), symbolic_vector)
end


function props_raw_choice_cols(min_raw, prop_or_freq = "prop")

  turnedintovecs = Vector{String}.(eachrow(min_raw))
  filterothers = filter(x->all(y-> y != "other", x), turnedintovecs)
  if prop_or_freq == "freq"
    filteredproportions = collect(countmap(filterothers))
  else 
    filteredproportions = collect(proportionmap(filterothers))
  end
  filtered_df_props = DataFrame(:ranking_vectors => map(first, filteredproportions),
                                :props => map(x->x[2], filteredproportions))
  return(filtered_df_props)
end



function getp_4candidates(df, prop_or_freq = "prop")
  props = props_raw_choice_cols(df, prop_or_freq)
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
  normalp = Vector{Float64}(get_numericOf_qₛOrwₛ(method,
  p))
  cartp = rcopy(geometry.bary2cart(baseline_tetrahedron(),normalp)) |> Tuple
  return(cartp)
end









function plain_opened_tetrahedron(;title = "")
  foo =  plain_triangle()  
  avertex = (0,0)
  bvertex = (1.,0.)
  cvertex = (0.5,sqrt(3)/2)
#  plainpoints = [avertex, bvertex, cvertex]
  d1 = (-0.5, sqrt(3)/2)
  d2 = (1.5,sqrt(3)/2)
  d3 = (0.5, -sqrt(3)/2)
  ds = [d1,d2,d3]
  #scatter!(foo.axis,[d1,d2,d3])
  poly!(foo.axis,GeometryBasics.Polygon(GeometryBasics.Point2.([d1,d2,d3,d1])),
  color =:transparent, strokecolor =:black, strokewidth = 1, title =title)  
  lines!(foo.axis, [bvertex,d1], color =:black)
  lines!(foo.axis, [avertex,d2], color =:black)
  lines!(foo.axis, [cvertex,d3], color =:black)
  lines!(foo.axis,[cvertex, midpoint(avertex, d1)], color=:black)
  lines!(foo.axis,[cvertex, midpoint(bvertex, d2)], color=:black)
  lines!(foo.axis,[avertex, midpoint(bvertex, d3)], color=:black)
  lines!(foo.axis,[bvertex, midpoint(avertex, d3)], color=:black)
  lines!(foo.axis,[avertex, midpoint(cvertex, d1)], color=:black)
  lines!(foo.axis,[bvertex, midpoint(cvertex, d2)], color=:black)
  
   hidespines!(foo.axis)
  hidexdecorations!(foo.axis)
  hideydecorations!(foo.axis) 
  return(foo)
end



positions_tetrahedron = [(permutation = "A,B,C,D" , position =   (0.37, 0.1)),
                          (permutation = "B,A,C,D" , position =  (0.63, 0.1)),
                          (permutation = "C,A,B,D" , position =  (0.37,0.53)),
                          (permutation = "A,C,B,D" , position =  (0.27, 0.27)),
                          (permutation = "B,C,A,D" , position =  (0.67 , 0.27)),
                          (permutation = "C,B,A,D" , position =  (0.57 , 0.53)),
                          (permutation = "C,B,D,A" , position =   (0.75,0.60)),
                          (permutation = "B,C,D,A" , position =  (0.87,0.33)),
                          (permutation = "D,C,B,A" , position =  (1.13,0.77)),
                          (permutation = "C,D,B,A" , position =  ( 0.83, 0.77)),
                          (permutation = "B,D,C,A" , position =  (1.07, 0.33)),
                          (permutation = "D,B,C,A" , position =  (1.23,0.6)),
                          (permutation = "D,A,C,B" , position =  (-0.25 , 0.6)),
                          (permutation = "A,D,C,B", position =  (-0.13, 0.33)),
                          (permutation = "C,D,A,B", position =  (0.17, 0.77)),
                          (permutation = "D,C,A,B", position =  (-0.17, 0.77)),
                          (permutation = "A,C,D,B", position =  (0.07, 0.33)),
                          (permutation = "C,A,D,B", position =  (0.25, 0.6)),
                          (permutation = "B,A,D,C", position =  (0.63, -0.07)),
                          (permutation = "A,B,D,C", position =  (0.37, -0.07)),
                          (permutation = "D,B,A,C", position =  (0.57, -0.53)),
                          (permutation = "B,D,A,C", position =  (0.77,-0.23)),
                          (permutation = "A,D,B,C", position =  (0.23, -0.23)),
                          (permutation = "D,A,B,C", position =  ( 0.37 , -0.53))]

   
function representation_tetrahedron_freqs(p4c,
  candidates = candidates, perms_poss = positions_tetrahedron; 
  coerce_to_int = true, 
  textsize = 15, 
  title = "" )

  ot = plain_opened_tetrahedron(title =title)
  d1 = (-0.5, sqrt(3)/2,0)
  d2 = (1.5,sqrt(3)/2,0)

  d3 = (0.5, -sqrt(3)/2,0)
  if coerce_to_int 
    for (p,pos) in zip(p4c, perms_poss)
  text!(string(Int(p)), position = pos.position, textsize = textsize)
  end    
else
  for (p,pos) in zip(p4c, perms_poss)
    text!(string(p), position = pos.position, textsize = textsize)
    end      
end  

  text!(candidates[1], position = (-0.29,-0.05))
  text!(candidates[2], position = (1.05,-0.05))
  text!(candidates[3], position = (0.48,0.867))

  ot.axis.title = title
  text!(candidates[4], position = d1[1:2])
  text!(candidates[4], position = d2[1:2] .+ (-0.16,0.))
  text!(candidates[4], position = d3[1:2] .+ (-0.1,-0.09))
  return(ot)
end



function getpl_apl_v42(p)

  pl =  get_numericOf_qₛ(plurality_4c_qₛ(), p)  |> Vector{Float64} |> v-> round.(v, digits = 4)

  apl = get_numericOf_qₛ(antiplurality_4c_qₛ(), p)  |> Vector{Float64} |> v-> round.(v, digits = 4)

  v42 = get_numericOf_qₛ(vote_for_two_4c_qₛ(), p)  |> Vector{Float64} |> v-> round.(v, digits = 4)

  #borda = get_numericOf_qₛ(borda_4c_qₛ(), p)  |> Vector{Float64} |> v-> round.(v, digits = 4)
  return(pl,apl,v42)

end


                          

function get_tpl_tapl_tv42(pl,apl,v42)
  na  = (0.,0.,0.)
  nb = (1.,0.,0.)
  nc = (0.5,sqrt(3)/2,0.)
  ncentr = (0.5,  0.28867513459481287, sqrt(6)/3)
  helper = [na,nb,nc,ncentr]
  tetra_matrix = hcat(first.(helper), (x->x[2]).(helper), last.(helper))
  geometry = rimport("geometry")
  tpl = rcopy(geometry.bary2cart(tetra_matrix,pl)) |> Tuple 
  tapl = rcopy(geometry.bary2cart(tetra_matrix,apl)) |> Tuple 
  tv42 = rcopy(geometry.bary2cart(tetra_matrix,v42)) |> Tuple 
  #tborda = rcopy(geometry.bary2cart(tetra_matrix,borda)) |> Tuple 
  return(tpl,tapl,tv42)#,tborda)
end

drop_idx(vec, idx) = vec[eachindex(vec) .∉ Ref(idx)] # this is actually super useful in general, should be in an utils package 

standardize(vec) = vec./sum(vec)

function filled_tetrahedron(tpl,tapl,tv42,candidates = candidates) 
    ot = plain_opened_tetrahedron()
    d1 = (-0.5, sqrt(3)/2,0)
    d2 = (1.5,sqrt(3)/2,0)

    d3 = (0.5, -sqrt(3)/2,0)

    ds = [d1,d2,d3]

    centroid_base = Meshes.Triangle((0.,0.), (1.,0.), (0.5, sqrt(3)/2)) |> Meshes.centroid 
    new_closestd(point,ds = ds) = map(x->evaluate(Euclidean(), point,x), 
    ds) |> argmin
    
    centroid_base3 = (centroid_base.coords[1],centroid_base.coords[2],0.0)

    
    truncpl =  standardize(tpl[1:2])

    truncapl = standardize(tapl[1:2])
    
    truncv42 = standardize(tv42[1:2])
    
    
    tpl_pull = (truncpl .- centroid_base3[1:2]) .+  ds[new_closestd(tpl)][1:2]
    
    tapl_pull = (truncapl .- centroid_base3[1:2]) .+  ds[new_closestd(tapl)][1:2]
    
    tv42_pull = (truncv42 .- centroid_base3[1:2]) .+  ds[new_closestd(tv42)][1:2] 
    

    possible_pulled_pl = (1-(tpl[3])) .* truncpl  .+ (tpl[3]) .* tpl_pull 
    
    possible_pulled_apl = (1-(tapl[3])) .* truncapl  .+ (tapl[3]) .* tapl_pull 
    
    possible_pulled_v42 = (1-(tv42[3])) .* truncv42  .+ (tv42[3]) .* tv42_pull
    
    possible_pulled_borda = (possible_pulled_apl .+ possible_pulled_pl .+ possible_pulled_v42) ./ 3
    
    
    
    scatter!(ot.axis,[possible_pulled_pl], color = :black, markersize = 10, marker = :utriangle) 
        
    scatter!(ot.axis,[possible_pulled_apl], color = :black, markersize = 10, marker = :dtriangle) 
    
    
    scatter!(ot.axis,[possible_pulled_v42], color = :black, markersize = 10) 
            
    scatter!(ot.axis,[possible_pulled_borda], color = :black, markersize = 10, marker = :diamond) 
    
    poly!(ot.axis,GeometryBasics.Polygon(GeometryBasics.Point2.([possible_pulled_pl,
    possible_pulled_apl,possible_pulled_v42,possible_pulled_pl])),
    color =:transparent, strokecolor =:black, strokewidth = 1)

  
    text!(candidates[1], position = (-0.29,-0.05))
    text!(candidates[2], position = (1.05,-0.05))
    text!(candidates[3], position = (0.48,0.867))
  
    
    text!(candidates[4], position = d1[1:2])
    text!(candidates[4], position = d2[1:2] .+ (-0.16,0.))
    text!(candidates[4], position = d3[1:2] .+ (-0.1,-0.09))

    hidespines!(ot.axis)
    hidexdecorations!(ot.axis)
    hideydecorations!(ot.axis)

    return(ot)    
end