using Revise 
import Pkg
Pkg.activate("./")


import CWElectionsBR as cw 

#using MeshViz
using GeometryBasics
using GLMakie
using Combinatorics
import Meshes
import SymPy as sp 
import MeshViz
import CSV 
using DataFrames
using StatsBase
using RCall
using Distances 
using MeshViz
using LinearAlgebra
using Rotations

dfspath = "../rscripts/dfs/"

mincw1 = CSV.read(dfspath * "min_c1_raw.csv", DataFrame)

p4c = cw.getp_4candidates(mincw1)


#=
So, the tetrahedron vertices are (1,0,0,0), (0,1,0,0), (0,0,1,0) and (0,0,0,1)
=#

#= tetrahedron_points = cw.baseline_tetrahedron()



tetrapoints = map(Tuple{Float32,Float32, Float32},[collect(eachrow(tetrahedron_points))...])


## tetrapoints2 = [(0,0,0), (1,1,0), (1,0,1), (0,1,1)]


midpoint3d(p1,p2) = ( (p1[1] + p2[1] )/2, (p1[2] + p2[2])/2, (p1[3] + p2[3])/2 )

pointspairs = combinations(tetrapoints, 2) |> collect 

midpointss = [ midpoint3d(pair...) for pair in pointspairs]


firstps = map(x->x[1],pointspairs)
secondps = map(x->x[2],pointspairs)


midpointss

pointspairs

tetramesh = Meshes.Tetrahedron(tetrapoints)

##tetramesh2 = Meshes.Tetrahedron(tetrapoints2)

tetra_centroid =  Meshes.centroid(tetramesh)

##tetra_centroid2 =  Meshes.centroid(tetramesh2)


vertex_combs = combinations([GeometryBasics.Point3f0.(tetrapoints)...,
GeometryBasics.Point3f0(tetra_centroid.coords)], 3)

foo = wireframe(GeometryBasics.Point3f0.(tetrapoints[1:3]), show_axis = false )

wireframe!(foo.axis, GeometryBasics.Point3f0.(tetrapoints[2:4]))
wireframe!(foo.axis, 
[GeometryBasics.Point3f0(tetrapoints[4]),
GeometryBasics.Point3f0(tetrapoints[1]),
GeometryBasics.Point3f0(tetrapoints[2])] )

for comb in vertex_combs 
wireframe!(foo.axis, comb)
end  

stuff1 = zip(firstps, midpointss,
  repeat([tetra_centroid.coords], outer = 6))   |> collect 



for stuff in stuff1
  wireframe!(foo.axis, [GeometryBasics.Point3f0(stuff[1]),
  GeometryBasics.Point3f0(stuff[2]), GeometryBasics.Point3f0(stuff[3])])
end


annotations!( ["A", "B", "C", "D"], GeometryBasics.Point3f0.(tetrapoints))


#= 
zoom!(foo.axis.scene, 
cameracontrols(foo.axis.scene), 0.9, false)
 =#


rotate!(foo.axis.scene, 0.2)
mypol3 = Polygon(tetrapoints .|> GeometryBasics.Point3f0)

plt3 = MeshViz.viz(Meshes.Tetrahedron(tetrapoints),
 showboundary = true,
     facetcolor = :green,
   showfacets= true,
    alpha = 0., show_axis = false 
     )

for comb in vertex_combs 
      wireframe!(plt3.axis, comb, linewidth = 2, show_axis = false )
end  
      
 annotations!( ["A", "B", "C", "D"], GeometryBasics.Point3f0.(tetrapoints))      
plt3
 =#

## Preprocessing For visualization 

#= dfspath = "../rscripts/dfs/"

mincw1 = CSV.read(dfspath * "min_c1_raw.csv", DataFrame)

p4c = cw.getp_4candidates(mincw1)
 =#
#= cart_antiplurality = cart_of_method(antiplurality_four_candidates, p4c) 
cart_plurality = cart_of_method(plurality_four_candidates, p4c) 
cart_vote_for_two = cart_of_method(vote_for_two_four_candidates, p4c) 



ngon = Meshes.Ngon([cart_antiplurality,cart_plurality, cart_vote_for_two]...)
 =#


#= MeshViz.viz!(plt3.axis,ngon, color = :white, alpha = 0.01)
plt3

foo
meshscatter!(foo.axis,[cart_antiplurality,cart_plurality, cart_vote_for_two], markersize = 0.01)

wireframe!(foo.axis,GeometryBasics.Point3f0.([cart_antiplurality,cart_plurality, cart_vote_for_two]))
 
 foo
 =#



## Opened tetrahedron tests  ------------------------

#= 
Graphics[{RGBColor[1, 1, 0.85], EdgeForm[GrayLevel[0]],
  GraphicsComplex[{{0, 0}, {1/2, Sqrt[3]/2}, {1, 0},
    {1, Sqrt[3]}, {3/2, Sqrt[3]/2}, {2, 0}},
   Polygon[{{1, 3, 2}, {4, 2, 5}, {3, 5, 2}, {5, 3, 6}}]]}]
 =#

 test_points = [(0, 0), (1/2, sqrt(3)/2), (1, 0),
 (1, sqrt(3)), (3/2, sqrt(3)/2), (2, 0)]

# I think those are not the correct external points 

external_points = [(0.,0.), (2.,0.), (1., sqrt(3))]    

internal_points = [(1/2, sqrt(3)/2),
               (3/2, sqrt(3)/2), 
               (1., 0.)] 

t1 = [(0., 0.), (1/2, sqrt(3)/2), (1., 0.) ] .|> GeometryBasics.Point2
t2 = [(1., 0.), (3/2, sqrt(3)/2), (2.,0.) ] .|> GeometryBasics.Point2
t3 = [(1., sqrt(3)), (1/2, sqrt(3)/2), (3/2, sqrt(3)/2)] .|> GeometryBasics.Point2
t4 = [(1., 0.), (1/2, sqrt(3)/2), (3/2, sqrt(3)/2)] .|> GeometryBasics.Point2


mypol = Polygon(external_points .|> GeometryBasics.Point2,
[t1,t2,t3,t4])

plt = poly(mypol,  color = :white, strokecolor = :black, strokewidth = 1 )

function makeline(deplot, pair) 
lines!(deplot.axis, 
     [pair[1] |> GeometryBasics.Point2,
     pair[2] |> GeometryBasics.Point2],
     color = :black)
end

#= 
plt

cw.tern2cart([1//3,1//3,1//3])

avertex= cw.midpoint((0.,0.), (1.,sqrt(3)))

cvertex = (1.,0.)

bvertex = cw.midpoint((2.0,0.0), (1., sqrt(3)/2)) # 1.5,sqrt(3)/2

linepairs = [[[1., 0.] , [1.,sqrt(3)]],
[[0., 0.] , [1.5, sqrt(3)/2]],
[[2., 0.] , [0.5, sqrt(3)/2]],
[[0.5, sqrt(3)/2] , [0.5, 0.]],
[[1.5, sqrt(3)/2] , [1.5, 0.]],
[CWElectionsBR.midpoint([0.0,0.0],[0.5, sqrt(3)/2]) , [1., 0.]],
[CWElectionsBR.midpoint([2.0,0.0],[1.5, sqrt(3)/2]) , [1., 0.]],
[CWElectionsBR.midpoint([0.5,sqrt(3)/2],[1., sqrt(3)]) , [1.5, sqrt(3)/2]],
[CWElectionsBR.midpoint([1.5,sqrt(3)/2],[1., sqrt(3)]) , [.5, sqrt(3)/2]]]

for p in linepairs 
makeline(plt, p) 
end

plt                  

for i in external_points
text!("D",position=i)
end

for (i,j) in zip(["A","B", "C"], [avertex,bvertex,cvertex])
text!(i,position=j)
end
plt

plt.axis.yreversed = true

#= 
circle_around_a = cw.GeometryBasics.Circle{Float64}(avertex,abs(1-0.196))
circle_around_b = cw.GeometryBasics.Circle{Float64}(bvertex,abs(1-0.298))
circle_around_c = cw.GeometryBasics.Circle{Float64}(cvertex,abs(1-0.285))

circles_around_d = [ cw.GeometryBasics.Circle{Float64}(dv,abs(1-0.219))
 for dv in external_points] =#


 circle_around_a = cw.GeometryBasics.Circle{Float64}(avertex,1)
 circle_around_b = cw.GeometryBasics.Circle{Float64}(bvertex,1)
 circle_around_c = cw.GeometryBasics.Circle{Float64}(cvertex,1)
 
 circles_around_d = [ cw.GeometryBasics.Circle{Float64}(dv,1)
  for dv in external_points]
 
 


 for cic in [circle_around_a, circle_around_b, circle_around_c]
poly!(plt.axis, cic, color = :transparent, 
strokecolor = :blue, strokewidth = 2)
end


for cic in circles_around_d
  poly!(plt.axis, cic, color = :transparent, 
  strokecolor = :blue, strokewidth = 2)
end

circle_around_a2 = cw.GeometryBasics.Circle{Float64}(avertex,abs(1-0.196))
circle_around_b2 = cw.GeometryBasics.Circle{Float64}(bvertex,abs(1-0.298))
circle_around_c2 = cw.GeometryBasics.Circle{Float64}(cvertex,abs(1-0.285))

circles_around_d2 = [ cw.GeometryBasics.Circle{Float64}(dv,abs(1-0.219))
 for dv in external_points]

 for cic in [circle_around_a2, circle_around_b2, circle_around_c2]
  poly!(plt.axis, cic, color = :transparent, 
  strokecolor = :red, strokewidth = 2)
  end
  
  
  for cic in circles_around_d2
    poly!(plt.axis, cic, color = :transparent, 
    strokecolor = :red, strokewidth = 2)
  end

plt
  
 =#
## -----------------------------------------------------------

vertex_combs = combinations([GeometryBasics.Point3f0.(tetrapoint2)...,
GeometryBasics.Point3f0(tetra_centroid2.coords)], 3)

fig = Figure(resolution = (600,600))
ax = LScene(fig[1,1], show_axis=false)


#= tetrapoints2

foo = wireframe(GeometryBasics.Point3f0.(tetrapoints2[1:3]), show_axis = false )


GeometryBasics.Point3f0.(tetrapoints2[1:3])


foo

wireframe!(foo.axis, GeometryBasics.Point3f0.(tetrapoints2[2:4]))

tetrapoints2


wireframe!(foo.axis, 
[GeometryBasics.Point3f0(tetrapoints2[4]),
GeometryBasics.Point3f0(tetrapoints2[1]),
GeometryBasics.Point3f0(tetrapoints2[2])] )

for comb in vertex_combs 
wireframe!(foo.axis, comb)
end  

 =#

 # -- Using my base triangle 

#= foo =  cw.plain_triangle()


avertex = (0,0)
bvertex = (1.,0.)
cvertex = (0.5,sqrt(3)/2)

plainpoints = [avertex,bvertex, cvertex]

d1 = (-0.5, sqrt(3)/2)
d2 = (1.5,sqrt(3)/2)
d3 = (0.5, -sqrt(3)/2)

ds = [d1,d2,d3]

#scatter!(foo.axis,[d1,d2,d3])

poly!(foo.axis,cw.GeometryBasics.Polygon(cw.GeometryBasics.Point2.([d1,d2,d3,d1])),
color =:transparent, strokecolor =:black, strokewidth = 1)



lines!(foo.axis, [bvertex,d1], color =:black)
lines!(foo.axis, [avertex,d2], color =:black)
lines!(foo.axis, [cvertex,d3], color =:black)

lines!(foo.axis,[cvertex, cw.midpoint(avertex, d1)], color=:black)
lines!(foo.axis,[cvertex, cw.midpoint(bvertex, d2)], color=:black)
lines!(foo.axis,[avertex, cw.midpoint(bvertex, d3)], color=:black)
lines!(foo.axis,[bvertex, cw.midpoint(avertex, d3)], color=:black)
lines!(foo.axis,[avertex, cw.midpoint(cvertex, d1)], color=:black)
lines!(foo.axis,[bvertex, cw.midpoint(cvertex, d2)], color=:black)


dfspath = "../rscripts/dfs/"

mincw1 = CSV.read(dfspath * "min_c1_raw.csv", DataFrame)

p4c = cw.getp_4candidates(mincw1) =#

pl =  cw.get_numericOf_qₛ(cw.plurality_4c_qₛ(), p4c)  |> Vector{Float64} |> v-> round.(v, digits = 4)

apl = cw.get_numericOf_qₛ(cw.antiplurality_4c_qₛ(), p4c)  |> Vector{Float64} |> v-> round.(v, digits = 4)

v42 = cw.get_numericOf_qₛ(cw.vote_for_two_4c_qₛ(), p4c)  |> Vector{Float64} |> v-> round.(v, digits = 4)

borda = cw.get_numericOf_qₛ(cw.borda_4c_qₛ(), p4c)  |> Vector{Float64} |> v-> round.(v, digits = 4)


apl

pl
#= 

truncpl = map(x->x + pl[4]/3,pl)[1:3]

truncapl = map(x->x + apl[4]/3,apl)[1:3]

truncv42 = map(x->x + v42[4]/3,v42)[1:3]

truncborda = map(x->x + borda[4]/3,borda)[1:3]

 =#
foo
#= 

scatter!(foo.axis, [cw.tern2cart(truncv42... ), 
                    cw.tern2cart(truncpl... ),
                    cw.tern2cart(truncapl... )]) =#






closestd(point) = map(x->evaluate(Euclidean(), cw.tern2cart(point...),x), 
[d1,d2,d3]) |> argmin


#= function newpoint(point,untruncated_point)
  (1-untruncated_point[4]) .* cw.tern2cart(point...)  .+
   untruncated_point[4] .* ds[closestd(point)]
end
 =#
#= function plot_baselineAnd_corrected(truncated_baseline,untruncated)
  scatter!(foo.axis, [cw.tern2cart(truncated_baseline...)])                    
  lines!(foo.axis,[cw.tern2cart(truncated_baseline...),
   ds[closestd(truncated_baseline)]], color=:blue)

scatter!(foo.axis, [newpoint(truncated_baseline, untruncated)], color = :red)
end
 =#

#= pullapl = newpoint(truncapl,apl)
pullpl  = newpoint(truncpl,pl)
pullv42 = newpoint(truncv42,v42)
pullborda = newpoint(truncborda,borda)
 =#

#= poly!(foo.axis,cw.GeometryBasics.Polygon(cw.GeometryBasics.Point2.([pullapl,
pullpl,pullv42,pullapl])),
color =:transparent, strokecolor =:black, strokewidth = 1)

scatter!(foo.axis,[pullapl,
pullpl,pullv42,pullapl], color = :black, markersize = 5)

scatter!(foo.axis,[pullborda], color = :blue, markersize = 5)
 =#

foo

truncapl |> println

foo


d1 = (-0.5, sqrt(3)/2)
d2 = (1.5,sqrt(3)/2)
d3 = (0.5, -sqrt(3)/2)

ds = [d1,d2,d3]

## tetrahedron with permuted candidates 
#foo =  cw.plain_triangle(["A", "B","C"])

function plain_opened_tetrahedron()
  foo =  cw.plain_triangle()  
  avertex = (0,0)
  bvertex = (1.,0.)
  cvertex = (0.5,sqrt(3)/2)
  plainpoints = [avertex, bvertex, cvertex]
  d1 = (-0.5, sqrt(3)/2)
  d2 = (1.5,sqrt(3)/2)
  d3 = (0.5, -sqrt(3)/2)
  ds = [d1,d2,d3]
  #scatter!(foo.axis,[d1,d2,d3])
  poly!(foo.axis,cw.GeometryBasics.Polygon(cw.GeometryBasics.Point2.([d1,d2,d3,d1])),
  color =:transparent, strokecolor =:black, strokewidth = 1)
  lines!(foo.axis, [bvertex,d1], color =:black)
  lines!(foo.axis, [avertex,d2], color =:black)
  lines!(foo.axis, [cvertex,d3], color =:black)
  lines!(foo.axis,[cvertex, cw.midpoint(avertex, d1)], color=:black)
  lines!(foo.axis,[cvertex, cw.midpoint(bvertex, d2)], color=:black)
  lines!(foo.axis,[avertex, cw.midpoint(bvertex, d3)], color=:black)
  lines!(foo.axis,[bvertex, cw.midpoint(avertex, d3)], color=:black)
  lines!(foo.axis,[avertex, cw.midpoint(cvertex, d1)], color=:black)
  lines!(foo.axis,[bvertex, cw.midpoint(cvertex, d2)], color=:black)
  
  #= hidespines!(foo.axis)
  hidexdecorations!(foo.axis)
  hideydecorations!(foo.axis)
 =#

  return(foo)
end


ot = plain_opened_tetrahedron()

# simp = cw.baseline_tetrahedron()

na  = (0.,0.,0.)
nb = (1.,0.,0.)
nc = (0.5,sqrt(3)/2,0.)
ncentr = (0.5,  0.28867513459481287, sqrt(6)/3)

helper = [na,nb,nc,ncentr]

tetra_matrix = hcat(first.(helper), (x->x[2]).(helper), last.(helper))

tetra_matrix


geometry = rimport("geometry")

function get_position_in_simplex(method,outer_candidate,tetra_base = tetra_matrix)
  base_of_tetra = drop_idx(method,outer_candidate)  
  new_tetra = [base_of_tetra...,method[outer_candidate]]

  rcopy(geometry.bary2cart(tetra_base,new_tetra)) |> Tuple 
end  



tpl

tpl = rcopy(geometry.bary2cart(tetra_matrix,pl)) |> Tuple 
tapl = rcopy(geometry.bary2cart(tetra_matrix,apl)) |> Tuple 
tv42 = rcopy(geometry.bary2cart(tetra_matrix,v42)) |> Tuple 
tborda = rcopy(geometry.bary2cart(tetra_matrix,borda)) |> Tuple 


t = Meshes.Tetrahedron([na,nb,nc,ncentr])



helper

tpl


helper 

tpl

d1

#= 
test_viz = MeshViz.viz(t, showfacets =true ,
 color = :green, strokecolor = :black, alpha = 0.1)

meshscatter!(test_viz.axis,[test], markersize = 0.01)
 =#
d1 = (-0.5, sqrt(3)/2,0)

d2 = (1.5,sqrt(3)/2,0)

d3 = (0.5, -sqrt(3)/2,0)

ds = [d1,d2,d3]

centroid_base = Meshes.Triangle((0.,0.), (1.,0.), (0.5, sqrt(3)/2)) |> Meshes.centroid 

new_closestd(point,ds = ds) = map(x->evaluate(Euclidean(), point,x), 
ds) |> argmin


centroid_base3 = (centroid_base.coords[1],centroid_base.coords[2],0.0)

ot

tpl

projectonto(a,b) = (a ⋅ b)/(b ⋅ b) .* b

function get_ot_projection(point,ds = ds)

  closestd =   ds[new_closestd(point)]
  new_center = (point[1],point[2],0.0)
  shiftedD = closestd .- new_center
  # BUG:  This only gives nonsense 
  test = ((1-point[3]) .* new_center  .+
   point[3] .* shiftedD) #.+ new_center
#  R = AngleAxis(π/2, shiftedD[1], shiftedD[2], shiftedD[3])

#=   shiftedD = map(x->x/sum(closestd), closestd)
  R = AngleAxis(π/2, shiftedD[1], shiftedD[2], shiftedD[3])
  shiftedpoint = [point[1],point[2],point[3]]
  projection = (shiftedpoint'*R)[1:2] |> Tuple =#
#= rosz(θ) = [cos(θ) -sin(θ) 0; 
           sin(θ) cos(θ) 0;
           0 0 1]
 =#

#$projection = projectonto(shiftedpoint,shiftedD) .+ centroid_base3
#(rosz(π/2) * collect((point .- closestd))  .+ collect(closestd) |> Tuple)[1:2]
  
#projection = ( rosz(π/2) ⋅ collect((point .- closestd))) .+ collect(point ) |> Tuple 
 #return(projection[1:2])
 #return(projection[1:2])
 return(test[1:2])
end  




dfspath = "../rscripts/dfs/"

mincw1 = CSV.read(dfspath * "min_c1_raw.csv", DataFrame)

p4c = cw.getp_4candidates(mincw1)

drop_idx(vec, idx) = vec[eachindex(vec) .∉ Ref(idx)] # this is actually super useful in general, should be in an utils package 

#= closestd(point) = map(x->evaluate(Euclidean(), cw.tern2cart(point...),x), 
[d1,d2,d3]) |> argmin

 =#

standardize(vec) = vec./sum(vec)
truncpl =  standardize(tpl[1:2])

truncapl = standardize(tapl[1:2])

truncv42 = standardize(tv42[1:2])

truncborda = standardize(tborda[1:2])


tpl_pull = (truncpl .- centroid_base3[1:2]) .+  ds[new_closestd(tpl)][1:2]

tapl_pull = (truncapl .- centroid_base3[1:2]) .+  ds[new_closestd(tapl)][1:2]

tv42_pull = (truncv42 .- centroid_base3[1:2]) .+  ds[new_closestd(tv42)][1:2] 

tborda_pull = (truncborda .- centroid_base3[1:2]) .+  ds[new_closestd(tborda)][1:2]


possible_pulled_pl = (1-(tpl[3])) .* truncpl  .+ (tpl[3]) .* tpl_pull 

possible_pulled_apl = (1-(tapl[3])) .* truncapl  .+ (tapl[3]) .* tapl_pull 

possible_pulled_v42 = (1-(tv42[3])) .* truncv42  .+ (tv42[3]) .* tv42_pull


possible_pulled_borda = (1-(tborda[3])) .* truncborda  .+ (tborda[3]) .* tborda_pull 








scatter!(ot.axis,[truncpl], color = :black, markersize = 10, marker = :utriangle)


scatter!(ot.axis,[tpl_pull[1:2]], color = :black, markersize = 10) 

lines!(ot.axis,[truncpl[1:2],tpl_pull[1:2]],color = :red)
tpl_pull

scatter!(ot.axis,[possible_pulled_pl], color = :black, markersize = 10, marker = :utriangle) 




scatter!(ot.axis,[truncapl], color = :black, markersize = 10, marker = :utriangle)


scatter!(ot.axis,[tapl_pull[1:2]], color = :black, markersize = 10) 

lines!(ot.axis,[truncapl[1:2],tapl_pull[1:2]],color = :red)

scatter!(ot.axis,[possible_pulled_apl], color = :black, markersize = 10, marker = :dtriangle) 



scatter!(ot.axis,[truncv42], color = :black, markersize = 10, marker = :utriangle)


scatter!(ot.axis,[tv42_pull[1:2]], color = :black, markersize = 10) 

lines!(ot.axis,[truncv42[1:2],tv42_pull[1:2]],color = :red)

scatter!(ot.axis,[possible_pulled_v42], color = :black, markersize = 10) 




scatter!(ot.axis,[truncborda], color = :black, markersize = 10, marker = :utriangle)


scatter!(ot.axis,[tborda_pull[1:2]], color = :black, markersize = 10) 

lines!(ot.axis,[truncborda[1:2],tborda_pull[1:2]],color = :red)

scatter!(ot.axis,[possible_pulled_borda], color = :black, markersize = 10, marker = :diamond) 


poly!(ot.axis,cw.GeometryBasics.Polygon(cw.GeometryBasics.Point2.([possible_pulled_pl,
possible_pulled_apl,possible_pulled_v42,possible_pulled_pl])),
color =:transparent, strokecolor =:black, strokewidth = 1)




function newpoint(point,untruncated_point, who_is_outer)
  #println(point)
  #println(sum(point))
  foo = (1-(untruncated_point)[who_is_outer]) .*cw.tern2cart(point...)  .+
   ((untruncated_point)[who_is_outer]) .* ds[closestd(point)]
   println(foo)
   return(foo)
end


function plot_4c_hull(ps, who_is_outer_vertex::Int, candidate_list::Vector{String} = cw.candidates)
  
  inner_candidates = drop_idx(candidate_list,who_is_outer_vertex) .|> titlecase
  outer_candidate = candidate_list[who_is_outer_vertex] .|> titlecase
  
  ot = plain_opened_tetrahedron()
  tpl,tapl,tv42,tborda = map(x->get_position_in_simplex(x,who_is_outer_vertex),
                             [pl,apl,v42,borda])
  ppl,papl,pv42,pborda = get_ot_projection.([tpl,tapl,tv42,tborda])

  poly!(ot.axis,cw.GeometryBasics.Polygon(cw.GeometryBasics.Point2.([papl,
  ppl,pv42,papl])),
  color =:transparent, strokecolor =:black, strokewidth = 1)
  scatter!(ot.axis,[papl], color = :black, markersize = 10, marker = :dtriangle)
  scatter!(ot.axis,[ppl], color = :black, markersize = 10, marker = :utriangle)
  scatter!(ot.axis,[pv42], color = :black, markersize = 10, marker = :circle)
  
  scatter!(ot.axis,[pborda], color = :blue, markersize = 10, marker =:diamond)
  
#=   for trunc in [truncapl,
    truncpl,truncv42,truncapl,truncborda]
  lines!(ot.axis,[cw.tern2cart(trunc...),
   ds[closestd(trunc)]], color=:red)
  end   
 =#
#=   poly!(ot.axis,cw.GeometryBasics.Polygon(cw.GeometryBasics.Point2.(map(x->cw.tern2cart(x...),[truncapl,
  truncpl,truncv42,truncapl]))),
  color =:transparent, strokecolor =:black, strokewidth = 1)
 =#
#=   scatter!(ot.axis,[cw.tern2cart(truncapl...)], color = :red, markersize = 10, marker = :dtriangle)
  scatter!(ot.axis,[cw.tern2cart(truncpl...)], color = :red, markersize = 10, marker = :utriangle)
  scatter!(ot.axis,[cw.tern2cart(truncv42...)], color = :red, markersize = 10, marker = :circle)
  
  scatter!(ot.axis,[cw.tern2cart(truncborda...)], color = :red, markersize = 10, marker =:diamond)
 =#  
  
  text!(inner_candidates[1], position = (-0.29,-0.05))
  text!(inner_candidates[2], position = (1.05,-0.05))
  text!(inner_candidates[3], position = (0.48,0.867))

  
  text!(outer_candidate, position = d1[1:2])
  text!(outer_candidate, position = d2[1:2] .+ (-0.16,0.))
  text!(outer_candidate, position = d3[1:2] .+ (-0.1,-0.09))
  
  return(ot)  
end  


foo3 = plot_4c_hull(p4c, 1)


apl




function plot_4c_hull(ps, who_is_outer_vertex::Int, candidate_list::Vector{String} = cw.candidates)

  pl =  cw.get_numericOf_qₛ(cw.plurality_4c_qₛ(), ps)  |> Vector{Float64} |> v-> round.(v, digits = 4)
  apl = cw.get_numericOf_qₛ(cw.antiplurality_4c_qₛ(), ps)  |> Vector{Float64} |> v-> round.(v, digits = 4)
  v42 = cw.get_numericOf_qₛ(cw.vote_for_two_4c_qₛ(), ps)  |> Vector{Float64} |> v-> round.(v, digits = 4)
  borda = cw.get_numericOf_qₛ(cw.borda_4c_qₛ(), ps)  |> Vector{Float64} |> v-> round.(v, digits = 4)


  truncpl = drop_idx(map(x->x + pl[who_is_outer_vertex]/3,pl), who_is_outer_vertex) |> normalize 
  truncapl = drop_idx(map(x->x + apl[who_is_outer_vertex]/3,apl),who_is_outer_vertex) |> normalize 
  truncv42 = drop_idx(map(x->x + v42[who_is_outer_vertex]/3,v42),who_is_outer_vertex) |> normalize 
  truncborda = drop_idx(map(x->x + borda[who_is_outer_vertex]/3,borda), who_is_outer_vertex) |> normalize 
  
  inner_candidates = drop_idx(candidate_list,who_is_outer_vertex) .|> titlecase
  outer_candidate = candidate_list[who_is_outer_vertex] .|> titlecase
  
  ot = plain_opened_tetrahedron()
  
  pullapl = newpoint(truncapl,apl,who_is_outer_vertex)
  pullpl  = newpoint(truncpl,pl,who_is_outer_vertex)
  pullv42 = newpoint(truncv42,v42,who_is_outer_vertex)
  pullborda = newpoint(truncborda,borda,who_is_outer_vertex)

  poly!(ot.axis,cw.GeometryBasics.Polygon(cw.GeometryBasics.Point2.([pullapl,
  pullpl,pullv42,pullapl])),
  color =:transparent, strokecolor =:black, strokewidth = 1)
  scatter!(ot.axis,[pullapl], color = :black, markersize = 10, marker = :dtriangle)
  scatter!(ot.axis,[pullpl], color = :black, markersize = 10, marker = :utriangle)
  scatter!(ot.axis,[pullv42], color = :black, markersize = 10, marker = :circle)
  
  scatter!(ot.axis,[pullborda], color = :blue, markersize = 10, marker =:diamond)
  
  for trunc in [truncapl,
    truncpl,truncv42,truncapl,truncborda]
  lines!(ot.axis,[cw.tern2cart(trunc...),
   ds[closestd(trunc)]], color=:red)
  end   

  poly!(ot.axis,cw.GeometryBasics.Polygon(cw.GeometryBasics.Point2.(map(x->cw.tern2cart(x...),[truncapl,
  truncpl,truncv42,truncapl]))),
  color =:transparent, strokecolor =:black, strokewidth = 1)

  scatter!(ot.axis,[cw.tern2cart(truncapl...)], color = :red, markersize = 10, marker = :dtriangle)
  scatter!(ot.axis,[cw.tern2cart(truncpl...)], color = :red, markersize = 10, marker = :utriangle)
  scatter!(ot.axis,[cw.tern2cart(truncv42...)], color = :red, markersize = 10, marker = :circle)
  
  scatter!(ot.axis,[cw.tern2cart(truncborda...)], color = :red, markersize = 10, marker =:diamond)
  
  
  text!(inner_candidates[1], position = (-0.29,-0.05))
  text!(inner_candidates[2], position = (1.05,-0.05))
  text!(inner_candidates[3], position = (0.48,0.867))

  
  text!(outer_candidate, position = d1)
  text!(outer_candidate, position = d2 .+ (-0.16,0.))
  text!(outer_candidate, position = d3 .+ (-0.1,-0.09))
  
  return(ot)  
end  


cw.get_numericOf_qₛ(cw.plurality_4c_qₛ(), p4c) 

cw.borda_4c_qₛ()

foo3 = plot_4c_hull(p4c, 1)



function plot_baselineAnd_corrected(truncated_baseline,untruncated)
  scatter!(foo.axis, [cw.tern2cart(truncated_baseline...)])                    
  lines!(foo.axis,[cw.tern2cart(truncated_baseline...),
   ds[closestd(truncated_baseline)]], color=:blue)
   scatter!(foo.axis, [newpoint(truncated_baseline, untruncated)], color = :red)
end


plw = cw.plurality_4c_wₛ_num(p4c) 


aplw


aplw= cw.antiplurality_4c_wₛ_num(p4c)

v42w= cw.vote_for_two_4c_wₛ_num(p4c) 



bordaw= cw.borda_4c_wₛ_num(p4c)

function newpoint(point,untruncated_point, who_is_outer)
  #println(point)
  #println(sum(point))
  foo = ((untruncated_point)[who_is_outer]) .*cw.tern2cart(point...)  .+
   (1-(untruncated_point)[who_is_outer]) .* ds[closestd(point)]
   println(foo)
   return(foo)
end


function plot_4c_hull2(ps, who_is_outer_vertex::Int, candidate_list::Vector{String} = cw.candidates)

  pl =  cw.get_numericOf_qₛ(cw.plurality_4c_qₛ(), ps)  |> Vector{Float64} |> v-> round.(v, digits = 4)
  apl = cw.get_numericOf_qₛ(cw.antiplurality_4c_qₛ(), ps)  |> Vector{Float64} |> v-> round.(v, digits = 4)
  v42 = cw.get_numericOf_qₛ(cw.vote_for_two_4c_qₛ(), ps)  |> Vector{Float64} |> v-> round.(v, digits = 4)
  borda = cw.get_numericOf_qₛ(cw.borda_4c_qₛ(), ps)  |> Vector{Float64} |> v-> round.(v, digits = 4)


  truncpl = drop_idx(pl, who_is_outer_vertex) 
  truncapl = drop_idx(apl,who_is_outer_vertex) 
  truncv42 = drop_idx(v42,who_is_outer_vertex)
  truncborda = drop_idx(borda, who_is_outer_vertex)
  
  inner_candidates = drop_idx(candidate_list,who_is_outer_vertex) .|> titlecase
  outer_candidate = candidate_list[who_is_outer_vertex] .|> titlecase
  
  ot = plain_opened_tetrahedron()

  
  pullapl = newpoint(truncapl,apl,who_is_outer_vertex)
  pullpl  = newpoint(truncpl,pl,who_is_outer_vertex)
  pullv42 = newpoint(truncv42,v42,who_is_outer_vertex)
  pullborda = newpoint(truncborda,borda,who_is_outer_vertex)

  poly!(ot.axis,cw.GeometryBasics.Polygon(cw.GeometryBasics.Point2.([pullapl,
  pullpl,pullv42,pullapl])),
  color =:transparent, strokecolor =:black, strokewidth = 1)
  scatter!(ot.axis,[pullapl], color = :black, markersize = 10, marker = :dtriangle)
  scatter!(ot.axis,[pullpl], color = :black, markersize = 10, marker = :utriangle)
  scatter!(ot.axis,[pullv42], color = :black, markersize = 10, marker = :circle)
  
  scatter!(ot.axis,[pullborda], color = :blue, markersize = 10, marker =:diamond)
  
  for trunc in [truncapl,
    truncpl,truncv42,truncapl,truncborda]
  lines!(ot.axis,[cw.tern2cart(trunc...),
   ds[closestd(trunc)]], color=:red)
  end   

  poly!(ot.axis,cw.GeometryBasics.Polygon(cw.GeometryBasics.Point2.(map(x->cw.tern2cart(x...),[truncapl,
  truncpl,truncv42,truncapl]))),
  color =:transparent, strokecolor =:black, strokewidth = 1)

  scatter!(ot.axis,[cw.tern2cart(truncapl...)], color = :red, markersize = 10, marker = :dtriangle)
  scatter!(ot.axis,[cw.tern2cart(truncpl...)], color = :red, markersize = 10, marker = :utriangle)
  scatter!(ot.axis,[cw.tern2cart(truncv42...)], color = :red, markersize = 10, marker = :circle)
  
  scatter!(ot.axis,[cw.tern2cart(truncborda...)], color = :red, markersize = 10, marker =:diamond)
  
  
  text!(inner_candidates[1], position = (-0.29,-0.05))
  text!(inner_candidates[2], position = (1.05,-0.05))
  text!(inner_candidates[3], position = (0.48,0.867))

  
  text!(outer_candidate, position = d1)
  text!(outer_candidate, position = d2 .+ (-0.16,0.))
  text!(outer_candidate, position = d3 .+ (-0.1,-0.09))
  
  return(ot)  
end  




foo2 = plot_4c_hull2(p4c, 1)

foo2


foo3