
import Pkg
Pkg.activate("./")

using CWElectionsBR

#using MeshViz
using GeometryBasics
using GLMakie
using Combinatorics
import Meshes
import SymPy as sp 
import MeshViz

using RCall

#=
So, the tetrahedron vertices are (1,0,0,0), (0,1,0,0), (0,0,1,0) and (0,0,0,1)
=#

mysimplex = rcopy(R"""
simplex <- function(n) {
  qr.Q(qr(matrix(1, nrow=n)) ,complete = TRUE)[,-1]
}
""")

bar = mysimplex(4)

tetrapoints

tetrapoints = map(Tuple{Float32,Float32, Float32},[collect(eachrow(mysimplex(4)))...])

## tetrapoints2 = [(0,0,0), (1,1,0), (1,0,1), (0,1,1)]


tetramesh = Meshes.Tetrahedron(tetrapoints)

##tetramesh2 = Meshes.Tetrahedron(tetrapoints2)

tetra_centroid =  Meshes.centroid(tetramesh)

##tetra_centroid2 =  Meshes.centroid(tetramesh2)



GeometryBasics.Point3f0(tetra_centroid.coords)

tetra_centroid |> typeof |> fieldnames

tetrapoints[4]



tetrapoints

vertex_combs = combinations([GeometryBasics.Point3f0.(tetrapoints)...,
GeometryBasics.Point3f0(tetra_centroid.coords)], 3)

fig = Figure(resolution = (600,600))
ax = LScene(fig[1,1], show_axis=false)

fig


foo = wireframe(GeometryBasics.Point3f0.(tetrapoints[1:3]), show_axis = false )

wireframe!(foo.axis, GeometryBasics.Point3f0.(tetrapoints[2:4]))
wireframe!(foo.axis, 
[GeometryBasics.Point3f0(tetrapoints[4]),
GeometryBasics.Point3f0(tetrapoints[1]),
GeometryBasics.Point3f0(tetrapoints[2])] )

for comb in vertex_combs 
wireframe!(foo.axis, comb)
end  

annotations!( ["A", "B", "C", "D"], GeometryBasics.Point3f0.(tetrapoints))




#= 
zoom!(foo.axis.scene, 
cameracontrols(foo.axis.scene), 0.9, false)
 =#
foo


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


  


# Testing saari tetrahedron -------------------------------------------------------------------------

s₁ = sp.symbols("s₁")
s₂ = sp.symbols("s₂")

p_twentyfour = [sp.Sym("p$i") for i in 1:24]

standard_vote_matrix  = [1 s₁ s₁ 1 s₂ s₂ 0 0 0 0 0 0 s₁ 1 s₂ s₂ 1 s₁ s₁ 1 s₂ s₂ 1 s₁;
                         s₁ 1 s₂ s₂ 1 s₁ s₁ 1 s₂ s₂ 1 s₁ 0 0 0 0 0 0 1 s₁ s₁ 1 s₂  s₂;
                         s₂ s₂ 1 s₁ s₁ 1 1 s₁ s₁ 1 s₂ s₂ s₂ s₂ 1 s₁ s₁ 1 0 0 0 0 0 0;
                         0 0 0 0 0 0 s₂ s₂ 1 s₁ s₁ 1 1 s₁ s₁ 1 s₂ s₂ s₂ s₂ 1 s₁ s₁ 1]

general_positional_vs = standard_vote_matrix * p_twentyfour

function positional_voting_method_4candidates(concrete_s1, concrete_s2)
    map(x -> sp.simplify(1//(1 + concrete_s1 + concrete_s2) * sp.subs(x, zip((s₁,s₂),
                                                                (concrete_s1,concrete_s2))...)) ,
        general_positional_vs)
end 

plurality_four_candidates =  positional_voting_method_4candidates(0,0)

antiplurality_four_candidates = positional_voting_method_4candidates(1,1)

vote_for_two_four_candidates =  positional_voting_method_4candidates(1,0)



plurality_four_candidates




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


plt.axis.yreversed = true






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