import Pkg
Pkg.activate("../")

using CWElectionsBR
#using MeshViz
using GeometryBasics
using CairoMakie
using Combinatorics
import Meshes

#= 
Graphics[{RGBColor[1, 1, 0.85], EdgeForm[GrayLevel[0]],
  GraphicsComplex[{{0, 0}, {1/2, Sqrt[3]/2}, {1, 0},
    {1, Sqrt[3]}, {3/2, Sqrt[3]/2}, {2, 0}},
   Polygon[{{1, 3, 2}, {4, 2, 5}, {3, 5, 2}, {5, 3, 6}}]]}]
 =#

test_points = [(0, 0), (1/2, sqrt(3)/2), (1, 0),
    (1, sqrt(3)), (3/2, sqrt(3)/2), (2, 0)]


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
plt.axis.yreversed = true


save("tetra_test.png",plt)

# Testing saari triangle -------------------------------------------------------------------------

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
