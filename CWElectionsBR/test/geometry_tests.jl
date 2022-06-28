import Pkg
Pkg.activate("../CWElectionsBR")

using CWElectionsBR
#using MeshViz
using GeometryBasics
using GLMakie
using Combinatorics
import Meshes


tern2cart(a, b, c) = (1 / 2 * (2b + c) / (a + b + c), √3 / 2 * (c / (a + b + c)))


plainpoints = [(0.,0.), (1.,0.), (0.5,sqrt(3)/2)]

midpoint(p1,p2) = ( (p1[1] + p2[1] )/2, (p1[2] + p2[2])/2)

external_triangle_centroid = Meshes.centroid(Meshes.Triangle(plainpoints...)) |> x-> x.coords |> Tuple |> GeometryBasics.Point2


externalpoints = GeometryBasics.Point2.(plainpoints)



midpoints = GeometryBasics.Point2.(map(x->(midpoint(x...)), combinations(plainpoints, 2) ))
#midpoints = GeometryBasics.Point2.([(0.5, 0.0), (0.25, 0.5 ), (0.75, 0.5)])

propercombinations = [[i,j,external_triangle_centroid] for j in externalpoints
                        for i in midpoints]

# Meshes.centroid(Meshes.Triangle((0.,0.), (1.,0.), (0.5,1.)))
mypoly = Polygon(externalpoints, propercombinations)

barepoints = map(x-> convert(Vector{Vector{Float64}}, x) , propercombinations )
barepoints = map(x-> x.coords,map(point -> Meshes.centroid(Meshes.Triangle(map(x-> Tuple(x),point))), barepoints))


foo = poly(mypoly,
        color = :white, strokecolor = :black, strokewidth = 1)

weirdcentroid(x,y)= ((y <= 0.4) && (y>=0.2)) && ((x <= 0.6) && (x>=0.4))
weirdcentroid(x) = weirdcentroid(x[1], x[2])

#scatter!(barepoints) 

scatter!(filter(!weirdcentroid,barepoints))  

hidespines!(foo.axis)
hidexdecorations!(foo.axis)
hideydecorations!(foo.axis)

text!("A", position = (-0.03,-0.01))
text!("B", position = (1.01,-0.01))
text!("C", position = (0.49,0.8655))








poly(Polygon(externalpoints),
        color = :red, strokecolor = :black, strokewidth = 1)