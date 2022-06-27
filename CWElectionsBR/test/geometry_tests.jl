import Pkg
Pkg.activate("../CWElectionsBR")

using CWElectionsBR
#using MeshViz
using GeometryBasics
using GLMakie
using Combinatorics
import Meshes

externalpoints = GeometryBasics.Point2.([(0.,0.), (1.,0.), (0.5,1.)])
midpoints = GeometryBasics.Point2.([(0.5, 0.0), (0.25, 0.5 ), (0.75, 0.5)])

propercombinations = [[i,j,GeometryBasics.Point2([0.5, 0.33])] for j in externalpoints
                        for i in midpoints]

# Meshes.centroid(Meshes.Triangle((0.,0.), (1.,0.), (0.5,1.)))
mypoly = Polygon(externalpoints, propercombinations)

barepoints = map(x-> convert(Vector{Vector{Float64}}, x) , propercombinations )
barepoints = map(x-> x.coords,map(point -> Meshes.centroid(Meshes.Triangle(map(x-> Tuple(x),point))), barepoints))


foo = poly(mypoly,
        color = :red, strokecolor = :black, strokewidth = 1)

weirdcentroid(x,y)= ((y <= 0.45) && (y>=0.25)) && ((x <= 0.6) && (x>=0.4))
weirdcentroid(x) = weirdcentroid(x[1], x[2])

scatter!(filter(!weirdcentroid,barepoints))  

hidespines!(foo.axis)
hidexdecorations!(foo.axis)
hideydecorations!(foo.axis)

text!("A", position = (-0.03,-0.01))
text!("B", position = (1.01,-0.01))
text!("C", position = (0.49,1))




