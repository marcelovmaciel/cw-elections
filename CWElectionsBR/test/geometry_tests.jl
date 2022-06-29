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

#scatter!(filter(!weirdcentroid,barepoints))  

hidespines!(foo.axis)
hidexdecorations!(foo.axis)
hideydecorations!(foo.axis)

text!("A", position = (-0.03,-0.05))
text!("B", position = (1.01,-0.05))
text!("C", position = (0.49,0.867))

# Nurmi Uncertainty book page 122 example

tern2cart(0.444, 0.256, 0.291) |> GeometryBasics.Point2|> x-> scatter!([x], color = :red)
tern2cart(0.314, 0.335, 0.351) |> GeometryBasics.Point2 |> x-> scatter!([x], color = :red)

lines!([tern2cart(0.444, 0.256, 0.291) |> GeometryBasics.Point2,
tern2cart(0.314, 0.335, 0.351) |> GeometryBasics.Point2 ])
tern2cart(0.357, 0.311, 0.331) |> GeometryBasics.Point2 |> x-> scatter!([x], color = :green)

function positional_method_3a(s,p)
        [p[1] + p[2] + (-p[1]-p[2]+p[3]+p[6])*s,
         p[6] + p[5] + (p[4] - p[5] + p[1] - p[6])*s,
         p[3] + p[4] + (p[2]-p[3]-p[4] + p[5])*s ]
end        


plurality_3a(p) = three_positional_method(0,p)
borda_3a(p) = three_positional_method(1/3,p)
antiplurality_3a(p) = three_positional_method(1/2,p)

