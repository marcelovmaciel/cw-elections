import Pkg
Pkg.activate("../")

using CWElectionsBr
using MeshViz
using GeometryBasics
using CairoMakie
using Combinatorics
import Meshes

CairoMakie.activate!(type = "png")


tern2cart(a, b, c) = (1 / 2 * (2b + c) / (a + b + c), √3 / 2 * (c / (a + b + c)))
midpoint(p1,p2) = ( (p1[1] + p2[1] )/2, (p1[2] + p2[2])/2)

function turn_into_euclideanpoint(point)
    tern2cart(point...) |> GeometryBasics.Point2
end

function make_basic_3_candidate△()


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
    mypoly = Polygon(externalpoints, propercombinations)

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

    text!("A", position = (-0.03,-0.05))
    text!("B", position = (1.01,-0.05))
    text!("C", position = (0.49,0.867))

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
                         borda_share)
    basic△ = make_basic_3_candidate△()
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

plurality_result = plurality_3a(p)
antiplurality_result = antiplurality_3a(p)
borda_result = borda_3a(p)

[turn_into_euclideanpoint(fn(p)) for fn in [plurality_3a,
        antiplurality_3a,
        borda_3a]]

function representation△(voter_profile)
    positional_results = map(fn->fn(voter_profile),
       [plurality_3a,
        antiplurality_3a,
        borda_3a])

    representation△(positional_results...)
end


#  Nurmi Uncertainty book page 122 example
plurality_share = [0.444, 0.256, 0.291]
antiplurality_share = [0.314, 0.335, 0.351]
borda_share = [0.357, 0.311, 0.331]

bar = representation△(plurality_share,
                         antiplurality_share,
                         borda_share)

save("foo.png", bar)

# Tabarrok 2001 Figure 1

p = [0, .419,0., .258,.322, 0. ]

baz = representation△(p)

save("baz.png", baz)
