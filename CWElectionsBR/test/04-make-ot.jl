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


function getpl_apl_v42(p)

  pl =  cw.get_numericOf_qₛ(cw.plurality_4c_qₛ(), p)  |> Vector{Float64} |> v-> round.(v, digits = 4)

  apl = cw.get_numericOf_qₛ(cw.antiplurality_4c_qₛ(), p)  |> Vector{Float64} |> v-> round.(v, digits = 4)

  v42 = cw.get_numericOf_qₛ(cw.vote_for_two_4c_qₛ(), p)  |> Vector{Float64} |> v-> round.(v, digits = 4)

  #borda = cw.get_numericOf_qₛ(cw.borda_4c_qₛ(), p)  |> Vector{Float64} |> v-> round.(v, digits = 4)
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

function filled_tetrahedron(tpl,tapl,tv42,candidates = cw.candidates) 
    ot = cw.plain_opened_tetrahedron()
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
    
    poly!(ot.axis,cw.GeometryBasics.Polygon(cw.GeometryBasics.Point2.([possible_pulled_pl,
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


cw.filled_tetrahedron(cw.get_tpl_tapl_tv42(cw.getpl_apl_v42(p4c)...)...)

get_ot(df) = filled_tetrahedron(get_tpl_tapl_tv42(getpl_apl_v42(cw.getp_4candidates(df))...)...)

dfs_names = readdir(cw.dfspath)

imputted_poly = filter(x->occursin("poly_imp_min_raw",x), dfs_names)
imp_poly_dfs = map(x->cw.CSV.read(cw.dfspath * x, DataFrame), imputted_poly)


ots = map(get_ot, imp_poly_dfs)

ots[1]

for (index,plt) in enumerate(ots)
  save("../writing/images/opened_tetrahedron$index.png",plt)
end  


mincw1 = imp_poly_dfs[1]

p4c = cw.getp_4candidates(mincw1, "freq")

reptetra = representation_tetrahedron_freqs(p4c)

save("../writing/images/representation_tetrahedron.png", reptetra)