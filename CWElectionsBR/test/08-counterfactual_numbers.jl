using Revise

import Pkg

Pkg.activate("./")


import CWElectionsBR as cw
using PrettyTables
using Suppressor


## This comes from the wolfram notebook
counterfactuals = [
    0.  0.310558  0.  0.575539;
    0.689442  0.  0.470875  0.998342;
    1.  0.529125  0.  0.806683;
    0.424461  0.00165774  0.193317  0.
   ] .|> x-> round(x, digits = 2)


counterfactuals = cw.DataFrame(counterfactuals, :auto)

cw.rename!(counterfactuals, cw.candidates)

counterfactuals[!,:candidates] = cw.candidates 

counterfactuals

cw.select!(counterfactuals, [:candidates, Symbol.(cw.candidates)...])


table_counterfactuals = @capture_out  pretty_table(counterfactuals ,backend=Val(:latex))

open("../writing/images/table_counterfactuals.tex", "w") do file 
    write(file, table_counterfactuals)
end        
    
    