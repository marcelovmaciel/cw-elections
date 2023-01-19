using Revise

import Pkg

Pkg.activate("./")


import CWElectionsBR as cw
using PrettyTables
using Suppressor



counterfactuals = [0.	0.319214	0.	0.565866;
0.680786	0.	0.467016	0.98424;
1.	0.532984	0.	0.801678;
0.434134	0.0157595	0.198322	0.] .|> x-> round(x, digits = 2)

counterfactuals = cw.DataFrame(counterfactuals, :auto)

cw.rename!(counterfactuals, cw.candidates)

counterfactuals[!,:candidates] = cw.candidates 

cw.select!(counterfactuals, [:candidates, Symbol.(cw.candidates)...])


table_counterfactuals = @capture_out  pretty_table(counterfactuals ,backend=Val(:latex))



