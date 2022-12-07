

using CWElectionsBR
import Base.Filesystem as fl 
using Pipe 
using FloatingTableView
using PrettyTables

dfspath= "../rscripts/dfs/"

foo = CSV.read(dfspath * "freq_ranks_inferred.csv",DataFrame)

foo

mincw1 = CSV.read(dfspath * "min_c1_raw.csv",DataFrame)
mincw2 = CSV.read(dfspath * "min_c2_raw.csv",DataFrame)

browse(mincw1)

drop_candidate(acc, candidate) = map(a->filter(x->x≠ candidate,a), acc)


@pipe mincw1 |> fix_raw_into_vecs |> make_pretty_ranking_vector |> pretty_table(_, backend = :latex)

@pipe mincw2 |> fix_raw_into_vecs |> make_pretty_ranking_vector |> pretty_table(_, backend = :latex)




noalckmin_df = finaldf_without_candidate("alckmin", foo)
nohaddad_df = finaldf_without_candidate("haddad", foo)
nociro_df = finaldf_without_candidate("ciro", foo)
nobolsonaro_df = finaldf_without_candidate("bolsonaro", foo) 


nobolsonaro_df

browse(noalckmin_df) 

browse(nohaddad_df) 

browse(nociro_df) 

browse(nobolsonaro_df) 

CSV.write(dfspath * "noalckmin_df.csv", noalckmin_df)
CSV.write(dfspath * "nohaddad_df.csv", nohaddad_df)
CSV.write(dfspath * "nociro_df.csv", nociro_df)
CSV.write(dfspath * "nobolsonaro_df.csv", nobolsonaro_df)





reval("load('../../rscripts/dta_objects/corrected_freq_ranks.RData')")

@rget corrected_freq_ranks

print(corrected_freq_ranks)

noalckmin_df = finaldf_without_candidate("alckmin", corrected_freq_ranks)

freq_without_candidate("alckmin", corrected_freq_ranks)


nohaddad_df = finaldf_without_candidate("haddad", corrected_freq_ranks)
nociro_df = finaldf_without_candidate("ciro", corrected_freq_ranks)
nobolsonaro_df = finaldf_without_candidate("bolsonaro", corrected_freq_ranks)
