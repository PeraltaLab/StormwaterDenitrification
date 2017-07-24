python ../bin/name_change.py TC.bac.final.0.03.rep.fasta TC.bac.final.0.03.rep.rename.fasta

FastTree -gtr -nt -gamma -fastest TC.bac.final.0.03.rep.fasta > TC.bac.tree

FastTree -gtr -nt -gamma -fastest TC.bac.final.0.03.rep.rename.fasta > TC.bac.rename.tree
