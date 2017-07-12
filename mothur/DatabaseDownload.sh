#!/bin/bash

wget https://mothur.org/w/images/b/b4/Silva.nr_v128.tgz
tar -xzvf Silva.nr_v128.tgz

wget https://mothur.org/w/images/f/f1/Silva.gold.bacteria.zip
unzip -j Silva.gold.bacteria.zip

wget https://mothur.org/w/images/c/c3/Trainset16_022016.pds.tgz
tar -xzvf  Trainset16_022016.pds.tgz

module load gcc/4.9.2
module load boost/1.52.0
module load mothur/1.39.0

mothur "#pcr.seqs(fasta=silva.nr_v128.align, start=12000, end=25000, keepdots=F, processors=1)"
mv silva.nr_v128.pcr.align silva.v4.fasta
