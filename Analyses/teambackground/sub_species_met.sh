#!/bin/sh
#$ -wd /usr2/postdoc/crolli/radcliffe/Analyses/teambackground
#$ -j y 
#$ -S /bin/bash         
#$ -V 
#$ -m e
#$ -M crollinson@gmail.com
#$ -q "geo*"
#$ -l hostname=!scc-c*&!scc-t*
#$ -l h_rt=120:00:00
#$ -N TEST
#cd /usr2/postdoc/crolli/radcliffe/Analyses/teambackground
R CMD BATCH SpeciesRanges_MetExtractions_TEST.R