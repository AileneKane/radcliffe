#!bin/bash
# This spawns a separate extraction section for each species
#  Doing this is a parallel R session didn't work and this should help
#  Just get things in the cue & they should stay there until they're done
#  -- if the extraction takes more than 5 days, we can re-run this script
#     and things should be able to pick up where they left off
# Christy Rollinson, crollinson@gmail.com

# Get the list of species
pushd "input/little_ranges/"
	species_list=(*)
popd

# Find out how many species there are
n=${#species_list[@]}

# for FILE in $(seq 0 (($n-1)))
for ((FILE=0; FILE<$n; FILE++)) # This is a way of doing it so that we don't have to modify N
do
	# Site Name and Lat/Lon
	SPECIES=${species_list[FILE]}
	echo $SPECIES

	cp SpeciesRanges_MetExtractions.R SpeciesRanges_MetExtractions_${SPECIES}.R
	cp sub_species_met.sh sub_species_met_${SPECIES}.sh
	
	sed -i "s,TEST,${SPECIES},g" SpeciesRanges_MetExtractions_${SPECIES}.R
	sed -i "s,TEST,${SPECIES},g" sub_species_met_${SPECIES}.sh

 	qsub sub_species_met_${SPECIES}.sh
done

# git stash # stash the pulled file so we don't get confilcts

