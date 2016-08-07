README: SpeciesMet
Owners: Radcliffe Phenology Working Group
Author: Christy Rollinson
Contact: crollinson@gmail.com
Date: 7 August, 2015

Data Generation Scripts: 
— https://github.com/AileneKane/radcliffe/blob/master/Analyses/teambackground/2_SpeciesRanges_MetExtractions.R
— https://github.com/AileneKane/radcliffe/blob/master/Analyses/teambackground/extract_met_range.R

Organization Description:
This directory contains daily mean, max, and minimum temperature plus total daily precipitation from 1949-2010 for 47 common tree species of the United States.  There is a single directory for each species that has been compressed as .tar.bz2.  Within each directory, there is a .png file that shows the raw Little’s range and the mask used to extract the met data.  Temperature and precipitation variables for each year are stored in a single netcdf file with dimensions (longitude, latitude, day of year).

The compressed files total 19 GB and are currently stored on the Cyverse (formerly iPlant) Discovery Environment (de.iplantcollaborative.org).  This is a free service, but to access the data you do need to register and send Christy Rollinson your username so she can give you read/write permissions to the folder.

Meteorology Description:
Spatial Extent: varies by species
Spatial Resolution: 0.125 - degree
Temporal Extent: 1949-2010
Temporal Resolution: daily
Variables: 
 1. tmean (mean daily temperature) 
 2. tmax (maximum daily temperature) 
 3. tmin (minimum daily temperature)
 4. precip (total daily precipitation)

Daily meteorology was calculated and extracted from 3-hourly GLDAS 2.0 data: http://disc.sci.gsfc.nasa.gov/datareleases/gldas-version-2.0-data-sets

GLDAS data was first extracted and aggregated to daily for North America using and then spatially-subsetted and stored locally using script: extract_gldas_region_day.R.  As of 1 August, 2016 NASA changed the permissions on these files and so while they are still publicly available, the existing extraction script no longer works.  I may fix this in the eventual future, but in the near term, we’ll be restricted to North America.