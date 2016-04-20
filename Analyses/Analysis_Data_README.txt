Analysis Files README

Suggested Format to Follow:
File Name
 - Description
 - Script to Generate
 - Column Descriptions
     1. Name - units - Description
     2. Name - units - Description
     ...

These data were compiled from multiple experimental studies that manipulated temperature (and in some cases precipitation as well) and monitored phenology. The data were generously shared by authors of the experiments and should only be used by members of the Radcliffe “Predicting Future Springs” Working Group for purposes outlined in the working group. Contact Ailene Ettinger or Lizzie Wolkovich if you would like to use the data for other purposes.
aettinger@fas.harvard.edu, wolkovich@fas.harvard.edu

# ---------------------------------------

expclim_gapfill.csv
 - Description: Gapfilled soil & air temperatures based off of expclim.csv
                codes for data type flags are as follows:
                1. Observed  = raw value from expclim.csv
                2. doy.mean  = interpolated pattern only (doy smoother)
                3. doy.resid = interpolated pattern + estimated day/treatment deviation
 - Script to Generate: Gapfill_ExpMet.R
 - Columns
    #  Name                units         Description
    1. year                year          year of met observations/fill
    2. doy                 julian day    day of year of observation; Jan 1 = 1
    3. temptreat           category      experimental temperature treatment
    4. preciptreat         category      experimental precipitation treatment
    5. plot                category      experiment plot number
    6. site                category      name of experiment/site
    7. fill.airtemp_min    degrees C     gap-filled daily min air temperature
    8. flag.airtemp_min    category      raw or gap fill type (see above)
    9. fill.airtemp_max    degrees C     gap-filled daily max air temperature
   10. flag.airtemp_max    category      raw or gap fill type (see above)
   11. fill.soiltemp1_min  degrees C     gap-filled daily min soil temperature
   12. flag.soiltemp1_min  category      raw or gap fill type (see above)
   11. fill.soiltemp1_max  degrees C     gap-filled daily max soil temperature
   12. flag.soiltemp1_max  category      raw or gap fill type (see above)

# ---------------------------------------

exppheno.csv 
 - Description: 
 - Script to Generate: radcliffe_merge_exp.R
 - Column Descriptions
     1. Name - units - Description
     2. Name - units - Description
     ...

# ---------------------------------------

expclim.csv 
 - Description: daily climate data measured* from experiments that manipulated temperature via active warming treatments  (and in some cases precipitation as well) and monitored phenology. 
 - Script to Generate:radcliffe_merge_expclim.R
 - Column Descriptions               
#  Name       		units     Description
1. temptreat  		levels	temperature treatment level (1-9;in `expsiteinfo.csv’)
2. preciptreat		levels	precip. treatment level (1-2;in `expsiteinfo.csv')
3. block		levels	grouping factor for plots, from individual study 
4. plot	   		NA	taken from individual study
5. year	   		year	year of climate measurements
6. doy 	   		doy	day of year of climate measurements
7. airtemp-min		deg C	min daily air temperature
8. airtemp-max		deg C	max daily air temperature
9. cantemp-min		deg C	min daily canopy temperature
10. cantemp-max		deg C	max daily canopy temperature
11. surftemp_min	deg C	min daily temperature measured at soil surface
12. surftemp_max	deg C	max daily temperature measured at soil surface
13. soiltemp1-min	deg C	min daily soil temperature, shallowest depth 
14. soiltemp2-min	deg C	min daily soil temperature, second shallowest depth	15. soiltemp1-max 	deg C	max daily soil temperature,  shallowest depth 	16. soiltemp2-max 	deg C	max daily soil temperature, second shallowest depth	17. soiltemp1-mean	deg C	mean daily soil temperature, shallowest depth
18. soilmois1		VWC	volumetric water content, proportion, shallowest depth
19. soilmois2		VWC	volumetric water content, proportion, shallowest depth

*in most cases climate data are measured, but 2 studies include modeled estimates as well: 
BACE: canopy temperature data (cantemp-min and cantemp-max) for temperature treatments 1 and 2 are estimated using relationships between measured soil temperature in all 4 temperature treatments (0,1,2,3) and measured canopy temperature in temperature treatments 0 and 3. Specifically, the equation used to estimate canopy temperature in the two unmeasured treatments (1 and 2) was:   
China:

# ---------------------------------------

obspheno.csv 
 - Description: 
 - Script to Generate: radcliffe_merge_obs.R
 - Column Descriptions
     1. Name - units - Description
     2. Name - units - Description
     ...
