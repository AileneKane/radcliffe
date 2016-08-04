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
                2. doy.adj   = interpolated pattern only (doy smoother), adjusted by plot/treatment
                3. doy.resid = interpolated pattern + estimated day/treatment deviation
                4. forced_min= a forced temperature to deal with troubled winter values at the dunne site

 - Script to Generate: Gapfill_ExpMet.R
 - Columns
    #  Name                units         Description
    1. site                category      name of experiment/site
    2. year                year          year of met observations/fill
    3. doy                 julian day    day of year of observation; Jan 1 = 1
    4. temptreat           category      experimental temperature treatment
    5. preciptreat         category      experimental precipitation treatment
    6. plot                category      experiment plot number
    7. fill.AGtemp_min     degrees C     gap-filled daily min aboveground
    8. flag.AGtemp_min     category      raw or gap fill type (see above)
    9. fill.AGtemp_max     degrees C     gap-filled daily max air temperature
   10. flag.AGtemp_max     category      raw or gap fill type (see above)
   11. fill.soiltemp1_min  degrees C     gap-filled daily min soil temperature
   12. flag.soiltemp1_min  category      raw or gap fill type (see above)
   11. fill.soiltemp1_max  degrees C     gap-filled daily max soil temperature
   12. flag.soiltemp1_max  category      raw or gap fill type (see above)
   13. AG.type             category      type of aboveground temperature data provided

# ---------------------------------------

exppheno.csv 
 - Description: 
 - Script to Generate: radcliffe_merge_exp.R
 - Column Descriptions:
#  Name		units		Description  
1. site				study/site name
2. plot	   			taken from individual study
3. event	category	phenological event observed
4. year	   	year		year of phenological event
5. genus			genus of plant observed	
6. species			species of plant observed
7. doy 	   	doy		day of year of phenological event

# ---------------------------------------

expclim.csv 
 - Description: daily climate data measured* from experiments that manipulated temperature via active warming treatments  (and in some cases precipitation as well) and monitored phenology. 
 - Script to Generate:radcliffe_merge_expclim.R
 - Column Descriptions               
#  Name       		units  Description
1. site				study/site name
2. temptreat  		levels	temperature treatment level (1-9;in `expsiteinfo.csv’)
3. preciptreat		levels	precip. treatment level (1-2;in `expsiteinfo.csv')
4. block		levels	grouping factor for plots, from individual study 
5. plot	   		NA	taken from individual study
6. year	   		year	year of climate measurements
7. doy 	   		doy	day of year of climate measurements
8. airtemp-min		deg C	min daily air temperature
9. airtemp-max		deg C	max daily air temperature
10. cantemp-min		deg C	min daily canopy temperature
11. cantemp-max		deg C	max daily canopy temperature
12. surftemp_min	deg C	min daily temperature measured at soil surface
13. surftemp_max	deg C	max daily temperature measured at soil surface
14. soiltemp1-min	deg C	min daily soil temperature, shallowest depth 
15. soiltemp2-min	deg C	min daily soil temperature, second shallowest depth	16. soiltemp1-max 	deg C	max daily soil temperature,  shallowest depth 	17. soiltemp2-max 	deg C	max daily soil temperature, second shallowest depth	18. soiltemp1-mean	deg C	mean daily soil temperature, shallowest depth
19. soilmois1		VWC	volumetric water content, proportion, shallowest depth
20. soilmois2		VWC	volumetric water content, proportion, shallowest depth

*in most cases climate data are measured, but 2 studies include modeled estimates as well: 
BACE: canopy temperature data (cantemp-min and cantemp-max) for temperature treatments 1 and 2 are estimated using relationships between measured soil temperature in all 4 temperature treatments (0,1,2,3) and measured canopy temperature in temperature treatments 0 and 3. Specifically, the equation used to estimate canopy temperature in the two unmeasured treatments (1 and 2) was:   
China:

# ---------------------------------------

expsiteinfo.csv

- Description: daily climate data measured* from experiments that manipulated temperature via active warming treatments  (and in some cases precipitation as well) and monitored phenology. 
 - Script to Generate:radcliffe_merge_expclim.R
 - Column Descriptions               
#  Name       		units  Description
1. Site				study/site name
2. Location  		
3. FirstAuthor		
4. Pubyear		
5. Publication_name   
6. Volume	   	
7. startpage 	   	
8. warming_type		
9. warming_type_detail		
10. no_spp		#	number of species whose phenology were tracked
11. habitat			
12. Lat			decdeg decimal degrees of latitude for experiment location
13. Long		decdeg decimal degrees of longitude for experiment location
14. Elev_m		m	elevation of experiment location 
15. data_startyear	year	first year of phenology data collection
16. data_endyear	year	last year of phenology data collection 	
17. exp_startyear	year	first year of experiment
18. obsfreq_days	days	frequency of phenology data collection
19. airtemp_freq_hours	hours	frequency of air temperature data collection
20. soiltemp_freq_hr	hours	frequency of soil temperature data collection
21. soilmois_freq_hr	hours	frequency of soil moisture data collection
22. soiltemp_freq_hr	hours	frequency of air temperature data collection
23. Tsoildepth1_cm	cm	shallowest depth at which soil temp was measured
24. Tsoildepth2_cm	cm	second shallowest depth at which soil temp was measured
25. Msoildepth1_cm	cm	shallowest depth at which soil moisture was measured
26. Msoildepth1_cm	cm	second shallowest depth at which soil moisture was measured
27-36. temptreat_1:
	temptreat_9		target warming treatment levels
37. temptreat_units		units for temp treatment (usually degC)
38. preciptreat_1		precip added treatment amount
39. preciptreat_-1		precip removed treatment amount
40. preciptreat_units		units for precipitation treatment
41-49. temptreat_1_reported:	
temptreat_9_reported		reported warming average treatment levels (if crossed treatments, these are given for the warming only treatment)

# ---------------------------------------

obspheno.csv 
 - Description: 
 - Script to Generate: radcliffe_merge_obs.R
  - Column Descriptions:
#  Name		units		Description  
1. site				study/site name
2. plot	   			taken from individual study, not used in most cases
3. event	category	phenological event observed
4. year	   	year		year of phenological event
5. doy 	   	doy		day of year of phenological event
6. date 	mm/dd/yy	date of phenological event
7. genus			genus of plant observed	
8. species			species of plant observed

# ---------------------------------------

EffectiveWarming_Plot.csv
 - Description: Treatment effects on above- and below-ground temperature calculated as the plot-deviation from the mean control
 - Script to Generate: EffectiveWarming_Simple.R
 - Columns
    #  Name                units         Description
    1. site                category      name of experiment/site
    4. temptreat           category      experimental temperature treatment
    5. preciptreat         category      experimental precipitation treatment
    6. plot                category      experiment plot number
    7. block               category      experiment block number
    8. AG.type             category      type of aboveground temperature data provided
    9. AGtemp_max_dev      deg C         mean temperature effect on aboveground daily maximum temperature 
   10. AGtemp_min_dev      deg C         mean temperature effect on aboveground minimum temperature
   11. AGtemp_mean_dev     deg C         mean temperature effect on aboveground mean temperature
   12. BGtemp_max_dev      deg C         mean temperature effect on belowground (1st layer) daily maximum temperature
   13. BGtemp_min_dev      deg C         mean temperature effect on belowground (1st layer) daily minimum temperature
   14. BGtemp_mean_dev     deg C         mean temperature effect on belowground (1st layer) daily mean temperature

# ---------------------------------------

EffectiveWarming_Treatment.csv
 - Description: Treatment effects on above- and below-ground temperature calculated as the treatment-level deviation from the mean control
                NOTE: this was calculated directly from the raw daily data.  if you would like to go through the plot level first to remove
                      potential bias of plot representation, use the aggregate function on EffectiveWarming_Plot.csv
 - Script to Generate: EffectiveWarming_Simple.R
 - Columns
    #  Name                units         Description
    1. site                category      name of experiment/site
    2. temptreat           category      experimental temperature treatment
    3. preciptreat         category      experimental precipitation treatment
    4. AG.type             category      type of aboveground temperature data provided
    5. AGtemp_max_dev      deg C         mean temperature effect on aboveground daily maximum temperature 
    6. AGtemp_min_dev      deg C         mean temperature effect on aboveground minimum temperature
    7. AGtemp_mean_dev     deg C         mean temperature effect on aboveground mean temperature
    8. BGtemp_max_dev      deg C         mean temperature effect on belowground (1st layer) daily maximum temperature
    9. BGtemp_min_dev      deg C         mean temperature effect on belowground (1st layer) daily minimum temperature
   10. BGtemp_mean_dev     deg C         mean temperature effect on belowground (1st layer) daily mean temperature

# ---------------------------------------
