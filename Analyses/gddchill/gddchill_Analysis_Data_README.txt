godchill Files README

Suggested Format to Follow:
File Name
 - Description
 - Script to Generate
 - Column Descriptions
     1. Name - units - Description
     2. Name - units - Description
     ...

# ---------------------------------------

expclim.wchill.csv 
 - Description: this file contains all the microclimate data in “expclim.csv” (obtained from experiments) and adds chilling days, calculated using both air and soil temperature
 - Script to Generate: chilldays.R
  - Column Descriptions:
#  Name		units		Description  
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