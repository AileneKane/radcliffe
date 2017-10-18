gddchill Files README

Suggested Format to Follow:
File Name
 - Description
 - Script to Generate
 - Column Descriptions
     1. Name - units - Description
     2. Name - units - Description
     ...

# ---------------------------------------

expclim.wchillgdd.csv 
 - Description: this file contains all the microclimate data in “expclim.csv” (obtained from experiments) and adds chilling days, calculated using both air and soil temperature, and cumulative growing degree days, calculated using both air and soil temperature. 
 - Script to Generate: chilldays_and_gdd.R
  - Column Descriptions:
#  Name		units		Description  
- Description: daily climate data measured* from experiments that manipulated temperature via active warming treatments  (and in some cases precipitation as well) and monitored phenology. 
 - Script to Generate:radcliffe_merge_expclim.R
 - Column Descriptions               
#  Name       		units  Description
1. site			name	study/site name
2. temptreat  		levels	temperature treatment level (1-9;in `expsiteinfo.csv’)
3. preciptreat		levels	precip. treatment level (1-2;in `expsiteinfo.csv')
4. block		name	grouping factor for plots, from individual study 
5. plot	   		name	taken from individual study
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
19. soiltemp2-mean	deg C	mean daily soil temperature, second shallowest depth
20. soilmois1		VWC	volumetric water content, proportion, shallowest depth
21. soilmois2		VWC	volumetric water content, second shallowest depth
22. sept01doy  		doy	day of year for 1 sept (either 244 or 245)   
23. dec31doy 		doy	day of year for 1 sept (either 365 or 366)      
24. chyr    		year	growing season year for which chilling day counts to
25. chdoy		doy	doy after sept 1, when chilling days start accumulating
26. chday_soil		0/1	1=counts as a chilling day (<5 deg C for meansoiltemp1)
27. airtemp_mean	deg C	(air Tmin + air Tmax)/2
28. chday_air		0/1	1=counts as a chilling day (<5 deg C for meansoiltemp1)
29. cantemp_mean	deg C	(canopy Tmin + canopy Tmax)/2
20. cumchill_soil	#days	# of chilling days, based on mean soil temp1
31. numnas_soil		#	# of days for which there are no soiltemp/chillday data
32. cumchill_air	#days	# of chilling days, based on mean air or canopy temp
33. numnas_air		#	# of days for which there are no airtemp/chillday data
34. chdoy_min		chdoy	first day for which temp data exist, days since sept 1, 
35. gdd_soil		deg C	mean daily soil temp, above Tbase (5 deg C) 
36. gdd_air		deg C	mean daily air.canopy temp, above Tbase (5 deg C)
37. cumgdd_soil		deg C	accumulated gdd_soil, since 1 jan of that year
38. numnas_soilgdd	#	# of days for which there are no soiltemp/gdd data
39. cumgdd_air    	deg C	accumulated gdd_air, since 1 jan of that year
40. numnas_airgdd	#	# of days for which there are no airtemp/gdd data
41. mindoy		doy	first doy for which temp data exist

*in most cases climate data are measured, but 2 studies include modeled estimates as well: 
BACE: canopy temperature data (cantemp-min and cantemp-max) for temperature treatments 1 and 2 are estimated using relationships between measured soil temperature in all 4 temperature treatments (0,1,2,3) and measured canopy temperature in temperature treatments 0 and 3. Specifically, the equation used to estimate canopy temperature in the two unmeasured treatments (1 and 2) was:   