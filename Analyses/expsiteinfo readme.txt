README for expsiteinfo.csv
This file contains information on the experimental sites and methods for the experiments included in the MC3E database
Author of this file and manager of the database is Ailene Ettinger, ailene.ettinger@gmail.com

Below are column names and descriptions:

DatasetID: assigned name for study	
Location: research site, city, state, country of study	
FirstAuthor: First author of publication associated with experimental site	
Pubyear: Year publication associated with experimental site	
Publication_name: Journal name of publication associated with experimental site	
Volume: Volume of publication associated with experimental site	
startpage: First page of publication associated with experimental site	
warming_type: warming methodology; options are soil warming, forced air, infrared or soil warming plus forced air	
warming_type_detail: additional details about warming methodology	
no_spp: number of species for which phenology was tracked	
habitat: habitat in which the study was located	
Lat: latitude of the study site, in decimal degrees	
Long: longitude of the study site, in decimal degrees		
Elev_m: elevation above sea level of the study site, in meters	

data_startyear: first year of data included in the database	

data_endyear: final year of data included in the database		

exp_startyear: the year in which the experiment began	

obsfreq_days: frequency at which phenology data were collected, in days	

airtemp_freq_hours: frequency at which above-ground temperature data were collected, in hours	

soiltemp_freq_hr: frequency at which soil temperature data were collected, in hours	

soilmois_freq_hr: frequency at which soil moisture data were collected, in hours	

Tsoildepth1_cm: shallowest depth at which soil temperature was measured, in cm	

Tsoildepth2_cm: second shallowest depth at which soil temperature was measured, in cm	

Msoildepth1_cm: shallowest depth at which soil moisture was measured, in cm	

Msoildepth2_cm: second shallowest depth at which soil moisture was measured, in cm		

temptreat_1:temptreat_9: target amount of warming for warming treatments; if no target given, then reported warming is listed	

temptreat_units: units of the target and reported temperature (degrees C for all sites)

preciptreat_1: precipitation addition treatments	

preciptreat_-1:	precipitation reduction (drought) treatments

preciptreat_units: units for precipitation treatments	

temptreat_1_reported:temptreat_9_reported: measured warming achieved for warming treatments, as reported in publication

expdesign: type of experimental design (e.g., blocked, in which plots are organized into blocks such that all treatments exist within each block of plots; random, in which treatments were randomly applied to each plot not are not organized into blocks)	

analysis: method of analyzing experimental data. Options are categorical, in which explanatory climate variables are analyzed as categorical treatments (i.e. warmed versus unwarmed)	or continuous, in which measured explanatory microclimate is used

analysis_notes: additional details of how analysis was conducted	

warming_control: method of controlling warming. options are constant wattage, in which an unvarying energy output is used, or by feedback control, in which energy outputs are linked to a thermometer and varied depending on the measured temperature in plots, in order to maintain consistent warming levels.
	
heating_intensity_W: amount of energy applied, in Watts	

heater_ht_cm: height above ground at which infrared heaters were installed	

heaters_per_plot: number of infrared heaters per plot	

plot_area_m2: plot area, in square meters	

W_per_m2: heat applied divided by plot area	

target_mentioned: whether target warming was specified in the publication (choices are yes or no) 

control_type: type of control plots included in the study. choices are structural controls (i.e., `shams' or `disturbance controls,' which contained the warming infrastructure, but with no heat applied), ambient controls with no infrastructure added, or both.