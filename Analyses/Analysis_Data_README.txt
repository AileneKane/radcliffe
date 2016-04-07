Analysis Files README

Suggested Format to Follow:
File Name
 - Description
 - Script to Generate
 - Column Descriptions
     1. Name - units - Description
     2. Name - units - Description
     ...

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
