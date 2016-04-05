# ------------------------------------------------------------------------------
# README For Phenology Data from Rollinson & Kaye 2012
# ------------------------------------------------------------------------------

This data were collected in 2009 & 2010 as part of the Forest Regeneration 
and Climate Experiment (FORCE) in central PA.
PIs: Dr. Margot W. Kaye & Dr. Jason Kaye, 
     Pennsylvania State University, 
     University Park, PA 16802

Experimental Design:
 - Randomized block design with 4 treatments:
    1) Warming: +2˚C average daily
    2) Increased Precipitation: +20%, applied weekly
    3) Warming + Precipitation
    4) Control
 - Experimental warming began in winter 2008 and experimental concluded with 
   destructive harvest in 2010.
 - Percent cover by species and phenological state recorded weekly in spring and
   bi-weekly thereafter in 2009 & 2010

Citations: 
 - Phenology: Rollinson, C.R. & M.W. Kaye. 2012.  Experimental warming alters 
              spring phenology of certain plant functional groups in an early 
              successional forest community. Global Change Biology 18:1108-1116.

 - Met Data:  McDaniel, M.D.,  R.J. Wagner, C.R. Rollinson, B.A. Kimball, M.W. 
              Kaye, & J.P. Kaye. 2014. Microclimate and ecological threshold 
              responses in a warming and wetting experiment following whole tree 
              harvest. Theoretical and Applied Climatology 116: 287-299.
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# Data Provided by: 
# ------------------------------------------------------------------------------
Christine R. Rollinson, 
crollinson@gmail.com

Current Affiliation (March 2016): 
Postdoctoral Researcher
Boston University
Boston, MA 02215

Former Affiliation (2010-2014):
Pennsylvania State University 
University Park, PA 16802

For additional data, contact:
Dr. Margot W. Kaye
Pennsylvania State University
University Park, PA 26802
mwk12@psu.edu
# --------------------------------------------------------------------------

# --------------------------------------------------------------------------
# Directory Contents
# --------------------------------------------------------------------------
  1) FORCE_Inventories_2009_2010_raw.csv
  2) FORCE_Inventories_2009_2010_clean.csv
  3) Phenology_Raw_QAQC.R
  4) Species_List.csv
  5) FoRCE_CLIMATE_DATA_ALL_2008-2010_raw.xls
# --------------------------------------------------------------------------

# --------------------------------------------------------------------------
# File Descriptions
# --------------------------------------------------------------------------
  # ----------------------------------------------------------------
  # 1) FORCE_Inventories_2009_2010_raw.csv
  # 2) FORCE_Inventories_2009_2010_clean.csv
  # ----------------------------------------------------------------
    Description: Field inventory data from 2009 & 2010 that has been through 
                 QA/QC & last modified in 12 April, 2011.  These data were 
                 used in Rollinson & Kaye 2012.  These were extracted from C. 
                 Rollinson’s messy “2009-2010 FORCE Inventories.xls” file.

                 FORCE_Inventories_2009_2010_clean.csv was generated from 
                 FORCE_Inventories_2009_2010_raw.csv on 22 March 2016 using 
                 Phenology_Raw_QAQC.R

    Data Dimensions: 17147 rows x 15 columns

    Column Descriptions:
      1) Block - which block (set of 4 plots, 1 each treatment) the plot was 
                 located in
      2) Treatment - Experimental treatment, 5 levels:
          A = ‘Ambient’ temperature & precipitation (control) with replicated
              disturbance through installation of ‘dummy’ heating structures and
              adjacent planted seedlings experiment
          B = ‘Both’ (+2 C, + 20% precip)
          H = Heating only (+2 C)
          W = ‘Wetting’ only (+ 20% preciptiation)
          E = additional undisturbed controls with no ‘dummy’ heaters or adjacent
              planted seedling sub-plots.  No temperature or soil data were 
              collected in these plots.
      3) Heat - warming treatment applied (binary yes/no)
      4) Water - increased precipitation treatment applied (binary yes/no)
      5) Year - year of observation
      6) Survey.DOY - Julian day of year of observation (0-365)
      7) Plant.Form - very general plant form classification (3 levels)
          forb  = herbaceous non-grass plant
          grass = grasses
          woody = non-herbaceous
      8) Form.SubGroup — subdivision of plant form classification based on maximum 
         stature or growth form listed in Rhoads & Block 2007. (8 levels)
          grass      = grasses & sedges
          low forb   = non-grass herbaceous plants with maximum height < 1 meter
          tall forb  = non-grass herbaceous plants with maximum height >= 1 meter
          shrub      = multi-stemmed woody plants
          small tree = predominantly single-stemmed woody plants who are primarily 
                       found in the understory and/or heights less than 10 m
          large tree = predominantly single-stemmed woody plants who are often found
                       in the canopy of temperate forests
          vine       = twining and tendril-climbing woody or herbaceous plants
          unknown    = specimen could not be identified to high enough taxonomic 
                       resolution to be assigned to a group
          
      9) Native.Status — designation as native or non-native in Rhoads & Block 2007
         (3 levels)
          native     = native
          non-native = non-native 
          unknown    = native status unknown, typically because of inability to 
                       positively plant species
     10) Species — four-letter code for species based on identification throughout
         a single growing season (153 levels)
          - see Species_List for full more information
          - some individuals were never positively identified to species and remain
            unknown, but with unique taxa identifiers (e.g. ‘U59’, ‘tree 6’)
     11) Species.Original — the original species designation at time of survey. Many
         individuals were tagged as ‘unknowns’ in the spring and then positively 
         identified later in the season (see ‘notes’ column)
     12) Abundandance.Count - stem count of woody taxa
     13) Cover.Percent - estimated total percent plot coverage of taxa foliage
     14) Phenology.State - what phenological states are observed? note: there may be 
         multiple entries for a species/plot/date because multiple phenophases can be
         observed at once (i.e. leaves should almost always be present) (7 levels)
          0 = dormant
          1 = leaves expanded
          2 = flowering
          3 = seeds forming
          4 = seeds/fruits ripe
          5 = senesce
          6 = ????
     15) Notes - notes in from the field or in data entry; most common notes pertain 
         to correction of IDs or noting that some vegetation was trimmed too keep them
         from touching heaters and avoid starting a fire
  # ----------------------------------------------------------------

  # ----------------------------------------------------------------
  # 3) Phenology_Raw_QAQC.R
  # ----------------------------------------------------------------
    Description: R script used to do a little bit of data cleaning of 
                 FORCE_Inventories_2009_2010_raw.csv to create 
                 FORCE_Inventories_2009_2010_clean.csv
  # ----------------------------------------------------------------

  # ----------------------------------------------------------------
  # 4) Species_List.csv
  # ----------------------------------------------------------------
    Description: Master list of species found during the duration of the FORCE 
                 experiment by C. Rollinson. This file includes many “unknown” 
                 species that were flagged and identified later in the season.
                 All taxonomic and growth form information is from Rhoads & Block
                 2007.

    Data Dimensions: 274 rows x 20 columns

    Column Descriptions:
      1) ID Sheet - original listing on ID sheet for field surveys
      2) ID Sheet Code - code listed on ID sheet for field surveys
      3) Species CODE - four letter code from genus + specific epithet
      4) Genus - species genus
      5) Species - specific epithet
      6) Common Name - species common name
      7) Family - botanical family
      8) Life Form - very general plant form classification (3 levels)
          forb  = herbaceous non-grass plant
          grass = grasses
          woody = non-herbaceous
      9) Life Form 2 - subdivision of plant form classification based on maximum 
         stature or growth form listed in Rhoads & Block 2007. (8 levels)
          grass      = grasses & sedges
          low forb   = non-grass herbaceous plants with maximum height < 1 meter
          tall forb  = non-grass herbaceous plants with maximum height >= 1 meter
          shrub      = multi-stemmed woody plants
          small tree = predominantly single-stemmed woody plants who are primarily 
                       found in the understory and/or heights less than 10 m
          large tree = predominantly single-stemmed woody plants who are often found
                       in the canopy of temperate forests
          vine       = twining and tendril-climbing woody or herbaceous plants
     10) Mature Layer - where in the structure of a mature forest the plant is found
     11) Life History - annual/perrennial
     12) Flower Season - season of peak flowering
     13) Fruit Season - season of peak fruiting
     14) Shade Tolerance
     15) Native/Exotic 
     16) Native Status - native, naturalized, or designated invasive
     17) Wetland Status 
     18) Habitat Description
     19) Distribution
     20) Notes
  # ----------------------------------------------------------------

  # ----------------------------------------------------------------
  # 5) FoRCE_CLIMATE_DATA_ALL_2008-2010_raw.xls
  # ----------------------------------------------------------------
    Description: Raw hourly micrometereorology data collected using plot-level sensors.
                 Data was collated by M. McDaniel 2008-2011.

    Data Dimensions: 22585 rows x 37 columns

    Column Descriptions:
         1) Year (yyyy)
         2) Month (m)
         3) Day (dd)
         4) Hour (hh)
         5) Measurement Time (m/d/yy hh:mm)
         6) Windspeed (m/s)
         7) Meteorology base station air temperature (degrees C)
         8) Hourly precipitation (mm)
         9) Cumulative daily rain (mm)
        10) Irrigation — additional manual precipitation addition (“water” treatment)
     11-26) Plot surface temperature (degrees C) measured by infrared radiometers (IRR) 
     27-42) Plot soil temperature (degrees C) measured at a depth of 3 cm
     42-58) Plot soil moisture measured at a depth of 0-8 cm
     59-62) Mean treatment surface temperature (degrees C) 
  # ----------------------------------------------------------------

# --------------------------------------------------------------------------
