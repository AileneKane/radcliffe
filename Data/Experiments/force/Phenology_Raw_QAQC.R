# -----------------------------------
# QA/QC of phenology data from Rollinson & Kaye 2012 for Harvard Radcliff Phenology Seminar 3-6 April 2016
# Script Generated: 22 March 2016
# Contact: Christy Rollinson, crollinson@gmail.com
# Experiment PI: Margot W. Kaye, mwk12@psu.edu
#
# Original Data Source:
# sheet 1 ('2009-2010 Raw Cleaned') from '~/Desktop/Personal/Penn State/Research/FORCE (MS)/FORCE Data/2010 FORCE Natural/2009-2010 FORCE Inventories.xls' 
# -----------------------------------
setwd("~/Desktop/Research/PalEON_CR/Conferences etc/Radcliff Seminar 2016/")
# -------------------------
# Read in raw file
# -------------------------
file.raw <- read.csv("FORCE_Inventories_2009_2010_raw.csv", na.strings=c("", "*"))
dim(file.raw)
# -------------------------


# -------------------------
# Assigning more descriptive & R-friendly column names
# -------------------------
names(file.raw) <- c("Block", "Treatment", "Heat", "Water", "Year", "Survey.DOY", "Plant.Form", "Form.SubGroup", "Native.Status", "Species", "Species.Original", "Abundance.Count", "Cover.Percent", "Phenology.State", "Notes")
names(file.raw)
summary(file.raw)
# -------------------------

# -------------------------
# Convert Block to a factor (levels should be 1-4)
# -------------------------
file.raw$Block <- as.factor(file.raw$Block)
summary(file.raw$Block)
# -------------------------

# -------------------------
# Correcting '0' level in 'Form.2' and 'Native' to 'unknown'
# -------------------------
# These observations were never identified to species
summary(file.raw$Form.SubGroup)
summary(file.raw[file.raw$Form.SubGroup ==0,])
file.raw[file.raw$Form.SubGroup==0,c("Form.SubGroup")] <- c("unknown")

summary(file.raw[file.raw$Native.Status==0,])
summary(file.raw[file.raw$Native.Status==0,"Species"])
file.raw[file.raw$Native==0,c("Native.Status")] <- c("unknown")
# -------------------------

# -------------------------
# Checking on Species
# -------------------------
summary(file.raw$Species)
unique(file.raw$Species)
summary(file.raw[file.raw$Species=="(Other)",])
# -------------------------

# -------------------------
# Looking at the Phenology State designations
# Phenology should be categorical
# 0 = dormant
# 1 = leafed out
# 2 = flowering
# 3 = seeds forming?
# 4 = ripe seeds/fruit
# 5 = senescing 
# 6 = ???? It's in several herbaceous observations, but I can't find any
#     of my notes on what it means
# -------------------------
file.raw$Phenology.State <- as.factor(file.raw$Phenology.State)

# pretty sure 0.1 supposed to be 0 & got messed up in a manual find-replace
file.raw[!is.na(file.raw$Phenology.State) & file.raw$Phenology.State==0.1,"Phenology.State"] <- 0

# 12 is supposed to be separate rows for 1 & 2 
#  - Make duplicate records for these with separate phenophases
#  - add them to the original data frame and delete the 12s
rows.12.1 <- file.raw[!is.na(file.raw$Phenology.State) & file.raw$Phenology.State==12,]
rows.12.2 <- file.raw[!is.na(file.raw$Phenology.State) & file.raw$Phenology.State==12,]
rows.12.1$Phenology.State <- 1
rows.12.2$Phenology.State <- 2

# Adding in the correct records & getting rid of '12' 
file.raw <- rbind(file.raw[!file.raw$Phenology.State==12,], rows.12.1, rows.12.2)
dim(file.raw)
dim(file.raw2)

# There's also a 7 that must be a 1 (misread handwriting?)
file.raw[!is.na(file.raw$Phenology.State) & file.raw$Phenology.State==7,"Phenology.State"] <- 1

summary(file.raw$Phenology.State)
# -------------------------

# -------------------------
# Save cleaned data as a new file
# -------------------------
summary(file.raw)

write.csv(file.raw, "FORCE_Inventories_2009_2010_clean.csv")
# -------------------------
