### AE Melton, 2021
# Script for downloading occurrence data for plants lacking use per Kew plant list and
# identifying which plants occur in particular environments (i.e., hyper-arid)

### Distribution stuff for USELESS plants
library(spocc)
library(dplyr)
library(readr)

setwd("Occurrence_Data/")

source("GetOccurrenceData.R")
source("GetGBIFKeys.R")
source("GetGBIFOccurrences.R")

GetOccurrenceData(usr.query = Useless$Species, usr.db = c("gbif", "idigbio"), usr.how = "shortest")
# The previous command generates a "raw" data csv and a curated data csv. Use the raw to
# generate a gbif DOI and the curated for further analysis.
GetGBIFKeys() # Get the Keys to the all downloaded GBIF data. Use these to generate a GBIF derived dataset DOI 
GetGBIFOccurrences() # Get just the data from GBIF so it can be set aside and cited as a derived dataset


QC.files <- list.files(pattern = "PrelimQCd_Data.csv", full.names = TRUE)  %>% 
  lapply(read_csv) %>% 
  bind_rows 

QC.files

write.csv(x = QC.files, file = "USELESS_PLANTS_PrelimQCd_Data.csv", row.names = F)

###

# Further QC the curated data csv
occ.dat <- read.csv(file = "USELESS_PLANTS_PrelimQCd_Data.csv")
rastey <- raster("30s_Env_Data/AI_and_PET/ai_et0/ai_et0.tif") # Identify coordinated lacking environmental data
occ.dat.Extract <- raster::extract(rastey, occ.dat[,2:3])
occ.dat.Extract <- cbind(occ.dat, occ.dat.Extract)
clean_occ.dat.Extract <- occ.dat.Extract[complete.cases(occ.dat.Extract$occ.dat.Extract),]
write.csv(x = clean_occ.dat.Extract[,1:3], file = "USELESS_PLANTS_PrelimQCd_Plus_No_NA_Env_Data.csv", row.names = F)

pdf("USELESS_PLANTS_data_distribution.pdf") # Generate a PDF file with a map showing distribution of points
plot(clean_occ.dat.Extract[,2:3])
dev.off()

#
occs <- read.csv("USELESS_PLANTS_PrelimQCd_Plus_No_NA_Env_Data.csv")
rastey <- raster("30s_Env_Data/AI_and_PET/ai_et0/ai_et0.tif")
rastey[is.na(rastey)] <- 0
rastey <- rastey*0.0001 # Rescale; information regarding the scales of AI and PET files are available in the readmes that accompany the data downloads
cellStats(x = rastey, stat = "range")
rastey.at.occs <- extract(x = rastey, y = occs[,2:3])
range(!is.na(rastey.at.occs))

df <- as.data.frame(as.numeric(rastey.at.occs))

hist(x = df$`as.numeric(rastey.at.occs)`, breaks = 1000) # Generate histogram of AI values at occurrence points
df[order(df$`as.numeric(rastey.at.occs)`),]

nrow(filter(df, df$`as.numeric(rastey.at.occs)` < 0.05)) # HYPER ARID; these are the points we want
# nrow(filter(df, df$`as.numeric(rastey.at.occs)` >= 0.05 & df$`as.numeric(rastey.at.occs)` < 0.2)) # ARID
# nrow(filter(df, df$`as.numeric(rastey.at.occs)` <= 0.2 & df$`as.numeric(rastey.at.occs)` < 0.5)) # SEMI ARID
# nrow(filter(df, df$`as.numeric(rastey.at.occs)` <=0.5 & df$`as.numeric(rastey.at.occs)` < 0.65)) # DRY SUBHUMID
# nrow(filter(df, df$`as.numeric(rastey.at.occs)` > 0.65)) # HUMID

df.w.occs <- data.frame(occs, df)
HyperArid <- filter(df.w.occs, df.w.occs$as.numeric.rastey.at.occs. < 0.05)
write.csv(x = HyperArid, file = "HyperArid_Occurrences.csv", row.names = F)

plot(rastey)
points(occs[,2:3])
#

#
rastey <- raster("AI_and_PET/ai_et0/ai_et0.tif")
occs <- read.csv("USELESS_PLANTS_PrelimQCd_Plus_No_NA_Env_Data.csv")
species.list <- unique(occs$name)
length(species.list) # Generate a list of unique species names for the occurrences in hyper arid areas

# Get count data for the number of occurrences for each species in hyper arid environments
setwd("Drought_Adaptation_G2PMineR/")
df.counts <- data.frame(Species = character(107), Counts = numeric(107)) # Specify the number of species. In this case, 107
for(i in 1:length(species.list)){
  tmp.species <- species.list[i]
  tmp.occs <- occs[occs$name %in% tmp.species,]
  
  filename <- paste(tmp.species, "Occurrence_Points.pdf", sep = "_")
  setwd("Species_Plots/")
  pdf(filename)
  plot(rastey)
  points(tmp.occs[,2:3], pch = 19, col = "red")
  dev.off()
  
  setwd("../Occurrence_Data/")
  df.counts$Species[i] <- tmp.species
  df.counts$Counts[i] <- nrow(tmp.occs)
  setwd("../")
}

df.counts
write.csv(x = df.counts, file = "Species_Occurrence_Counts.csv", row.names = F)
#

# Re-order the data from most occurrences to fewest
df.counts
df.counts[order(-df.counts$Counts),]
#

#
setwd("Occurrence_Data/")
HyperAridOccs <- read.csv("HyperArid_Occurrences.csv")
head(HyperAridOccs)
length(unique(HyperAridOccs$name))

write.csv(x = unique(HyperAridOccs$name), file = "HyperAridSpecies.csv", row.names = F)

pdf("HyperAridOccurrenceMap.pdf")
plot(rastey)
points(HyperAridOccs[,2:3], pch = 19)
dev.off()
#

# Get the number of abstracts that mention each species from hyper arid areas
HypAridOccCounts <- read.csv("HyperAridSpecies_w_AbsCounts.csv")
HypAridOccCounts$Species <- gsub(x = HypAridOccCounts$Species, replacement = " ", pattern = "_")
UselessPlants <- read.csv("Useless_Plants.csv")

HypAridOccCounts$Species %in% UselessPlants$Species # Check the "usefulness" of each hyper
# arid species per Kew list. Ok, all the Hyper Arids are Useless. Cool.
#