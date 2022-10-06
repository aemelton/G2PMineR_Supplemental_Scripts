### AE Melton, 2021
# This script will identify plants lacking human use and provide some summary statistics

# Load libraries
library(plyr)
library(dplyr)
library(raster)
library(taxize)

### Filter out species to get only plants lacking human user per Ker Checklist
HumanUse <- read.csv("HumanUse_FILE.csv") # Data are in Table S2
colnames(HumanUse) <- c("Species", "Occurrence", "Kew_Checklist", "Notes") # "Occurrence" == Total Abstracts in Table S2

# take genera names and get family names for the species
genera <- gsub(x = HumanUse$Species, replacement = "", pattern = " .*")
unique.genera <- unique(genera)
families <- tax_name(sci = unique.genera, get = "family", db = "ncbi")
length(unique(families$family))

colnames(families) <- c("db", "Genus", "Family")

df <- data.frame(Genus = character(615), Species = character(615), Occurrence = numeric(615), Family = character(615)) # Number of rows = total number of species identified in G2PMineR pipeline
df$Genus <- gsub(x = HumanUse$Species, replacement = "", pattern = " .*")
df$Species <- HumanUse$Species
df$Occurrence <- HumanUse$Occurrence

### Calculate number of occurrences per family and sort from highest to lowest
head(HumanUse)
head(df)

for (i in 1:nrow(df)) {
  tmp.Family[i] <- families$Family[df$Genus[i] == families$Genus]
}
tmp.Family

df$Family <- tmp.Family
# write.csv(x = df, file = "HumanUse_FILE.csv", row.names = F)

df <- read.csv("HumanUse_FILE") # Data are in Table S2
head(df)
length(unique(df$Family))
df[order(-df$Abstracts),] 
sort(table(df$Family))
# write.csv(x = sort(table(df$Family)), file = "Family_counts_CONSENSUSONLY.csv", row.names = F)
genera <- gsub(x = df$Species, replacement = "", pattern = " .*")
unique(genera)

df <- read.csv("HumanUse_FILE") # Data are in Table S2
tmp.Family.df <- data.frame(TotalOccurrences = numeric(99), Fam = character(99)) # 99 families total, loop over all family names and count total per family
for (j in 1:length(unique(df$Family))) {
  tmp <- subset(x = df, df$Family == unique(df$Family)[j])
  tmp.Family.df$TotalOccurrences[j] <- sum(tmp$Occurrence)
  tmp.Family.df$Fam[j] <- tmp$Family
}  
tmp.Family.df
sort(tmp.Family.df$TotalOccurrences)
sort(table(tmp.Family.df$TotalOccurrences))
# write.csv(x = tmp.Family.df, file = "FamilyOccurrenceTotalsCONSENSUS.csv", row.names = F)
#

#
nrow(Species)
nrow(HumanUse)

head(Species)
head(HumanUse)
unique(Species$Species.REAL)
colnames(HumanUse) <- c("Species.Query", "Occurrence", "Kew_Checklist", "Notes")
head(HumanUse)
#

#
print(HumanUse %>% anti_join(Species)) # Identify species in HumanUse file but were not in the consensus abstract species

df <- join(x = Species, y = HumanUse, by = "Species.Query")
head(df)
df[duplicated(df$Species.REAL),]
#

# Identify most commonly used plants
df <- read.csv("Species_Frequency_2021_HumanUse.csv")
colnames(df) <- c("Species", "Occurrence", "Kew_Checklist", "Notes")
nrow(df)

df.sorted <- df[order(-df$Occurrence),] 
head(df.sorted)
sum(df$Occurrence)
#

# Identify plants lacking human use
Useless <- df.sorted[is.na(df.sorted$Kew_Checklist), ]
nrow(Useless)

(nrow(df) - nrow(Useless))/nrow(df)

Useless
write.csv(x = Useless, file = "Useless_Plants.csv", row.names = F)
###

###
df <- read.csv("Species_Frequency_2021_HumanUse.csv") # Data are in Table S2
nrow(df)
head(df)

# Kew Human Use Code
# Key	Term
# AF	Animal food
# EU	Environmental uses
# FU	Fuels
# GS	Gene sources
# HF	Human food
# IF	Invertebrate food
# MA	Materials
# ME	Medicines
# PO	Poisons
# SU	Social uses
UseDefs <- read.csv("KEW_HumanUse_Definitions.csv")
head(UseDefs)

# Get counts of each Human Use type
ConsensusTaxaTable <- read.csv("ConsenusOnlyTaxaTable.csv") 
head(ConsensusTaxaTable)
nrow(ConsensusTaxaTable)
ConsensusTaxaTable.sub <- df[which(df$Species %in% ConsensusTaxaTable$Taxa.Name),]
nrow(ConsensusTaxaTable.sub)
ConsensusTaxaTable.sub$Occurence <- ConsensusTaxaTable$as.numeric.Taxa.Count.
ConsensusTaxaTable.sub.NoNA <- na.omit(ConsensusTaxaTable.sub)
nrow(ConsensusTaxaTable.sub.NoNA)
#

Use.Counts <- NULL
Use <- NULL
Count <- NULL
for (i in 1:nrow(UseDefs)) {
  Use[i] <- UseDefs$Term[i]
  Count[i] <- length(grep(UseDefs[i,1], ConsensusTaxaTable.sub.NoNA$Kew.Checklist))
}
Use.Counts <- data.frame(Use, Count)
Use.Counts[order(-Use.Counts$Count),]

write.csv(x = Use.Counts, file = "HumanUse_Counts.csv", row.names = F)
###

