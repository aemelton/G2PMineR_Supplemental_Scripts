### AE Melton, 2021
# This script includes some follow up analysis on G2PMineR outputs, particularly looking
# at the geographic distributions of taxa found by G2PMineR and then filtering by environmental
# parameters (in this case, aridity).

# Load libraries
library(plyr)
library(dplyr)
library(raster)
library(taxize)

### Filter out species to get only USELESS plants
HumanUse <- read.csv("Species_Frequency_2021_HumanUse.csv") # Data are in Table S2
colnames(HumanUse) <- c("Species", "Occurrence", "Kew_Checklist", "Notes") # "Occurrence" == Total Abstracts in Table S2

# take genera names and get family names for the species
genera <- gsub(x = HumanUse$Species, replacement = "", pattern = " .*")
unique.genera <- unique(genera)
families <- tax_name(sci = unique.genera, get = "family", db = "ncbi")
length(unique(families$family))

colnames(families) <- c("db", "Genus", "Family")

df <- data.frame(Genus = character(615), Species = character(615), Occurrence = numeric(615), Family = character(615))
df$Genus <- gsub(x = HumanUse$Species, replacement = "", pattern = " .*")
df$Species <- HumanUse$Species
df$Occurrence <- HumanUse$Occurrence

### Calculate number of occurrences per family and sort from highest to lowest
head(HumanUse)
head(df)

for (i in 1:nrow(df)) {
  blerp[i] <- families$Family[df$Genus[i] == families$Genus]
}
blerp

df$Family <- blerp
# write.csv(x = df, file = "HumanUse_w_Fam.csv", row.names = F)

df <- read.csv("FILE") # Data are in Table S2
head(df)
length(unique(df$Family))
df[order(-df$Abstracts),] 
sort(table(df$Family))
# write.csv(x = sort(table(df$Family)), file = "Family_counts_CONSENSUSONLY.csv", row.names = F)
genera <- gsub(x = df$Species, replacement = "", pattern = " .*")
unique(genera)

df <- read.csv("FILE") # Data are in Table S2
blorp <- data.frame(TotalOccurrences = numeric(99), Fam = character(99))
for (j in 1:length(unique(df$Family))) {
  tmp <- subset(x = df, df$Family == unique(df$Family)[j])
  blorp$TotalOccurrences[j] <- sum(tmp$Occurrence)
  blorp$Fam[j] <- tmp$Family
}  
blorp
sort(blorp$TotalOccurrences)
sort(table(blorp$TotalOccurrences))
# write.csv(x = blorp, file = "FamilyOccurrenceTotalsCONSENSUS.csv", row.names = F)
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
print(HumanUse %>% anti_join(Species)) # 1 Populus canescens 1 <NA> THIS HAD ONE ABSTRACT BUT IT IS NOT IN CONSENSUS

df <- join(x = Species, y = HumanUse, by = "Species.Query")
head(df)
df[duplicated(df$Species.REAL),]
#

#
df <- read.csv("Species_Frequency_2021_HumanUse.csv")
colnames(df) <- c("Species", "Occurrence", "Kew_Checklist", "Notes")
nrow(df)

df.sorted <- df[order(-df$Occurrence),] 
head(df.sorted)
sum(df$Occurrence)
#

#
Useless <- df.sorted[is.na(df.sorted$Kew_Checklist), ]
nrow(Useless)

(nrow(df) - nrow(Useless))/nrow(df)

Useless
write.csv(x = Useless, file = "Useless_Plants.csv", row.names = F)
###

###
df <- read.csv("Species_Frequency_2021_HumanUse.csv")
nrow(df)
head(df)
UseDefs <- read.csv("KEW_HumanUse_Definitions.csv")
head(UseDefs)

#
doot <- read.csv("ConsenusOnlyTaxaTable.csv") 
head(doot)
nrow(doot)
dootdoot <- df[which(df$Species %in% doot$Taxa.Name),]
nrow(dootdoot)
dootdoot$Occurence <- doot$as.numeric.Taxa.Count.
dootlydoot <- na.omit(dootdoot)
nrow(dootlydoot)
#

Use.Counts <- NULL
Use <- NULL
Count <- NULL
for (i in 1:nrow(UseDefs)) {
  Use[i] <- UseDefs$Term[i]
  Count[i] <- length(grep(UseDefs[i,1], dootlydoot$Kew.Checklist))
}
Use.Counts <- data.frame(Use, Count)
Use.Counts[order(-Use.Counts$Count),]

write.csv(x = Use.Counts, file = "HumanUse_Counts.csv", row.names = F)
###

#
consensi.table <- read.csv("ConsensusSummary.csv") # output from ConsensusSummaryStats script
bleep <- data.frame(Genes = numeric(99), Fam = character(99))
for (j in 1:length(unique(consensi.table$Family))) {
  tmp <- subset(x = consensi.table, consensi.table$Family == unique(consensi.table$Family)[j])
  bleep$Genes[j] <- mean(tmp$NumGenes)
  bleep$Fam[j] <- tmp$Family
}  
bleep
sort(bleep$Genes)
sort(table(bleep$Genes))
write.csv(x = bleep, file = "FamilyGenoTotalsCONSENSUS.csv", row.names = F)
#

blerp <- data.frame(Phenos = numeric(99), Fam = character(99))
for (j in 1:length(unique(consensi.table$Family))) {
  tmp <- subset(x = consensi.table, consensi.table$Family == unique(consensi.table$Family)[j])
  blerp$Phenos[j] <- mean(tmp$NumPhenotypes)
  blerp$Fam[j] <- tmp$Family
}  
blerp
sort(blerp$Phenos)
sort(table(blerp$Phenos))
write.csv(x = blerp, file = "FamilyPhenosTotalsCONSENSUS.csv", row.names = F)

