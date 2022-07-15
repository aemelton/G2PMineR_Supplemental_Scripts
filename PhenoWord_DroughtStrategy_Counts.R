### AE Melton, 2021
# Get counts for the occurrence of plant drought strategy phenotype words

# Read in the drought phenotype lexicon
DroughtStrategies <- read.csv("Drought_Lexicon.csv") # Data are available in Table S6
head(DroughtStrategies)
#

#
head(df.PhenotypesCount) #From G2P Figures script

HowDeal <- NULL # Create an empty object to populate with count data

# Loop over the lexicon and search for these words in consensus abstract phenotypes
for (i in 1:nrow(df.PhenotypesCount)) {
  HowDeal[i] <- Drought_Lexicon_Long_V2$Area[df.PhenotypesCount$Phenotypes.Name[i] == Drought_Lexicon_Long_V2$Phrase]
}
head(HowDeal)
df.PhenoStrats <- data.frame(df.PhenotypesCount, HowDeal)
head(df.PhenoStrats)
#

#
HowDeal <- c("Avoidance", "Recovery", "Tolerance", "General Stress", "Detection", "Escape")
Total <- NULL
for (i in 1:length(HowDeal)) {
  tmp <- df.PhenoStrats[grep(pattern = HowDeal[i], x = df.PhenoStrats$HowDeal),]
  Total[i] <- sum(tmp$Phenotypes.Count)
}
Total
df <- data.frame(HowDeal, Total)
head(df)
write.csv(x = df, file = "DroughtPhenoWordStrategy_Counts.csv", row.names = F)
#