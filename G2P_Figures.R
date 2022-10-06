### AE Melton, 2021
# This script will take the Human Use data and the output of the G2PMineR pipeline to 
# generate barplots showing the top ten taxa, gene, and phenotype words. 

# Load the libraries
library(cowplot)
library(ggplot2)
library(stringr)

# Read in the Human Use data
df <- read.csv(file = "HumanUse_FILE.csv") # This information is available in Table S2 of the Supplemental Materials
head(df)

# Sort and take the top ten species.
top.ten.Species <- head(df[order(-df$Occurrence),], 10)

# Generate a top species barplot.
pdf("TopSpeciesBarplot.pdf")
ggplot(top.ten.Species, aes(x = Species, y = Occurrence)) + 
  geom_bar(stat = "identity") + 
  theme(axis.text.x = element_text(face = "italic", angle = -45)) +
  ylab("Abstract Mentions") +
  xlab("Species")
dev.off()
###

# Isolate species from the 'consensus' object and save the species listed in consensus abstract.
head(All_Consensus$`Taxonomy Consensus-Only`)

Taxa.Name <- All_Consensus$`Taxonomy Consensus-Only`$Species
Taxa.Count <- str_count(All_Consensus$`Taxonomy Consensus-Only`$Matches, ',') + 1

df.TaxaCount <- data.frame(Taxa.Name, as.numeric(Taxa.Count))
head(df.TaxaCount)

top.ten.Taxa <- head(df.TaxaCount[order(-df.TaxaCount$as.numeric.Taxa.Count.),], 10)
write.csv(df.TaxaCount, file = "ConsensusTaxa.csv", row.names = F)

# Next, do the same with genes
head(All_Consensus$`Genes Consensus-Only`)

Gene.Name <- All_Consensus$`Genes Consensus-Only`$Gene
Gene.Count <- str_count(All_Consensus$`Genes Consensus-Only`$Matches, ',') + 1

df.GeneCount <- data.frame(Gene.Name, as.numeric(Gene.Count))
head(df.GeneCount)

top.ten.Genes <- head(df.GeneCount[order(-df.GeneCount$as.numeric.Gene.Count.),], 10)

# Generate a gene-only barplot
pdf("TopGenesBarplot.pdf")
ggplot(top.ten.Genes, aes(x = Gene.Name, y = as.numeric.Gene.Count.)) + 
  geom_bar(stat = "identity") + 
  theme(axis.text.x = element_text(face = "italic", angle = -45)) +
  ylab("Abstract Mentions") +
  xlab("Genes")
dev.off()
###

# and now for phenotypes
head(All_Consensus$`Phenotypes Consensus-Only`)

Phenotypes.Name <- All_Consensus$`Phenotypes Consensus-Only`$PhenoWord
Phenotypes.Count <- All_Consensus$`Phenotypes Consensus-Only`$NumberAbs

df.PhenotypesCount <- data.frame(Phenotypes.Name, as.numeric(Phenotypes.Count)) # This object can be used as input for the "PhenoWord_DroughtStrategy_Counts.R" script
head(df.PhenotypesCount)

top.ten.Phenotypes <- head(df.PhenotypesCount[order(-df.PhenotypesCount$as.numeric.Phenotypes.Count.),], 10)

pdf("TopPhenotypesBarplot.pdf")
ggplot(top.ten.Phenotypes, aes(x = Phenotypes.Name, y = as.numeric.Phenotypes.Count.)) + 
  geom_bar(stat = "identity") + 
  theme(axis.text.x = element_text(angle = -45)) +
  ylab("Abstract Mentions") +
  xlab("Phenotypes")
dev.off()
###

#Generate a combined plot for taxa, genes, and phenotypes
A <- ggplot(top.ten.Taxa, aes(x = Taxa.Name, y = as.numeric.Taxa.Count.)) + 
  geom_bar(stat = "identity") + 
  theme(axis.text.x = element_text(face = "italic", angle = -25)) +
  ylab("Abstract Mentions") +
  xlab("Species")

B <- ggplot(top.ten.Genes, aes(x = Gene.Name, y = as.numeric.Gene.Count.)) + 
  geom_bar(stat = "identity") + 
  theme(axis.text.x = element_text(face = "italic", angle = -25)) +
  ylab("Abstract Mentions") +
  xlab("Genes")

C <- ggplot(top.ten.Phenotypes, aes(x = Phenotypes.Name, y = as.numeric.Phenotypes.Count.)) + 
  geom_bar(stat = "identity") + 
  theme(axis.text.x = element_text(angle = -25)) +
  ylab("Abstract Mentions") +
  xlab("Phenotypes")

pdf("AllTheBarplots.pdf")
cowplot::plot_grid(A, B,  C, labels = "AUTO", align = "v", ncol = 1, label_size = 12, label_x = 0, label_y = 0, hjust = -0.5, vjust = -0.5)
dev.off()
###
