### AE Melton, 2021
# Make some plots from the G2P outputs

library(cowplot)
library(ggplot2)
library(stringr)

###
df <- read.csv(file = "HumanUse_v3_w_Fam.csv") # This information is available in Table S2 of the Supplemental Materials
head(df)

top.ten.Species <- head(df[order(-df$Occurrence),], 10)

pdf("C:/Users/anthonymelton/Dropbox/Manuscripts/BSU/Drought_Adaptation_Review/Review_MetaAnalysis_DroughtTolerance_Plants/AEM_Figures/TopSpeciesBarplot.pdf")
ggplot(top.ten.Species, aes(x = Species, y = Occurrence)) + 
  geom_bar(stat = "identity") + 
  theme(axis.text.x = element_text(face = "italic", angle = -45)) +
  ylab("Abstract Mentions") +
  xlab("Species")
dev.off()
###
###
head(All_Consensus$`Taxonomy Consensus-Only`)

Taxa.Name <- All_Consensus$`Taxonomy Consensus-Only`$Species
Taxa.Count <- str_count(All_Consensus$`Taxonomy Consensus-Only`$Matches, ',') + 1

df.TaxaCount <- data.frame(Taxa.Name, as.numeric(Taxa.Count))
head(df.TaxaCount)

top.ten.Taxa <- head(df.TaxaCount[order(-df.TaxaCount$as.numeric.Taxa.Count.),], 10)
write.csv(df.TaxaCount, file = "ConsensusTaxa.csv", row.names = F)
###
head(All_Consensus$`Genes Consensus-Only`)

Gene.Name <- All_Consensus$`Genes Consensus-Only`$Gene
Gene.Count <- str_count(All_Consensus$`Genes Consensus-Only`$Matches, ',') + 1

df.GeneCount <- data.frame(Gene.Name, as.numeric(Gene.Count))
head(df.GeneCount)

top.ten.Genes <- head(df.GeneCount[order(-df.GeneCount$as.numeric.Gene.Count.),], 10)

pdf("C:/Users/anthonymelton/Dropbox/Manuscripts/BSU/Drought_Adaptation_Review/Review_MetaAnalysis_DroughtTolerance_Plants/AEM_Figures/TopGenesBarplot.pdf")
ggplot(top.ten.Genes, aes(x = Gene.Name, y = as.numeric.Gene.Count.)) + 
  geom_bar(stat = "identity") + 
  theme(axis.text.x = element_text(face = "italic", angle = -45)) +
  ylab("Abstract Mentions") +
  xlab("Genes")
dev.off()
###

###
head(All_Consensus$`Phenotypes Consensus-Only`)

Phenotypes.Name <- All_Consensus$`Phenotypes Consensus-Only`$PhenoWord
Phenotypes.Count <- All_Consensus$`Phenotypes Consensus-Only`$NumberAbs

df.PhenotypesCount <- data.frame(Phenotypes.Name, as.numeric(Phenotypes.Count))
head(df.PhenotypesCount)

top.ten.Phenotypes <- head(df.PhenotypesCount[order(-df.PhenotypesCount$as.numeric.Phenotypes.Count.),], 10)

pdf("C:/Users/anthonymelton/Dropbox/Manuscripts/BSU/Drought_Adaptation_Review/Review_MetaAnalysis_DroughtTolerance_Plants/AEM_Figures/TopPhenotypesBarplot.pdf")
ggplot(top.ten.Phenotypes, aes(x = Phenotypes.Name, y = as.numeric.Phenotypes.Count.)) + 
  geom_bar(stat = "identity") + 
  theme(axis.text.x = element_text(angle = -45)) +
  ylab("Abstract Mentions") +
  xlab("Phenotypes")
dev.off()
###

###
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

pdf("~/Dropbox/Manuscripts/BSU/Drought_Adaptation_Review/Review_MetaAnalysis_DroughtTolerance_Plants/AEM_Figures/AllTheBarplot_V2.pdf")
cowplot::plot_grid(A, B,  C, labels = "AUTO", align = "v", ncol = 1, label_size = 12, label_x = 0, label_y = 0, hjust = -0.5, vjust = -0.5)
dev.off()
###
