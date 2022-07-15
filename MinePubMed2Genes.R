### AE Melton, 2022
# Mine header data extracted from GenBank using PubMed2GenesTaxo.R by SB

library(stringr)

# Read in the csv output of PubMed2GenesTaxo.R
df1 <- read.csv("PubMed2Genes/PubMed2Seq.csv")
head(df1) # Always check the data

df2 <- read.csv("PubMed2Genes/PubMed2Seq_2.csv")
head(df2) # Always check the data

df <- rbind(df1, df2) # Combine the two outputs

AbstractsStrings <- df$Definition
IDs <- as.character(df$PubMedID)
GenBankGenes <- G2PMineR::GenesLookeR(AbstractsStrings,
                      IDs,
                      Kingdom = "P",
                      Add = AdditionalGenesDF,
                      SppAbbr = as.character(SpeciesAbbrvs))
write.csv(GenBankGenes,"GenBankGenesOutWithSyns.csv", row.names = F)

# Replace gene synonyms with accepted gene names
GenBankGenesOut <- G2PMineR::SynonymReplaceR(GenBankGenes, Kingdom = "P")

# write.csv(GenBankGenesOut,"GenBankGenesOut.csv", row.names = F)

AbsCounts <- c(str_count(GenBankGenesOut$Matches, ',') + 1)
GeneNameCounts <- data.frame(GenBankGenesOut$Gene, AbsCounts)
GeneNameCounts[order(GeneNameCounts$AbsCounts, decreasing = T),]

# write.csv(GeneNameCounts, "GeneNameCounts.csv", row.names = F)

#GenesOut <- read.csv("GenesOut.csv")
# (Optional) Create artificial gene groups
GenBankGenesGroups <- G2PMineR::GeneNamesGroupeR(as.vector(GenBankGenesOut[,1]))

# (Optional) Grade the usefulness of matches
GenBankGenesGrades <- G2PMineR::UtilityGradeR(GenBankGenesOut,
                                              Kingdom = "P",
                                              Add = NULL,
                                              Groups = as.data.frame(GenBankGenesGroups))

# (Optional) Sift genes by frequency, NOTE this asks for a user input through a prompt
GenBankGenesSifted <- G2PMineR::GeneFrequencySifteR(GenBankGenesOut,IDs)

#
df1 <- read.csv("PubMed2Genes/PubMed2Seq.csv")
head(df1) # Always check the data

df2 <- read.csv("PubMed2Genes/PubMed2Seq_2.csv")
head(df2) # Always check the data

df <- rbind(df1, df2) # Combine the two outputs
as.numeric(ConsensusIDs)

library(RColorBrewer)
myCol <- brewer.pal(3, "Pastel2")

VennDiagram::venn.diagram(x = list(df$PubMedID, as.numeric(ConsensusIDs)),
                          category.names = c("rentrez", "G2PMiner"),
                          fill = myCol[1:2],
                          cat.col = myCol[1:2],
                          filename = "miningVenn.png")

# And finally, let's compare these results to the G2PMineR results
ConSpp <- read.csv("ConsensusSummary.csv")[,2] # Available from primary G2PMineR functions
rentezSPP <- read.csv("PubMed2Genes/Contigency_table_speciesPubMed.csv")[,1]

ConGenes <- read.csv("ConsensusGenes.csv")[,1]
rentrezGenes <- GenBankGenesOut$Gene

ConPheno <- read.csv("PhenoCon.csv")[,1]

library(RColorBrewer)
myCol <- brewer.pal(2, "Pastel2")

# ConGenes, rentrezGenes, ConPheno

VennDiagram::venn.diagram(x = list(ConSpp, species),
                          category.names = c("G2PMiner T", "rentrez T"),
                          fill = myCol[1:2],
                          cat.col = myCol[1:2],
                          filename = "miningVennTaxa.png")

# Let 's check the dates of publications for biases
date.df <- read.csv("PubMed2Genes/PubMed2GenesTaxo_year_occ_16March2022.csv")
head(date.df)
date.df.NA <- date.df[which(is.na(date.df$Year) == T),]
date.df.COMPLETE <- date.df[which(is.na(date.df$Year) == F),]

hist(date.df.COMPLETE$Year)
range(date.df.COMPLETE$Year)

Group <- rep("Total", nrow(date.df.COMPLETE))
date.df.COMPLETE <- cbind(date.df.COMPLETE$Year, Group)
rentrezID <- date.df.COMPLETE[which(date.df.COMPLETE$x %in% unique(df$PubMedID)),]
Group <- rep("Database Mining", nrow(rentrezID))
rentrezID <- cbind(rentrezID$Year, Group)
g2pID <- date.df.COMPLETE[which(date.df.COMPLETE$x %in% as.numeric(ConsensusIDs)),]
Group <- rep("Abstract Mining", nrow(g2pID))
g2pID <- cbind(g2pID$Year, Group)

all.data.df <- rbind(date.df.COMPLETE, rentrezID)
all.data.df <- rbind(all.data.df, g2pID)
all.data.df <- data.frame(all.data.df)


no.total.data.df <- all.data.df[which(all.data.df$Group != "Total")]
colnames(all.data.df) <- c("Year", "Group")
library(ggplot2)
library(VennDiagram)
# Histogram with density plot and mean line

# need both for rentrez
# Change axis labels 

pdf("PubMed2Genes/Abstract_Ages.pdf")
ggplot(all.data.df, aes(x = Year, fill = Group)) + 
  geom_histogram(binwidth = 1, position = "dodge", stat = "count") + # position = "identity", alpha = 0.2, 
  theme(axis.text.x = element_text(angle = 45)) #+ #aes(y=..density..), #colour = "black", fill = "white", 
  #geom_density(alpha=.2, fill = "#FF6666") +
  #geom_vline(aes(xintercept = mean(Year)),
  #         color = "blue", linetype = "dashed", size=1)
dev.off()

### Mine the databased abstracts for phenotypes
head(Drought_Phenotypes)

s <- strsplit(con.phen$AbsMatches, split = ",")
PhenoDroughtAbsIDs <- data.frame(V1 = rep(con.phen$PhenoWord, sapply(s, length)), V2 = unlist(s))
uniqueDroughtAbsIDs <- unique(PhenoDroughtAbsIDs$V2)


df.no.missing <- date.df[which(date.df$NGenBankAcc > 0),]
df.no.missing <- df.no.missing[which(df.no.missing$NSpecies > 0),]

DataMiningphenoAbIDs <- PhenoDroughtAbsIDs[which(PhenoDroughtAbsIDs$V2 %in% df.no.missing$x),]

GenBankGenesOut$Matches
s <- strsplit(GenBankGenesOut$Matches, split = ",")
GeneDroughtAbsIDs <- data.frame(V1 = rep(GenBankGenesOut$Gene, sapply(s, length)), V2 = unlist(s))
#uniqueDroughtAbsIDs <- unique(DroughtAbsIDs$V2)


contTable <- read.csv("PubMed2Genes/EDITSContigency_table_speciesPubMed.csv")

# ConGenes, rentrezGenes, ConPheno
library(ggVennDiagram)
library(cowplot)
library(ggvenn)
library(stringr)
library(RColorBrewer)
display.brewer.pal(n = 3, name = "PRGn")
myCol <- brewer.pal(3, "PRGn") # Picking n = 3 because that's the lowest it'll go

# gg
plot.a <- ggVennDiagram(x= list(ConGenes, unique(GeneDroughtAbsIDs$V1)),
                        category.names = c("Abstract Mining", "Database Mining"),
                        label_alpha = 0,
                        #category.names = names(x),
                        set_size = 3.5) +
  ggplot2::scale_fill_gradient(low="blue",high = "purple")

plot.b <- ggVennDiagram(x = list(ConSpp, contTable$Taxized),
                        category.names = c("Abstract Mining", "Database Mining"),
                        label_alpha = 0,
                        #category.names = names(x),
                        set_size = 3.5) +
  ggplot2::scale_fill_gradient(low="pink",high = "red")

plot.c <- ggVennDiagram(x = list(unique(PhenoDroughtAbsIDs$V1), unique(DataMiningphenoAbIDs$V1)),
                        category.names = c("Abstract Mining", "Database Mining"),
                        label_alpha = 0,
                        #category.names = names(x),
                        set_size = 3.5) +
  ggplot2::scale_fill_gradient(low="orange",high = "dark orange")

pdf("PubMed2Genes/AllTheOverlap.pdf")
plot_grid(plot.a, plot.b, plot.c, labels = c('Genes', 'Taxonomy', 'Phenotypes'), ncol = 1, label_size = 12)
dev.off()
### vennDiagram
plot.a <- venn.diagram(x = list(ConGenes, unique(GeneDroughtAbsIDs$V1)),
                          category.names = c("Abstract Mining", "Database Mining"),
                          fill = myCol[c(1,3)],
                          cat.col = myCol[c(1,3)],
                          sub.cex = 0,
                          main = "Genes",
                          filename = NULL)
plot.b <- venn.diagram(x = list(ConSpp, contTable$Taxized),
                          category.names = c("Abstract Mining", "Database Mining"),
                          fill = myCol[c(1,3)],
                          cat.col = myCol[c(1,3)],
                          sub.cex = 0,
                          main = "Taxonomy",
                          filename = NULL)
plot.c <- venn.diagram(x = list(unique(PhenoDroughtAbsIDs$V1), unique(DataMiningphenoAbIDs$V1)),
                          category.names = c("Abstract Mining", "Database Mining"),
                          fill = myCol[c(1,3)],
                          cat.col = myCol[c(1,3)],
                          sub.cex = 0,
                          main = "Phenotypes",
                          filename = NULL)

# Building venndiagram grid objects (i.e. gList)
# a<- venn.diagram(list(av= av, bv=bv), filename=NULL, main="VD1")
# b<- venn.diagram(list(av=av, cv=cv), filename=NULL, main="VD2")
# c<- venn.diagram(list(bv=bv, cv=cv), filename=NULL, main="VD3")
# d<- venn.diagram(list(av=av, cv=cv), filename=NULL, main="VD4")

# Draw the diagrams
#pdf("PubMed2Genes/AllTheOverlapV2.pdf")
pushViewport(plotViewport(layout=grid.layout(2, 2)))
pushViewport(plotViewport(layout.pos.col=1, layout.pos.row=1))
grid.draw(plot.a)
popViewport()
pushViewport(plotViewport(layout.pos.col=2, layout.pos.row=1))
grid.draw(plot.b)
popViewport()
pushViewport(plotViewport(layout.pos.col=1, layout.pos.row=2))
grid.draw(plot.c)
popViewport()
# pushViewport(plotViewport(layout.pos.col=2, layout.pos.row=2))
# grid.draw(d) 
dev.off()

