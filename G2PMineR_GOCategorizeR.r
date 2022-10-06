### AE Melton, 2021
# Find top GO categories for G2PMineR gene sets

### Get GO cats for all the ALL genes
AllCategoryList # Generated in the PhenoGene Venn Diagram script
AllCatGeneCounts <- df.GeneCount[which(df.GeneCount$Gene.Name %in% AllCategoryList),]
write.csv(x = AllCatGeneCounts[order(-AllCatGeneCounts$as.numeric.Gene.Count.),], file = "AllCatGenesCounts.csv", row.names = F)

AllCatGeneDF <- GenesCon[which(GenesCon$Gene %in% AllCategoryList),]

# OK! First, let's separate each string so that there is one GO cat per entry
LetsGOOOO.stringy <- unlist(strsplit(AllCatGeneDF$Ontology, ";", fixed = TRUE))

# OK! Next, let's get rid of everything that';s not a GO categor!
#
LetsGOOOO <- data.frame(unlist(str_extract_all(LetsGOOOO.stringy, "\\[GO:[0-9]{7}\\]", simplify = FALSE)))
head(LetsGOOOO)
colnames(LetsGOOOO) <- "GO_Number"
write.csv(x = LetsGOOOO, file = "GO_Categories_for_AllCatGenes.csv", row.names = F)
#

nrow(LetsGOOOO)
length(unique(LetsGOOOO$GO_Number))

write.csv(x = sort(table(LetsGOOOO$GO_Number), decreasing = T), file = "GOCat_AllCatGenes_Counts.csv", row.names = F)

hist(sort(table(LetsGOOOO$GO_Number),decreasing = T)) # Generate a histogram of GO category counts

