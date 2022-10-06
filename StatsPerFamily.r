### AE Melton, 2021
# Get counts for genotypes and phenotypes per family

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
