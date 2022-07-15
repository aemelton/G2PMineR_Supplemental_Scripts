### AE Melton, 2021
# Generate a phylogeny plot with abstract mining data mapped onto it
# Phylogeny used for this plot is from https://nph.onlinelibrary.wiley.com/doi/10.1111/nph.13264

# Load libraries
# if (!requireNamespace("BiocManager", quietly = TRUE))
#   install.packages("BiocManager")
# 
# BiocManager::install("ggtree")
library(ggimage)
library(ggnewscale)
library(ggtree)
library(phytools)
library(evobiR)
#

# Load phylogeny
phy <- ggtree::read.tree(file = "FamilyTree.phy")
length(phy$tip.label)
is.na(phy$tip.label)
plotTree(tree = phy, type = "fan") # Just to checkcheckcheck

# Read in data
fams <- read.csv(file = "FamilyOccurrenceTotalsCONSENSUS.csv")
head(fams)
is.na(fams)
nrow(fams)

# Let's trim the tree to only the families we have represented from the abstract data
fams$Fam[-which(fams$Fam %in% phy$tip.label)] # OK, we have some non-flower-power here. We'll lcome back to this....
phy$tip.label[-which(phy$tip.label %in% fams$Fam)]
phy.drops <- phy$tip.label[-which(phy$tip.label %in% fams$Fam)]
length(phy.drops)
phy.pruned <- drop.tip(phy = phy, tip = phy.drops)
plotTree(phy.pruned, type = "fan")

# Try a heatmap tree with just abstract counts
fams.trim <- read.csv(file = "FamilyOccurrenceTotalsCONSENSUS.csv", row.names = 1)
dummy.order <- ReorderData(tree = phy.pruned, data = fams.trim) # Reorder so that order of tips and data match
df <- as.data.frame(dummy.order)
abs <- df[1]
sp <- df[2]
#Ngen <- df[3]
#Nspp <- df[4]
Genes <- df[5]
Phenes <- df[6]

# Generate the plot
pdf("PhyloFig.pdf")
p <- ggtree(phy.pruned, layout = "circular") + geom_tiplab(size = 2, align = TRUE, hjust = -0.1)

p1 <- gheatmap(p = p, data = abs, offset = 425, width = 0.1, colnames = F, legend_title = "Abstracts") + scale_fill_gradientn(colors = RColorBrewer::brewer.pal(n = 3, name = "Blues"), name = "Abstracts")

p2 <- p1 + new_scale_fill()
p3 <- gheatmap(p = p2, data = sp, offset = 455, width = 0.1, colnames = F, legend_title = "Species")  + scale_fill_gradientn(colors = RColorBrewer::brewer.pal(n = 3, name = "Reds"), name = "Species")

#p4 <- p3 + new_scale_fill()
#p5 <- gheatmap(p = p4, data = Ngen, offset = 385, width = 0.1, colnames = F, legend_title = "Genera per Fam")  + scale_fill_gradientn(colors = RColorBrewer::brewer.pal(n = 3, name = "Oranges"), name = "Genera per Fam")

#p6 <- p5 + new_scale_fill()
#p7 <- gheatmap(p = p6, data = Nspp, offset = 415, width = 0.1, colnames = F, legend_title = "Species per Fam")  + scale_fill_gradientn(colors = RColorBrewer::brewer.pal(n = 3, name = "Reds"), name = "Species per Fam")

p8 <- p3 + new_scale_fill()
p9 <- gheatmap(p = p8, data = Genes, offset = 485, width = 0.1, colnames = F, legend_title = "Genes per Fam")  + scale_fill_gradientn(colors = RColorBrewer::brewer.pal(n = 3, name = "Purples"), name = "Genes per Fam")

p10 <- p9 + new_scale_fill()
p11 <- gheatmap(p = p10, data = Phenes, offset = 515, width = 0.1, colnames = F, legend_title = "Phenes per Fam")  + scale_fill_gradientn(colors = RColorBrewer::brewer.pal(n = 3, name = "Oranges"), name = "Phenotypes per Fam")

plot(p11)
dev.off()