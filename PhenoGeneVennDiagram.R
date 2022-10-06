### AE Melton, 2021
# Venn diagram of phenogenes per drought strategy
#
# if (!require(devtools)) install.packages("devtools")
# devtools::install_github("yanlinlin82/ggvenn")
#

# Load required libraries
library(ggVennDiagram)
library(ggvenn)
library(stringr)
#

# read in lexicon of drought related phenotype terms
Lexicon <- read.csv("Drought_Lexicon.csv") # Data are available in Table S6
ncol(PhenoGenes)

# Create an object with the phenotype categories
strategies <- c("Avoidance", "Detection", "Escape", "Tolerance", "Recovery", "General Stress")

# The next sections look for each phenotype word related to a specific strategy within the
# PhenoGenes object created in the G2PMineR pipeline
Avoidance <- Lexicon[grep(pattern = strategies[1], x = Lexicon$Area),1]

AvoidanceGenes <- NULL
for (i in 1:nrow(PhenoGenes)) {
  for (j in 1:ncol(PhenoGenes)) {
    if (row.names(PhenoGenes)[i] %in% Avoidance == T && PhenoGenes[i,j] > 0) {
      AvoidanceGenes <- c(AvoidanceGenes, colnames(PhenoGenes)[j])
    }
  }
}

AvoidanceGenes
#

#
Detection <- Lexicon[grep(pattern = strategies[2], x = Lexicon$Area),1]

DetectionGenes <- NULL
for (i in 1:nrow(PhenoGenes)) {
  for (j in 1:ncol(PhenoGenes)) {
    if (row.names(PhenoGenes)[i] %in% Detection == T && PhenoGenes[i,j] > 0) {
      DetectionGenes <- c(DetectionGenes, colnames(PhenoGenes)[j])
    }
  }
}

DetectionGenes
#

#
Escape <- Lexicon[grep(pattern = strategies[3], x = Lexicon$Area),1]

EscapeGenes <- NULL
for (i in 1:nrow(PhenoGenes)) {
  for (j in 1:ncol(PhenoGenes)) {
    if (row.names(PhenoGenes)[i] %in% Escape == T && PhenoGenes[i,j] > 0) {
      EscapeGenes <- c(EscapeGenes, colnames(PhenoGenes)[j])
    }
  }
}

EscapeGenes
#

#
Tolerance <- Lexicon[grep(pattern = strategies[4], x = Lexicon$Area),1]

ToleranceGenes <- NULL
for (i in 1:nrow(PhenoGenes)) {
  for (j in 1:ncol(PhenoGenes)) {
    if (row.names(PhenoGenes)[i] %in% Tolerance == T && PhenoGenes[i,j] > 0) {
      ToleranceGenes <- c(ToleranceGenes, colnames(PhenoGenes)[j])
    }
  }
}

ToleranceGenes
#

#
Recovery <- Lexicon[grep(pattern = strategies[5], x = Lexicon$Area),1]

RecoveryGenes <- NULL
for (i in 1:nrow(PhenoGenes)) {
  for (j in 1:ncol(PhenoGenes)) {
    if (row.names(PhenoGenes)[i] %in% Recovery == T && PhenoGenes[i,j] > 0) {
      RecoveryGenes <- c(RecoveryGenes, colnames(PhenoGenes)[j])
    }
  }
}

RecoveryGenes
#

#
Stress <- Lexicon[grep(pattern = strategies[6], x = Lexicon$Area),1]

StressGenes <- NULL
for (i in 1:nrow(PhenoGenes)) {
  for (j in 1:ncol(PhenoGenes)) {
    if (row.names(PhenoGenes)[i] %in% Stress == T && PhenoGenes[i,j] > 0) {
      StressGenes <- c(StressGenes, colnames(PhenoGenes)[j])
    }
  }
}

StressGenes
write.csv(x = StressGenes, "StressGenes.csv", row.names = F)
#

# UNUSED FOR PUBLICATION
# VennDiagram::venn.diagram(x = list(AvoidanceGenes, DetectionGenes, EscapeGenes, ToleranceGenes, RecoveryGenes),
#                          category.names = c("Avoidance", "Detection", "Escape", "Tolerance", "Recovery"),
#                          filename = "~/Desktop/GeneVennTest.png")

# Generate venn diagram with ggVennDiagram
x = list(unique(AvoidanceGenes),
         unique(DetectionGenes),
         unique(EscapeGenes),
         unique(ToleranceGenes),
         unique(RecoveryGenes))
names(x) <- c("Avoidance", "Detection", "Escape", "Tolerance", "Recovery")

pdf(file = "PhenoGenoType.pdf")
ggVennDiagram(x,
              label_alpha = 0,
              category.names = names(x),
              set_size = 3.5) +
  ggplot2::scale_fill_gradient(low="pink",high = "red")
dev.off()

###
#AvoidanceGenes[which(AvoidanceGenes %in% DetectionGenes %in% EscapeGenes %in% ToleranceGenes %in% RecoveryGenes)]
Intersect <- function (x) {  
  # Multiple set version of intersect
  # x is a list
  if (length(x) == 1) {
    unlist(x)
  } else if (length(x) == 2) {
    intersect(x[[1]], x[[2]])
  } else if (length(x) > 2){
    intersect(x[[1]], Intersect(x[-1]))
  }
}

AllCategoryList <- Intersect(x) # This will identify genes that occur in all strategy lists.
write.csv(AllCategoryList, "AllCategoryList.csv", row.names = F)