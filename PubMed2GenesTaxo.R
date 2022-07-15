# by S Buerki

#########
#PubMed IDs 2 Genes and Taxo
##########

###~~~
# R script relying on rentrez to mine DNA and taxonomical data associated to PubMed (= publication) accessions.
# Sven Buerki, Boise State University, March 2022
#
# Input: A csv file with PubMed accessions in 1 col.
# Output a data.frame with 7 columns (= PubMed object) (see below for content)
###~~~

#Load package
require(rentrez)

#Load csv with PubMedIDs (= all publications matching our query on NCBI)
PubMed <- read.csv("IDs_10Dec2020.csv")

###~~~
#Query entrez database for DNA and Taxonomy data associated to PubMed
###~~~

## Add columns for data mining on NCBI/entrez network of databases
# - NGenBankAcc: Number of GenBank sequences associated to PubMed ID
# - NTaxa: Number of taxa in Taxonomy database associated to PubMed ID
# - Taxonomy: Taxonomic names (separated by "|") associated to PubMed ID
# - UniqueSpecies: List of taxa at species level (few cases it will be at genus level) covered per PubMed ID
# - NSpecies: Number of species associated to PubMed ID (= species surveyed)
# - Year: Year when the PubMed ID was published.
PubMed$NGenBankAcc <- rep(0, length(PubMed$x))
PubMed$NTaxa <- rep(0, length(PubMed$x))
PubMed$Taxonomy <- rep(NA, length(PubMed$x))
PubMed$UniqueSpecies <- rep(NA, length(PubMed$x))
PubMed$NSpecies <- rep(0, length(PubMed$x))
PubMed$Year <- rep(NA, length(PubMed$x))
  
## Actual loop conducting web search and data wrangling
print("Processing PubMed accessions: fetching DNA, taxonomy and publication year")
#Set a progress bar
#Check 3312, 3423, 3722, 7241, 
pb <- txtProgressBar(min = 0, max = length(PubMed$NGenBankAcc), style = 3)
for(i in 7343:length(PubMed$NGenBankAcc)){
#for(i in 1:length(PubMed$NGenBankAcc)){
  
  #PubMedYear
  tmp <- entrez_fetch(db = "pubmed", id = PubMed$x[i], rettype = "xml")
  if(length(tmp > 0)){
    PubMed$Year[i] <- strsplit(as.vector(strsplit(as.vector(tmp), "</Year>"))[[1]][1], "<Year>")[[1]][2]
  }
  
  #PubMed2Nuccore
  PubMedInNuc <- rentrez::entrez_link(dbfrom = "pubmed", id = PubMed$x[i], db = "nuccore")
  #Retrieve the number of GenBank sequences associated to pub.
  if(length(PubMedInNuc$links) != 0){
    #Hits and look at # GenBank data submitted
    PubMed$NGenBankAcc[i] <- length(PubMedInNuc$links[[1]])
  }
  
  #PubMed2Taxo
  PubMedInTaxo <- rentrez::entrez_link(dbfrom = "pubmed", id = PubMed$x[i], db = "taxonomy")
  #Retrieve the number of species associated to pub.
  if(length(PubMedInTaxo$links) != 0){ 
    #Hits and look at # Species associated
    PubMed$NTaxa[i] <- length(PubMedInTaxo$links[[1]])
    
    ##If taxonomy hits then save taxa in col.
    #Extract taxa list to be saved in Taxonomy col
    TaxoID <- PubMedInTaxo$links[[1]]
    
    OUT <- NULL
    for(j in 1:length(TaxoID)){
      tmp <- strsplit(strsplit(entrez_fetch(db='taxonomy', id=TaxoID[j], rettype='native'), 
                               split = '\n')[[1]][1], split = ' ')
      OUT <- c(OUT, paste(tmp[[1]][2:length(tmp[[1]])], collapse = ' '))
    }
    if(length(OUT) > 1){
      #Paste the taxa in Taxonomy col
      PubMed$Taxonomy[i] <- paste(OUT, collapse="|")
      #Create unique list of taxa (at species level)
      #Infer length of each elements in OUT
      LengthOUTobj <- sapply(strsplit(OUT, split=" "), length)
      #Id elements <2
      ToDeal <- which(as.numeric(LengthOUTobj) <= 1)
      
      if(length(ToDeal) == 0){
        UniqueSp <- unique(paste(sapply(strsplit(OUT, split=" "), "[[", 1), sapply(strsplit(OUT, split=" "), "[[", 2), sep = " "))  
      }else{
        #Proceed with all objects >=2 and then cat the smaller object (made of 1 element)
        UniqueSp <- c(unique(paste(sapply(strsplit(OUT[-ToDeal], split=" "), "[[", 1), sapply(strsplit(OUT[-ToDeal], split=" "), "[[", 2), sep = " ")), OUT[ToDeal])  
      }
      
      if(length(UniqueSp) > 1){
        PubMed$UniqueSpecies[i] <- paste(UniqueSp, collapse="|")
      }else{
        PubMed$UniqueSpecies[i] <- UniqueSp
      }
      #Number of species = length(UniqueSp)
      PubMed$NSpecies[i] <- length(UniqueSp)
    }else{
      #Only one species
      PubMed$Taxonomy[i] <- OUT
      #Split OUT to identify taxonomic rank
      tmp <- strsplit(OUT, " ")
      if(length(tmp[[1]]) >= 2){
        #This means that the taxon is at species level
        PubMed$UniqueSpecies[i] <- unique(paste(sapply(strsplit(OUT, split=" "), "[[", 1), sapply(strsplit(OUT, split=" "), "[[", 2), sep = " "))
      }else{
        #This means that the taxon is at genus level
        PubMed$UniqueSpecies[i] <- OUT
      }
      PubMed$NSpecies[i] <- 1
    }
  }
  
  # update progress bar
  setTxtProgressBar(pb, i)
}
close(pb)

#Write data out
write.csv(PubMed, "PubMed2GenesTaxo_year_occ_16March2022.csv", row.names = F, quote = F)

###~~~
#What species have been analyzed?
###~~~

SpeciesList <- PubMed$UniqueSpecies[which(PubMed$UniqueSpecies != "NA")]

#Deal with PubMed retruning multiple species
ToSplit <- SpeciesList[grep("[|]", SpeciesList)]

SpeciesToSplit <- NULL
for(i in 1:length(ToSplit)){
  tmp <- strsplit(ToSplit[i], "[|]")[[1]]
  SpeciesToSplit <- c(SpeciesToSplit, tmp)
}

#Produce final list
SpeciesList <- c(SpeciesList[-grep("[|]", SpeciesList)], SpeciesToSplit)

#Contingency table
ContSpecies <- sort(table(SpeciesList), decreasing = T)
write.csv(ContSpecies, file="Contigency_table_speciesPubMed.csv", row.names = F, quote=F)

###~~~
#How many PubMed IDs have DNA and Taxonomic data available in entrez database?
###~~~
statsPubMedEntrez <- matrix(ncol=6, nrow = 1)
colnames(statsPubMedEntrez) <- c("NPubDNAacc", "PercPubDNAacc", "NPubTax", "PercPubTax", "NPubDNATax", "PercPubDNATax")
statsPubMedEntrez <- as.data.frame(statsPubMedEntrez)

# 1. DNA accessions on GenBank
statsPubMedEntrez$NPubDNAacc[1] <- length(which(as.numeric(PubMed$NGenBankAcc) > 0))
statsPubMedEntrez$PercPubDNAacc[1] <- 100*(length(which(as.numeric(PubMed$NGenBankAcc) > 0))/nrow(PubMed))
# 2. Taxonomy
statsPubMedEntrez$NPubTax[1] <- length(which(as.numeric(PubMed$NTaxa) > 0))
statsPubMedEntrez$PercPubTax[1] <- 100*(length(which(as.numeric(PubMed$NTaxa) > 0))/nrow(PubMed))
# 3. Both data
statsPubMedEntrez$NPubDNATax[1] <- length(which(as.numeric(PubMed$NGenBankAcc) > 0 & as.numeric(PubMed$NTaxa) > 0))
statsPubMedEntrez$PercPubDNATax[1] <- 100*(length(which(as.numeric(PubMed$NGenBankAcc) > 0 & as.numeric(PubMed$NTaxa) > 0))/nrow(PubMed))
#Export data in csv
write.csv(statsPubMedEntrez, file="StatsPubMed2GenesTaxo.csv", row.names = F, quote=F)


###~~~
# Download DNA sequences associated to PubMed
###~~~
#PubMedID, GenBankID, Fasta header, Fasta seq
#1. Subset PubMed to only contain those with DNA
#2. Fetch DNA accessions related to PubMedID (<= 1000) length(which(PubMed$NGenBankAcc > 0 & PubMed$NGenBankAcc <= 1000))
#3. Get FASTA description line (= input for G2Pminer to extract genes).
#Open PubMed file
PubMed <- read.csv("PubMed2GenesTaxo_year_occ_16March2022.csv")
# Vector with PubMedID having associated DNA sequences (in nucleotide database) and that have less 1000 sequences (= not NGS studies)
DNAIDs <- PubMed$x[which(PubMed$NGenBankAcc > 0 & PubMed$NGenBankAcc <= 1000)]

# Function to mine meta-data of DNA seq associated to PubMed
# Outputs a data.frame with 5 cols:
# 1. "PubMedID",
# 2. "GenBankID", 
# 3. Fasta definition line ("Definition"). This will be used to mine genes using G2PMiner, 
# 4. 'SeqLength_bp',
# 5. "Sequence".
PubMed2DNAinfo <- function(PubMed){
  #PubMed2Nuccore
  PubMedInNuc <- rentrez::entrez_link(dbfrom = "pubmed", id = PubMed, db = "nuccore")
  
  ###~~~
  #Fetch meta-data associated to sequences
  ###~~~
  
  #Use loop to automatically retrieve species, 
  # seq definition line, seq length and DNA sequence associated 
  # to each DNA accession
  
  #Create empty matrix to be populated
  OUT <- matrix(ncol=5, nrow=length(PubMedInNuc$links[[1]]))
  colnames(OUT) <- c("PubMedID","GenBankID", "Definition", 'SeqLength_bp',"Sequence")
  
  #Add PubMedID
  OUT[,1] <- rep(PubMed, nrow(OUT))
  #Add GenBank ID
  OUT[,2] <- PubMedInNuc$links[[1]]
  print("Processing sequences: fetching meta-data")
  #Set a progress bar
  pb <- txtProgressBar(min = 0, max = nrow(OUT), style = 3)
  for(i in 1:nrow(OUT)){
    #print(paste(i, "in", length(PubMedInNuc$ids), sep= ' '))
    #Download sequence
    seq <- rentrez::entrez_fetch(db='nuccore', id=OUT[i,2], rettype='fasta', retmode = "text")
    
    #Extract definition line
    OUT[i,3] <- strsplit(seq, split="\n")[[1]][1]
    
    #Infer seq length
    nbp <- strsplit(seq, split="\n")
    OUT[i,4] <- as.numeric(length(strsplit(paste(nbp[[1]][2:length(nbp[[1]])], 
                                                 collapse=''),"")[[1]]))
    
    #Extract sequence
    OUT[i,5] <- as.vector(paste(nbp[[1]][2:length(nbp[[1]])], collapse='')[1])
    # update progress bar
    setTxtProgressBar(pb, i)
  }
  close(pb)
  
  SEQ <- as.data.frame(OUT)
}


###
#Apply PubMed2DNAinfo function to DNAIDs
###

#Issue with i=1455
fileNames <- c("PubMed2Seq.csv", "PubMed2Seq_2.csv")
fileNames <- fileNames[2]

#If File exists where should we start 
if(file.exists(fileNames) == TRUE){
  #Open csv file
  csvPubMed <- read.csv(fileNames)
  #Identify last PubMed
  lastPub <- csvPubMed$PubMedID[nrow(csvPubMed)]
  #Identify i
  ToStart <- which(DNAIDs == lastPub)+1
}

pb <- txtProgressBar(min = 0, max = length(DNAIDs), style = 3)
for(i in ToStart:length(DNAIDs)){
#  for(i in 1:length(DNAIDs)){
  if(i == 1){
    #Check if the file exists and if T then exists loop
    if(file.exists(fileNames) == TRUE){
      print(paste(fileNames, "file already exists!", sep=""))
      break
    }else{
      #Download data
      tmp <- PubMed2DNAinfo(PubMed = DNAIDs[i])
      #Create csv and save output
      write.csv(tmp, fileNames, row.names = F) 
    }
  }else{
    #Check that the file exists if not then create it and tell the user
    if(file.exists(fileNames) == FALSE){
      print(paste(fileNames, " does not exist and will be created!", sep=""))
      #Download data
      tmp <- PubMed2DNAinfo(PubMed = DNAIDs[i])
      #Create csv and save output
      write.csv(tmp, fileNames, row.names = F) 
    }else{
      #Download data
      tmp <- PubMed2DNAinfo(PubMed = DNAIDs[i])
      #Open csv file
      csv <- read.csv(fileNames)
      #Save file with new rows
      write.csv(rbind(csv, tmp), fileNames, row.names = F)
    }
  }
  # update progress bar
  setTxtProgressBar(pb, i)
}
close(pb)

###~~~
#Compare PubMed ids from G2PMiner vs rentrez
###~~~

#Load Data from G2PMiner (matching for G and T)
G2P <- read.csv("GTConsensusIDs.csv")

#Subset PubMed to only contain PubMedID that have G and T

table(c(PubMed$x[which(as.numeric(PubMed$NGenBankAcc) > 0 & as.numeric(PubMed$NTaxa) > 0)], as.numeric(G2P$x)))





