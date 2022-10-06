# G2PMineR_Supplemental_Scripts
### README ###

THIS README IS FOR ANALYSES DESCRIBED IN "Meta-analysis reveals challenges and gaps for genome-to-phenome research underpinning plant drought response." BY MELTON ET AL. 2022. PLEASE BEGIN BY DOWNLOADING THE 'G2PMINER' PACKAGE AND VIGNETTE SCRIPT FROM https://buerkilabteam.github.io/G2PMineR_Web/.

# 1. PERFORM THE G2PMINER ANLYSES
The vignette script will explain how to perform the literature search and create all standard outputs of the pipeline (taxa, genes, phenotypes, and plots). The "ConsensusBySpecies.R" script can be used to generate tables that summary statistics per species.

# (OPTIONAL) CHECK TAXA NAMES
After generating the 'consensus' abstract object, manually check species names to confirm all are the most up-to-date identities if you did not add an automated taxon name check step into the pipeline. This can be performed using the R package 'taxize'. If entries under outdated names are identified, the taxa, genes, and phenotypes tables will need to be updated to reflect proper taxonomy. 

# 1. G2PMINER (CONTINUED)
Next, generate figures summarizing the basic results of the 'G2PMineR' pipeline. The  inputs for this script are Table S2 and the 'consensus' object from the 'G2PMineR' pipeline.

# 2. SUMMARY STATISTICS
Scripts are provided to get some summary statistics, such as taxa per family or number of phenotypes per response strategy. Family for each species found in the G2PMineR pipeline can be found using 'taxize'. The "G2P_Figures.R" script will generate  The "PhenoWord_DroughtStrategy_Counts.R" script uses data from Table S6 and output  of the "G2P_Figures.R" script.

# 3. IDENTIFY PLANTS OF HUMAN USE PER KEW CHECKLIST
The "PlantsLackingHumanUse.R" script requires Table S2 as input. It will generate tables with species that lack human use and human use counts per species and family.

# 4. IDENTIFY PLANTS OF ARID ECOSYSTEMS
Occurrence data can be downloaded from public databases, such the Global Biodiversity Information Facility (GBIF; https://www.gbif.org/) and Integrated Digitized Biocollections  (iDigBio; https://www.idigbio.org/). The "GetOccurrenceData.R" script can be used to download occurrence data and perform a preliminary cleaning of data, including a step using an environmental raster as a reference to remove points that lack relevant environmental data. Once the data are downloaded, you can use the "GetGBIFKeys.R" and "GetGBIFOccurrences.R" scripts to get accession keys for all data from the GBIF repository. These should be used to generate a DOI for the data (https://www.gbif.org/derived-dataset/about).

# 5. ASSESS CONNECTIVITY TO TAXA AND SEQUENCE DATABASES
The "PubMed2GenesTaxo.R" and "MinePubMed2Genes.R" can be used to mine public databases for gene and taxa data to identify publications with associated data. The input for the "PubMed2GenesTaxo.R" is the list of PubMed Abstract IDs acquired using the 'G2PMineR' pipeline. The "MinePubMed2Genes.R" script will mine the headers of sequences downloaded using the "PubMed2GenesTaxo.R" script. Then, it will compare the gene list mined from headers to the gene list obtained from abstract mining the papers from which these sequences came from.
