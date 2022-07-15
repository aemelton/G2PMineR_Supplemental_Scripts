### AE Melton, 2022
# Generate a file with GBIF keys for creating a GBIF DOI

GetGBIFKeys <- function() {
  file.list <- list.files(pattern = "Raw_Occurrence_Data.csv") # Get list of raw data files
  keyCounts <- NULL
  
  # Need to check files to make sure it has data. Some species may not have GBIF data, so you need to skip those to keep the loop going
  for (i in 1:length(file.list)) { 
    if (file.info(file.list[i])$size < 10) {
      next
    } else {
    tmp.csv <- read.csv(file.list[i])
    gbif.data <- subset(tmp.csv, prov == "gbif") # Subset to exclude non-GBIF data
    tmp.keyCounts <- gbif.data %>% dplyr::count(key, sort = TRUE)
    keyCounts <- rbind(keyCounts, tmp.keyCounts)
    }
  }
write.csv(x = keyCounts, file = "GBIF_Keys.csv", row.names = F) # Save a csv with the keys. Use this file to generate a DOI through the GBIF website
}