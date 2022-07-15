### AE Melton, 2022
# Generate a file with GBIF derived occurrences

GetGBIFOccurrences <- function() {
  file.list <- list.files(pattern = "Raw_Occurrence_Data.csv") # Get list of raw data files
  OccDat <- NULL
  
  # Need to check files to make sure it has data. Some species may not have GBIF data, so you need to skip those to keep the loop going
  for (i in 1:length(file.list)) { 
    if (file.info(file.list[i])$size < 10) {
      next
    } else {
      tmp.csv <- read.csv(file.list[i])
      gbif.data <- subset(tmp.csv, prov == "gbif") # Subset to exclude non-GBIF data
      OccDat <- rbind(OccDat, gbif.data)
    }
  }
  write.csv(x = OccDat, file = "GBIF_Occurrences.csv", row.names = F) # Save the GBIF data separately.
}

