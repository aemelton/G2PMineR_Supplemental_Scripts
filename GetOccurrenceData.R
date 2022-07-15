### AE Melton
# This function is used to query databases for occurrence data and then clean the data
# to get rid of unlikely or poor quality occurrences.

GetOccurrenceData <- function(usr.query, usr.db = c("gbif", "idigbio"), usr.how = "shortest"){
  for(i in 1:length(usr.query)){
    
	  spocc <- occ(query = as.character(usr.query[i]), from = usr.db, has_coords = T, limit = 100,000)
	  raw.df <- occ2df(spocc)
	  output.file <- paste0(as.character(usr.query[i]), "_Raw_Occurrence_Data.csv")
	  output.file <- gsub(pattern = " ", replacement = "_", x = output.file)
	  write.csv(x = raw.df, file = output.file, row.names = F)
	
	  if(nrow(as.data.frame(spocc$gbif$data)) > 0 | nrow(as.data.frame(spocc$idigbio$data)) > 0){
	    
	    allSpocc <- occ2df(spocc)
	    
	    allspocc <- scrubr::fix_names(allSpocc, how = usr.how)$name
	
	    allSpocc <- date_standardize(allSpocc, "%d%b%Y")

	    scrubbed <- coord_incomplete(allSpocc) # Removes data with incomplete coordinates
	
	    scrubbed <- coord_unlikely(scrubbed) # Removes data with unlikely coordinates (i.e. 0,0)

	    scrubbed <- scrubbed[,-6] # Removes unique, database-specific keys
	
	    unique <- unique(scrubbed[,-4]) # Removes duplicate points (not considering data provider)
	  
	    output.file <- paste0(as.character(usr.query[i]), "_PrelimQCd_Data.csv")
	    output.file <- gsub(pattern = " ", replacement = "_", x = output.file)
	  
	    write.csv(unique[,1:4], file = output.file, row.names = F)
	    
  	}else{
	    next
	  }
	}
}
