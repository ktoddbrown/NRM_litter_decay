defineEventIndex <- function(slopeType){
  slopeType <- as.character(slopeType)
  maxIndex <- length(slopeType)
  #trigger events if
  
  #mark begining of increasing slopes
  startIncreasing <- grepl('(increasing)|(peak)', slopeType) & #if we are currently increasing
    !grepl('(increasing)|(peak)', c(NA, NA, slopeType[-1*(0:-1 + maxIndex)])) & #but weren't previously
    !grepl('(increasing)|(peak)', c(NA, slopeType[-maxIndex])) & #but weren't previously
    (grepl('(increasing)|(peak)', c(slopeType[-1], NA)) | #and are in the next timestep
    grepl('(increasing)|(peak)', c(slopeType[-1:-2], NA, NA))) #for the next two timesteps
  
  #mark being of peaks not associated with increasing slopes
  # startPeak <- grepl('peak', slopeType) & 
  #   !grepl('(increasing)|(peak)', c(NA, slopeType[-maxIndex])) &
  #   grepl('(increasing)|(peak)', c(slopeType[-1], NA))
  
  
  #mark begining of valleys
  endValley <- grepl('valley', slopeType) & 
    !grepl('valley', c(NA, slopeType[-length(slopeType)])) &
    grepl('valley', c(slopeType[-1], NA))
  
  ans <- cumsum(startIncreasing | endValley)

  return(ans)
}
