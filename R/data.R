

# this function finds all participants in the target folder:
getParticipants <- function(path='data/main/') {
  
  directories <- list.dirs( path       = path,
                            recursive  = FALSE,
                            full.names = FALSE)
  
  return(directories)
  
}


# this function loads all participants' performance matrices from the target folder:
getAllParticipantData <- function(path='data/main/') {
  
  participants <- getParticipants( path = path )
  
  allData <- NA
  
  for (participant in participants) {
    
    filename <- sprintf('%s%s/%s_performance.csv', path, participant, participant)
    
    pdf <- read.csv( file             = filename,
                     stringsAsFactors = FALSE)    
    
    pdf$participant <- participant
    
    if (is.data.frame(allData)) {
      allData <- rbind(allData, pdf)
    } else {
      allData <- pdf
    }
    

  }
  
  return(allData)
  
}

# this function pre-processes all data from the participants in the target folder:
preprocessData <- function(path='data/main/') {
  
  df <- getAllParticipantData(path=path)
  
  participants <- unique(df$participant)
  
  prdf <- NA
  
  for (participant in participants) {
    
    pdf <- getSTLresponses(df[which(df$participant == participant),])
    
    pdf <- removeOutliers(pdf)
    
    pdf <- normalizeSTL(pdf)
    
    pdf <- aggregateSTL(pdf, FUN=median)
    
    if (is.data.frame(prdf)) {
      prdf <- rbind(prdf, pdf)
    } else {
      prdf <- pdf
    }
  }
  
  return(prdf)
  
}

getSTLresponses <- function(df) {
  
  idx <- which(df$label == 'stl-rotation')
  participant <- df$participant[1]
  
  rotation <- df$rotation[idx]
  aftereffect <- df$reachdeviation_deg[idx+1] - df$reachdeviation_deg[idx-1]
  delay <- df$delay[idx]
  
  ndf <- data.frame(rotation, aftereffect, delay)
  ndf$participant <- participant
  
  return(ndf)
  
  
}

removeOutliers <- function(df) {
  
  df <- df[which(abs(df$aftereffect) < 50),]
  
  return(df)
  
}

normalizeSTL <- function(df) {
  
  df$aftereffect[which(df$rotation > 0)] <- -1 * df$aftereffect[which(df$rotation > 0)]
  
  df$rotation[which(df$rotation < 0)] <- -1 * df$rotation[which(df$rotation < 0)]
  
  return(df)
  
}

aggregateSTL <- function(df, FUN=median) {
  
  
  df <- aggregate(aftereffect ~ rotation + delay + participant, data=df, FUN=FUN)
  
  return(df)
  
  
}

storeSTLpilot <- function() {
  
  df <- preprocessData(path = 'data/pilot/')
  write.csv( x         = df,
             file      = 'data/pilot/STLaftereffects.csv',
             quote     = FALSE,
             row.names = FALSE)
  
}

storeSTLmain <- function() {
  
  df <- preprocessData(path = 'data/main/')
  write.csv( x         = df,
             file      = 'data/main/STLaftereffects.csv',
             quote     = FALSE,
             row.names = FALSE)
  
}