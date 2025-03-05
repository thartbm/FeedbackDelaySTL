

doANOVA <- function() {
  
  
  df <- read.csv(file = 'data/main/STLaftereffects.csv',
                 stringsAsFactors = F)
  
  print(
    afex::aov_ez(
      id = 'participant',
      dv = 'aftereffect',
      data = df,
      within = c('rotation', 'delay'),
      type=3
    )
  )
  
  
}

widenData <- function() {
  
  df <- read.csv(file = 'data/main/STLaftereffects.csv',
                 stringsAsFactors = F)
  
  ndf <- reshape2::dcast(participant ~ delay + rotation, value.var = 'aftereffect', data=df)
  
  write.csv(ndf, 'data/main/feedbackDelayWIDE.csv', row.names=F)
  
}


