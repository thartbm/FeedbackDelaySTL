
getColors <- function() {
  
  cols.op <- c(rgb(255, 147, 41,  255, max = 255), # orange:  21, 255, 148
               rgb(229, 22,  54,  255, max = 255), # red:    248, 210, 126
               rgb(207, 0,   216, 255, max = 255), # pink:   211, 255, 108
               rgb(127, 0,   216, 255, max = 255), # violet: 195, 255, 108
               rgb(0,   19,  136, 255, max = 255)) # blue:   164, 255, 68
  
  cols.tr <- c(rgb(255, 147, 41,  32,  max = 255), # orange:  21, 255, 148
               rgb(229, 22,  54,  32,  max = 255), # red:    248, 210, 126
               rgb(207, 0,   216, 32,  max = 255), # pink:   211, 255, 108
               rgb(127, 0,   216, 32,  max = 255), # violet: 195, 255, 108
               rgb(0,   19,  136, 32,  max = 255)) # blue:   164, 255, 68
  
  cols <- list()
  cols$op <- cols.op
  cols$tr <- cols.tr
  
  return(cols)
  
}


plotData <- function() {

  df <- read.csv(file             = 'data/main/STLaftereffects.csv',
                 stringsAsFactors = FALSE)
  
  reg45 <- read.csv( file              = 'data/pointTargetSTL_45.csv',
                     stringsAsFactors = FALSE)
  
  # rotation     response superblock participant target
  
  reg45 <- aggregate(response ~ participant + rotation, FUN=mean, data=reg45)
  
  colors <- getColors()

  plot(-1000,-1000,
       main='', xlab='rotation [deg]', ylab='aftereffect [deg]',
       xlim=c(-1,max(df$rotation)+1),ylim=c(-2,6),
       bty='n',ax=F)

  lines(x=c(-0.5,45.5),y=c(0,0),lty=2,col='#000000',lw=1.5)
  
  
  avg <- aggregate(response ~ rotation, data=reg45, FUN=mean)
  CI  <- aggregate(response ~ rotation, data=reg45, FUN=Reach::getConfidenceInterval)
  
  polygon( x      = c(CI$rotation, rev(CI$rotation)),
           y      = c(CI$response[,1], rev(CI$response[,2])),
           col    = '#99999933', 
           border = NA,
           xpd=TRUE)
  
  lines(x=avg$rotation, y=avg$response, col='#999999')
  
  

  delays = sort(unique(df$delay))
  
  for (dn in c(1:length(delays))) {
  # for (dn in c(1,length(delays))) {
    
    delay = delays[dn]
    
    ddf <- df[which(df$delay == delay),]
    
    avg <- aggregate(aftereffect ~ rotation, data=ddf, FUN=mean)
    CI  <- aggregate(aftereffect ~ rotation, data=ddf, FUN=Reach::getConfidenceInterval)
    
    polygon( x      = c(CI$rotation, rev(CI$rotation)),
             y      = c(CI$aftereffect[,1], rev(CI$aftereffect[,2])),
             col    = colors$tr[dn], 
             border = NA,
             xpd=TRUE)
    
    lines(x=avg$rotation, y=avg$aftereffect, col=colors$op[dn])
    
     # print(CI$aftereffect[,1])
  }
  
  axis( side = 1,
        at = c(0,unique(df$rotation)))
  axis( side = 2,
        at = seq(-2,6,2))
  
  legend( x = -1,
          y = 7,
          title='delay:',
          legend = sprintf('%0.1f s', delays),
          lty=1,
          # seg.len = 1.5,
          col=colors$op,
          bty='n',
          xpd=TRUE)
  
}