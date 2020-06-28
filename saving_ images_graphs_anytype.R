graphs_sug4 <- function(data,v=c())
{
  setwd('C:\\Users\\anand\\Desktop\\Praxis\\sem2\\R\\function of graphs' )
  if(is.data.frame(data))
  {
    if(is.null(v))
    {
      for (i in 1:ncol(data))
      {
        if(is.numeric(data[,i]))
        {
          png(paste(names(data)[i],".png", sep=""))
          par(mfrow=c(2,1))
          hist(data[,i],main = paste('Histogram of',names(data)[i]),xlab = names(data)[i],col = 'gold')
          boxplot(data[,i],main=paste('Box plot of',names(data)[i]),xlab = names(data)[i],col = 'black',horizontal = T)
          dev.off()
        }
        else
        {
          png(paste(names(data)[i],".png", sep=""))
          par(mfrow=c(1,2))
          barplot(table(data[,i]),main = paste('Bar plot of',names(data)[i]),xlab = names(data)[i],col = 'maroon')
          pie(table(data[,i]))
          dev.off()
        }
      }
    }
    else
    {
      for (i in v)
      {
        if(is.numeric(data[,i]))
        {
          png(paste(names(data)[i],".png", sep=""))
          par(mfrow=c(2,1))
          hist(data[,i],main = paste('Histogram of',names(data)[i]),xlab = names(data)[i],col = 'gold')
          boxplot(data[,i],main=paste('Box plot of',names(data)[i]),xlab = names(data)[i],col = 'black',horizontal = T)
          dev.off()
        }
        else
        {
          png(paste(names(data)[i],".png", sep=""))
          par(mfrow=c(1,2))
          barplot(table(data[,i]),main = paste('Bar plot of',names(data)[i]),xlab = names(data)[i],col = 'maroon')
          pie(table(data[,i]))
          dev.off()
        }
      }
    }
  }
  else
  {
    stop('Enter a data frame')
  }
  
}
