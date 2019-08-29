saveXYZGraphs <- function(data, format, unit, line){
  axis <- c("x", "y", "z")
  # Open pdf connection
  pdf(paste("Graphs/", format, "XYZ Data.pdf"), onefile = TRUE)
  # for each axis
  for (i in 1:3) {
    # If we want a line on the graph
    if(line == FALSE){
      # Create scatter plot
      print(ggplot(data) + geom_point(aes(x=time, y = data[,i])) + labs(x="Time (s)", y=paste(paste(unit, "value on", toupper(axis[i]), "axis"))) + 
              ggtitle(paste(toupper(axis[i]), "Axis", unit, "value / Time Scatter Plot")))
    }else{
      # Create scatter plot
      print(ggplot(data, aes(x=time, y = data[,i])) + geom_point() + labs(x="Time (s)", y=paste(paste(unit, "value on", toupper(axis[i]), "axis"))) + 
              ggtitle(paste(toupper(axis[i]), "Axis", unit, "value / Time Scatter Plot")) + geom_line())
    }
  }
  dev.off()
}

cleanData <- function(data){
  # Calculate rolling average number
  avgNum <- floor(nrow(data) / 24) + 3
  index <- 1
  count <- 1
  # How many loops we need to average the whole dataset
  loop <- nrow(data) - (nrow(data) %% avgNum)
  # Create vectors and fill with 0
  avg.x <- replicate(loop / avgNum, 0)
  avg.y <- replicate(loop / avgNum, 0)
  avg.z <- replicate(loop / avgNum, 0)
  avg.time <- replicate(loop / avgNum, 0)
  for(i in 1:loop){
    # If the rolling agerage rumber is reached divide the sum to get the average
    if(count == avgNum){
      count <- 1
      avg.x[index] <- avg.x[index] / avgNum
      avg.y[index] <- avg.y[index] / avgNum
      avg.z[index] <- avg.z[index] / avgNum
      avg.time[index] <- avg.time[index] / avgNum
      index = index + 1
    }else{ # Sum the values
      avg.x[index] <- avg.x[index] + data$x[i]
      avg.y[index] <- avg.y[index] + data$y[i]
      avg.z[index] <- avg.z[index] + data$z[i]
      avg.time[index] <- avg.time[index] + data$time[i]
      count <- count + 1
    }
  }
  mag <- c()
  # Calculate the magnitude of each axis
  for(i in 1:length(avg.x)){
    x.2 <- avg.x[i] * avg.x[i]
    y.2 <- avg.y[i] * avg.y[i]
    z.2 <- avg.z[i] * avg.z[i]
    # Turn the G value into acceleration
    mag[i] <- ((sqrt(x.2 + z.2 + y.2)) - 1) * 9.81
  }
  # create a new dataframe with the new data
  avg.df <- data.frame(x = avg.x, y = avg.y, z = avg.z, time = avg.time, mag = mag)
  return(avg.df)
}

calculateJounce <- function(avg.df){
  # Get the min acceleration value
  minMag <- min(avg.df[,5])
  # Get the time at the min acceleration
  minTime <- avg.df[avg.df$mag == minMag,]$time
  # Get max acceleration
  maxMag <- max(avg.df[avg.df$time > minTime,]$mag)
  # Get the time fror the max acceleration
  maxTime <- avg.df[avg.df$mag == maxMag,]$time
  
  # Get a subset of the data points between the min and max time
  lowerPoints <- avg.df[avg.df$time <= maxTime,]
  interPoints <- lowerPoints[lowerPoints$time >= minTime,]
  
  # Calculate Jerk after caught by the rope
  jerkPoints <- c()
  for(i in 1:nrow(interPoints)-1){
    t <- interPoints[i + 1,]$time - interPoints[i,]$time
    a2 <- interPoints[i + 1,]$mag
    a1 <- interPoints[i,]$mag
    jerkPoints[i] <- (a2-a1)/t
  }
  jerkUp <- sum(jerkPoints)
  
  # Calculate Jerk before the rope catches
  bf <- avg.df[avg.df$time <= minTime,]
  i <- nrow(bf) -1
  bfT <- bf[i,]$time
  t <- bf[i + 1,]$time - bfT
  a2 <- bf[i + 1,]$mag
  a1 <- bf[i,]$mag
  jerkDown <- (a2-a1)/t
  
  # Calculate the Jounce 
  jounce <- (jerkUp - jerkDown) / (maxTime - bfT)
  return(jounce)
}

cleanAndCalculate <- function(df, save){
  library("ggplot2")
  
  # Clean the data
  avg.df <- cleanData(df)
  
  # if graphs should be saved
  if(save == TRUE){
    saveXYZGraphs(df, "Unclean", "G", FALSE)
    saveXYZGraphs(avg.df, "Smoothed", "G", TRUE)
    p <- ggplot(avg.df, aes(x=time, y = mag)) + geom_point() + geom_line() + labs(x="Time (s)", y="Acceleration (m/s/s)") + 
      ggtitle("Acceleration / Time Scatter plot")
    ggsave("Graphs/Smoothed Acceleration Data.pdf", p)
  }
  
  # Calculate the jounce
  jounce <- calculateJounce(avg.df)
  
  if(jounce <= 17){
    print("That was a soft catch")
  }else{
    print("That was a hard catch")
  }
}

# The directory of the dataset
filePath <- "Data/Real Data/fallLargeHard.csv"
# Save graphs to PDFs
saveGraphsToPFD <- TRUE

df <- read.csv(filePath, header = TRUE)
cleanAndCalculate(df, saveGraphsToPFD)
