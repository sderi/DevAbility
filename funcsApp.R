library(dplyr)
library(reshape2)
library(data.table)

dataw <- read.csv("data/tsCD38allDatav2.csv")

# Function to make a region-specific data frame
# Inputs(data = data frame, name = object name for output data frame, var = CD region - character)
# Output(data frame of CD-specific attributes)
cleandata <- function(data, name, var){
  name <- filter(data, Region == var)
  name <- name[colSums(!is.na(name)) >0]
  name[is.na(name)] <- 0
  return(name)
}

# Function to arragne data frame in wide format with option for log 2 normalization
# Inputs(df = data - data frame, region = CD region - character, norm = Bool logic for normalizition - True False)
# Output(data frame based on inputs)
cleanDF <- function(df, region, norm) {
  # input region and filter data frame
  fundf <- filter(df, Region == region)
  # drop columns containing all NAN's 
  fundf <- fundf[colSums(!is.na(fundf)) > 0]
  # Select only Time and modifications columns
  fundf <- fundf[,2:length(colnames(fundf))]
  # melt the data frame to long format to create new unqiue sample column
  fundf <- melt(fundf, id.vars = c("Construct", "Time", "Buffer"))
  # create a new column with a unique name for construct, buffer, modification
  fundf$Sample <- paste(fundf$Construct, fundf$Buffer, fundf$variable, sep="_")
  # select only "Time", "Value" and "Sample" columns
  fundf <- select(fundf, Time, value, Sample)
  # cast the data frame to wide format to allow looping over columns for calculations
  fundf <- dcast(fundf, Time ~ Sample, value.var="value")
  # replace NaN values with 0
  fundf[is.na(fundf)] <- 0
  
  if(norm == 1){
    ###### Normalize data with a log2 transformation ######
    fundf <- as.data.frame(apply(fundf, 2, function(x) log2(x/x[1])))
    fundf$Time <- c(0, 1, 2, 3)
    fundf[is.na(fundf)] <- 0
    fundf[] <- lapply(fundf, function(i) if(is.numeric(i)) ifelse(is.infinite(i), 0, i) else i)
  } else {
    fundf <- fundf
  }
  
  # melt df to remake Construct, Buffer, Attribute columns
  fundf <- melt(fundf, id.vars = c("Time"))
  # splitting fundf Sample vector to make Construct, Time, Buffer, Attribute columns
  samples <- as.character(fundf$variable)
  splitList <- strsplit(samples, "_", fixed=TRUE)
  mat <- as.data.frame(matrix(unlist(splitList), ncol=3, byrow=TRUE))
  data <- cbind(mat, fundf)
  setnames(data, old=c("V1", "V2", "V3"),
           new=c("Construct", "Buffer", "Attribute"))
  # Drop sample column
  data <- select(data, -starts_with("variable"))
  # cast back out to wide format to plot by columns
  data <- dcast(data, Construct + Buffer + Time ~ Attribute, value.var = "value")
  
  return(data)
}

# function to calculate slope of line and r-squared
# input df = a dataframe, region = character input of the region, 
# norm = logical parameter to enter normalize data function
#         0, F, FALSE = do not normalize raw data before calculating slope and R-squared
#         1, T or TRUE = Normalize raw data before calculating slope and R-squared


linReg <- function (df, region, norm) {
  
  ## create the data frame for the loop to calculate stats data frame
  
  # input region and filter data frame
  fundf <- filter(df, Region == region)
  # drop columns containing all NAN's 
  fundf <- fundf[colSums(!is.na(fundf)) > 0]
  # Select only Time and modifications columns
  fundf <- fundf[,2:length(colnames(fundf))]
  # melt the data frame to long format to create new unqiue sample column
  fundf <- melt(fundf, id.vars = c("Construct", "Time", "Buffer"))
  # create a new column with a unique name for construct, buffer, modification
  fundf$Sample <- paste(fundf$Construct, fundf$Buffer, fundf$variable, sep="_")
  # select only "Time", "Value" and "Sample" columns
  fundf <- select(fundf, Time, value, Sample)
  # cast the data frame to wide format to allow looping over columns for calculations
  fundf <- dcast(fundf, Time ~ Sample, value.var="value")
  # replace NaN values with 0
  fundf[is.na(fundf)] <- 0
  
  if(norm == 1){
    ###### Normalize data with a log2 transformation ######
    fundf <- as.data.frame(apply(fundf, 2, function(x) log2(x/x[1])))
    fundf$Time <- c(0, 1, 2, 3)
    fundf[is.na(fundf)] <- 0
    fundf[] <- lapply(fundf, function(i) if(is.numeric(i)) ifelse(is.infinite(i), 0, i) else i)
  } else {
    fundf <- fundf
  }
  
  ## create empty data frame to store slope and r-squared values
  linrgDF <- data.frame(Sample = as.character(),
                        Slope = as.double(),
                        R.squared = as.double())
  
  ## create list of column names to loop through
  cols <- colnames(fundf[,2:length(colnames(fundf))])
  
  ## loop function which calculates slope and r-squared values adding the values to a dataframe
  
  for (i in cols) {
    # change format of column name from character to an object for the lm function
    col <- fundf[,i]
    # Built in R function to calculate various parameters of linear regression analysis
    lin <- lm(col ~ Time, data = fundf)
    # extract r-squared value from lm results
    rsq <- summary(lin)$r.squared
    # extract slope value from lm function
    slp <- lin$coefficients[[2]]
    # create vector of results
    row <- c(Sample = i, Slope = slp, R.squared = rsq)
    # add vector as row to data frame
    #linrgDF <- rbind.data.frame(linrgDF, as.data.frame(t(row)))
    linrgDF <- rbind.data.frame(linrgDF, t(row), stringsAsFactors = F)
    # set numeric data types
    linrgDF$Slope <- as.numeric(linrgDF$Slope)
    linrgDF$R.squared <- as.numeric(linrgDF$R.squared)
    # convert NA's to 0
    linrgDF[is.na(linrgDF)] <- 0
  }
  
  # splitting linrgDF Sample vector to make Construct, Time, Buffer, Attribute columns
  samples <- as.character(linrgDF$Sample)
  splitList <- strsplit(samples, "_", fixed=TRUE)
  mat <- as.data.frame(matrix(unlist(splitList), ncol=3, byrow=TRUE))
  data <- cbind(mat, linrgDF)
  setnames(data, old=c("V1", "V2", "V3"),
           new=c("Construct", "Buffer", "Attribute"))
  # Drop sample column
  data <- select(data, -starts_with("Sample"))
  ## return final data frame containing sample names and corresponding slope and r-squared values
  return(data)
}

# Linear regression plots section ---------------------------------------------------------------

# Data cleaning section

#function for arranging the data to be plotted for linear regrssion analysis

cleanDF <- function(df, region, norm) {
  
  ## create the data frame for the loop to calculate stats data frame
  
  # input region and filter data frame
  fundf <- filter(df, Region == region)
  # drop columns containing all NAN's 
  fundf <- fundf[colSums(!is.na(fundf)) > 0]
  # Select only Time and modifications columns
  fundf <- fundf[,2:length(colnames(fundf))]
  # melt the data frame to long format to create new unqiue sample column
  fundf <- melt(fundf, id.vars = c("Construct", "Time", "Buffer"))
  # create a new column with a unique name for construct, buffer, modification
  fundf$Sample <- paste(fundf$Construct, fundf$Buffer, fundf$variable, sep="_")
  # select only "Time", "Value" and "Sample" columns
  fundf <- select(fundf, Time, value, Sample)
  # cast the data frame to wide format to allow looping over columns for calculations
  fundf <- dcast(fundf, Time ~ Sample, value.var="value")
  # replace NaN values with 0
  fundf[is.na(fundf)] <- 0
  
  if(norm == 1){
    ###### Normalize data with a log2 transformation ######
    fundf <- as.data.frame(apply(fundf, 2, function(x) log2(x/x[1])))
    fundf$Time <- c(0, 1, 2, 3)
    fundf[is.na(fundf)] <- 0
    fundf[] <- lapply(fundf, function(i) if(is.numeric(i)) ifelse(is.infinite(i), 0, i) else i)
  } else {
    fundf <- fundf
  }
  
  # melt df to remake Construct, Buffer, Attribute columns
  fundf <- melt(fundf, id.vars = c("Time"))
  # splitting fundf Sample vector to make Construct, Time, Buffer, Attribute columns
  samples <- as.character(fundf$variable)
  splitList <- strsplit(samples, "_", fixed=TRUE)
  mat <- as.data.frame(matrix(unlist(splitList), ncol=3, byrow=TRUE))
  data <- cbind(mat, fundf)
  setnames(data, old=c("V1", "V2", "V3"),
           new=c("Construct", "Buffer", "Attribute"))
  # Drop sample column
  data <- select(data, -starts_with("variable"))
  # cast back out to wide format to plot by columns
  data <- dcast(data, Construct + Buffer + Time ~ Attribute, value.var = "value")
  
  return(data)
}