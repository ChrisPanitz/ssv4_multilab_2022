# --- author: Christian Panitz
# --- encoding: en_US.UTF-8
# --- R version: 4.0.3 (2020-10-10) -- "Bunny-Wunnies Freak Out"
# --- RStudio version: 2022.02.3
# --- script version: December 2022
# --- content: reading individual PLI data and creating data frames for statistics & plots (common pipeline)


# Header Parameters -------------------------------------------------------

# indices in spectrum data matrices

# because of sliding-window averaging (window length = 4 cycles of driving frequency)
# driving frequency (1f) is always Bin 5, 2f => 9, 3f => 14, ... bin(xf) = 1 + 4x

# Florida: single bin at driving frequency, at Oz & Iz
FLbin6 <- 5
FLbin857 <- 5
FLbin15 <- 5
FLbinChn <- c(75,81)
freqResFL <- 3/7

# Leipzig: single bin at driving frequency, at Oz & Iz
LEbin6 <- 5
LEbin857 <- 5
LEbin15 <- 5
LEbinChn <- c(28,29)
freqResLE <- 3/7



# Packages & Path Setting -------------------------------------------------

# load required libraries
library(here)

# set parent folder
parentFolder <- here()



# Creating Data Frame and Reading Data From Text Files For Stats ---------

# create data frame with participant ID, lab, condition variables and ssvep amp
df <- data.frame(
  part = character(),
  lab = factor(levels = c(1,2), labels = c("FL","LE")),
  freq = factor(levels = c(1,2,3), labels = c("6Hz","8.57Hz","15Hz")),
  mod = factor(levels = c(1,2), labels = c("square","sine")),
  pli = numeric()
)



# Reading Individual Spectral Data & Filling Stats Data Frame -------------

# set path for raw data (individual spectra) to read
fileFolder <- paste0(parentFolder,"/supplementaryData/phaseStability/")

# for each condition, files in the folder are listed, values for participant ID,
# lab, condition, as well as the ssvep amplitude at predefined frequencies & channels
list6sq <- list.files(path = fileFolder, pattern = "6Hz_square")
for (i in 1:length(list6sq)) {
  currData <- read.csv(paste0(fileFolder,list6sq[i]), header = FALSE, sep = ",")
  if (substr(list6sq[i],1,2) == "FL") {
    df[nrow(df)+1, 1:4] <- c(substr(list6sq[i],1,5), substr(list6sq[i],1,2), "6Hz", "square")
    df$pli[nrow(df)] <- mean(as.matrix(currData[FLbinChn,FLbin6]))
    # if (exists("specMatFL6sq")) {
    #   specMatFL6sq <- rbind(specMatFL6sq, colMeans(currData[FLbinChn,]))
    # } else {
    #   specMatFL6sq <- colMeans(currData[FLbinChn,])
    # }
    if (exists("topoMatFL6sq")) {
      topoMatFL6sq <- cbind(topoMatFL6sq, colMeans(t(currData[,FLbin6])))
    } else {
      topoMatFL6sq <- colMeans(t(currData[,FLbin6]))
    }
  } else if (substr(list6sq[i],1,2) == "LE") {
    df[nrow(df)+1, 1:4] <- c(substr(list6sq[i],1,5), substr(list6sq[i],1,2), "6Hz", "square")
    df$pli[nrow(df)] <- mean(as.matrix(currData[LEbinChn,LEbin6]))
    # if (exists("specMatLE6sq")) {
    #   specMatLE6sq <- rbind(specMatLE6sq, colMeans(currData[LEbinChn,]))
    # } else {
    #   specMatLE6sq <- colMeans(currData[LEbinChn,])
    # }
    if (exists("topoMatLE6sq")) {
      topoMatLE6sq <- cbind(topoMatLE6sq, colMeans(t(currData[,LEbin6])))
    } else {
      topoMatLE6sq <- colMeans(t(currData[,LEbin6]))
    }
  }
}

list6sin <- list.files(path = fileFolder, pattern = "6Hz_sine")
for (i in 1:length(list6sin)) {
  currData <- read.csv(paste0(fileFolder,list6sin[i]), header = FALSE, sep = ",")
  if (substr(list6sin[i],1,2) == "FL") {
    df[nrow(df)+1, 1:4] <- c(substr(list6sin[i],1,5), substr(list6sin[i],1,2), "6Hz", "sine")
    df$pli[nrow(df)] <- mean(as.matrix(currData[FLbinChn,FLbin6]))
    # if (exists("specMatFL6sin")) {
    #   specMatFL6sin <- rbind(specMatFL6sin, colMeans(currData[FLbinChn,]))
    # } else {
    #   specMatFL6sin <- colMeans(currData[FLbinChn,])
    # }
    if (exists("topoMatFL6sin")) {
      topoMatFL6sin <- cbind(topoMatFL6sin, colMeans(t(currData[,FLbin6])))
    } else {
      topoMatFL6sin <- colMeans(t(currData[,FLbin6]))
    }
  } else if (substr(list6sin[i],1,2) == "LE") {
    df[nrow(df)+1, 1:4] <- c(substr(list6sin[i],1,5), substr(list6sin[i],1,2), "6Hz", "sine")
    df$pli[nrow(df)] <- mean(as.matrix(currData[LEbinChn,LEbin6]))
    # if (exists("specMatLE6sin")) {
    #   specMatLE6sin <- rbind(specMatLE6sin, colMeans(currData[LEbinChn,]))
    # } else {
    #   specMatLE6sin <- colMeans(currData[LEbinChn,])
    # }
    if (exists("topoMatLE6sin")) {
      topoMatLE6sin <- cbind(topoMatLE6sin, colMeans(t(currData[,LEbin6])))
    } else {
      topoMatLE6sin <- colMeans(t(currData[,LEbin6]))
    }
  }
}

list857sq <- list.files(path = fileFolder, pattern = "8.57Hz_square")
for (i in 1:length(list857sq)) {
  currData <- read.csv(paste0(fileFolder,list857sq[i]), header = FALSE, sep = ",")
  if (substr(list857sq[i],1,2) == "FL") {
    df[nrow(df)+1, 1:4] <- c(substr(list857sq[i],1,5), substr(list857sq[i],1,2), "8.57Hz", "square")
    df$pli[nrow(df)] <- mean(as.matrix(currData[FLbinChn,FLbin857]))
    # if (exists("specMatFL857sq")) {
    #   specMatFL857sq <- rbind(specMatFL857sq, colMeans(currData[FLbinChn,]))
    # } else {
    #   specMatFL857sq <- colMeans(currData[FLbinChn,])
    # }
    if (exists("topoMatFL857sq")) {
      topoMatFL857sq <- cbind(topoMatFL857sq, colMeans(t(currData[,FLbin857])))
    } else {
      topoMatFL857sq <- colMeans(t(currData[,FLbin857]))
    }
  } else if (substr(list857sq[i],1,2) == "LE") {
    df[nrow(df)+1, 1:4] <- c(substr(list857sq[i],1,5), substr(list857sq[i],1,2), "8.57Hz", "square")
    df$pli[nrow(df)] <- mean(as.matrix(currData[LEbinChn,LEbin857]))
    # if (exists("specMatLE857sq")) {
    #   specMatLE857sq <- rbind(specMatLE857sq, colMeans(currData[LEbinChn,]))
    # } else {
    #   specMatLE857sq <- colMeans(currData[LEbinChn,])
    # }
    if (exists("topoMatLE857sq")) {
      topoMatLE857sq <- cbind(topoMatLE857sq, colMeans(t(currData[,LEbin857])))
    } else {
      topoMatLE857sq <- colMeans(t(currData[,LEbin857]))
    }
  }
}

list857sin <- list.files(path = fileFolder, pattern = "8.57Hz_sine")
for (i in 1:length(list857sin)) {
  currData <- read.csv(paste0(fileFolder,list857sin[i]), header = FALSE, sep = ",")
  if (substr(list857sin[i],1,2) == "FL") {
    df[nrow(df)+1, 1:4] <- c(substr(list857sin[i],1,5), substr(list857sin[i],1,2), "8.57Hz", "sine")
    df$pli[nrow(df)] <- mean(as.matrix(currData[FLbinChn,FLbin857]))
    # if (exists("specMatFL857sin")) {
    #   specMatFL857sin <- rbind(specMatFL857sin, colMeans(currData[FLbinChn,]))
    # } else {
    #   specMatFL857sin <- colMeans(currData[FLbinChn,])
    # }
    if (exists("topoMatFL857sin")) {
      topoMatFL857sin <- cbind(topoMatFL857sin, colMeans(t(currData[,FLbin857])))
    } else {
      topoMatFL857sin <- colMeans(t(currData[,FLbin857]))
    }
  } else if (substr(list857sin[i],1,2) == "LE") {
    df[nrow(df)+1, 1:4] <- c(substr(list857sin[i],1,5), substr(list857sin[i],1,2), "8.57Hz", "sine")
    df$pli[nrow(df)] <- mean(as.matrix(currData[LEbinChn,LEbin857]))
    # if (exists("specMatLE857sin")) {
    #   specMatLE857sin <- rbind(specMatLE857sin, colMeans(currData[LEbinChn,]))
    # } else {
    #   specMatLE857sin <- colMeans(currData[LEbinChn,])
    # }
    if (exists("topoMatLE857sin")) {
      topoMatLE857sin <- cbind(topoMatLE857sin, colMeans(t(currData[,LEbin857])))
    } else {
      topoMatLE857sin <- colMeans(t(currData[,LEbin857]))
    }
  }
}

list15sq <- list.files(path = fileFolder, pattern = "15Hz_square")
for (i in 1:length(list15sq)) {
  currData <- read.csv(paste0(fileFolder,list15sq[i]), header = FALSE, sep = ",")
  if (substr(list15sq[i],1,2) == "FL") {
    df[nrow(df)+1, 1:4] <- c(substr(list15sq[i],1,5), substr(list15sq[i],1,2), "15Hz", "square")
    df$pli[nrow(df)] <- mean(as.matrix(currData[FLbinChn,FLbin15]))
    # if (exists("specMatFL15sq")) {
    #   specMatFL15sq <- rbind(specMatFL15sq, colMeans(currData[FLbinChn,]))
    # } else {
    #   specMatFL15sq <- colMeans(currData[FLbinChn,])
    # }
    if (exists("topoMatFL15sq")) {
      topoMatFL15sq <- cbind(topoMatFL15sq, colMeans(t(currData[,FLbin15])))
    } else {
      topoMatFL15sq <- colMeans(t(currData[,FLbin15]))
    }
  } else if (substr(list15sq[i],1,2) == "LE") {
    df[nrow(df)+1, 1:4] <- c(substr(list15sq[i],1,5), substr(list15sq[i],1,2), "15Hz", "square")
    df$pli[nrow(df)] <- mean(as.matrix(currData[LEbinChn,LEbin15]))
    # if (exists("specMatLE15sq")) {
    #   specMatLE15sq <- rbind(specMatLE15sq, colMeans(currData[LEbinChn,]))
    # } else {
    #   specMatLE15sq <- colMeans(currData[LEbinChn,])
    # }
    if (exists("topoMatLE15sq")) {
      topoMatLE15sq <- cbind(topoMatLE15sq, colMeans(t(currData[,LEbin15])))
    } else {
      topoMatLE15sq <- colMeans(t(currData[,LEbin15]))
    }
  }
}

list15sin <- list.files(path = fileFolder, pattern = "15Hz_sine")
for (i in 1:length(list15sin)) {
  currData <- read.csv(paste0(fileFolder,list15sin[i]), header = FALSE, sep = ",")
  if (substr(list15sin[i],1,2) == "FL") {
    df[nrow(df)+1, 1:4] <- c(substr(list15sin[i],1,5), substr(list15sin[i],1,2), "15Hz", "sine")
    df$pli[nrow(df)] <- mean(as.matrix(currData[FLbinChn,FLbin15]))
    # if (exists("specMatFL15sin")) {
    #   specMatFL15sin <- rbind(specMatFL15sin, colMeans(currData[FLbinChn,]))
    # } else {
    #   specMatFL15sin <- colMeans(currData[FLbinChn,])
    # }
    if (exists("topoMatFL15sin")) {
      topoMatFL15sin <- cbind(topoMatFL15sin, colMeans(t(currData[,FLbin15])))
    } else {
      topoMatFL15sin <- colMeans(t(currData[,FLbin15]))
    }
  } else if (substr(list15sin[i],1,2) == "LE") {
    df[nrow(df)+1, 1:4] <- c(substr(list15sin[i],1,5), substr(list15sin[i],1,2), "15Hz", "sine")
    df$pli[nrow(df)] <- mean(as.matrix(currData[LEbinChn,LEbin15]))
    # if (exists("specMatLE15sin")) {
    #   specMatLE15sin <- rbind(specMatLE15sin, colMeans(currData[LEbinChn,]))
    # } else {
    #   specMatLE15sin <- colMeans(currData[LEbinChn,])
    # }
    if (exists("topoMatLE15sin")) {
      topoMatLE15sin <- cbind(topoMatLE15sin, colMeans(t(currData[,LEbin15])))
    } else {
      topoMatLE15sin <- colMeans(t(currData[,LEbin15]))
    }
  }
}

# reading data finished



# Finishing Touches & Saving Stats Data Frame -----------------------------

# transform participant variable into factor and rename factor levels for lab
df$part <- factor(df$part)
levels(df$lab) <- c("Florida","Leipzig")

# z-standardize pli values into new variable, separately for labs
df$pliZ[df$lab == "Florida"] <- (df$pli[df$lab == "Florida"] - mean(df$pli[df$lab == "Florida"])) / 
                                 sd(df$pli[df$lab == "Florida"])
df$pliZ[df$lab == "Leipzig"] <- (df$pli[df$lab == "Leipzig"] - mean(df$pli[df$lab == "Leipzig"])) / 
                                 sd(df$pli[df$lab == "Leipzig"])

# save data frame in subflder for data frames
savename = paste0(parentFolder,"/dataframes/supp_dfPLI.csv")
write.csv(x = df, savename, row.names = FALSE)



# Averaging Individual Spectra and Creating Data Frame for Plotting -------

# Averaging individual spectra, separately for and across labs
# specAvgFL6sq <- colMeans(specMatFL6sq)
# specAvgFL6sin <- colMeans(specMatFL6sin)
# specAvgFL857sq <- colMeans(specMatFL857sq)
# specAvgFL857sin <- colMeans(specMatFL857sin)
# specAvgFL15sq <- colMeans(specMatFL15sq)
# specAvgFL15sin <- colMeans(specMatFL15sin)
# specAvgLE6sq <- colMeans(specMatLE6sq)
# specAvgLE6sin <- colMeans(specMatLE6sin)
# specAvgLE857sq <- colMeans(specMatLE857sq)
# specAvgLE857sin <- colMeans(specMatLE857sin)
# specAvgLE15sq <- colMeans(specMatLE15sq)
# specAvgLE15sin <- colMeans(specMatLE15sin)
# specAvg6sq <- colMeans(rbind(specMatFL6sq,specMatLE6sq))
# specAvg6sin <- colMeans(rbind(specMatFL6sin,specMatLE6sin))
# specAvg857sq <- colMeans(rbind(specMatFL857sq,specMatLE857sq))
# specAvg857sin <- colMeans(rbind(specMatFL857sin,specMatLE857sin))
# specAvg15sq <- colMeans(rbind(specMatFL15sq,specMatLE15sq))
# specAvg15sin <- colMeans(rbind(specMatFL15sin,specMatLE15sin))

# number of frequncy bins, separate for lab
# nrBinsFL = length(specAvgFL6sq)
# nrBinsLE = length(specAvgLE6sq)

# Create data frame with factors lab, driving frequency & modulation function, 
# as well as frequency bin (x axis) and spectral amplitude (y axis)
# dfSpectra <- data.frame(
#   lab = factor(c(rep(1, 6*nrBinsFL), rep(2, 6*nrBinsLE)),
#                   levels = c(1,2), labels = c("Florida","Leipzig")),
#   freq = factor(c(rep(1, 2*nrBinsFL), rep(2, 2*nrBinsFL), rep(3, 2*nrBinsFL),
#                   rep(1, 2*nrBinsLE), rep(2, 2*nrBinsLE), rep(3, 2*nrBinsLE)),
#                 levels = c(1,2,3), labels = c("6Hz", "8.57Hz", "15Hz")),
#   mod = factor(c(rep(c(rep(1, nrBinsFL), rep(2, nrBinsFL)),3),
#                  rep(c(rep(1, nrBinsLE), rep(2, nrBinsLE)),3)),
#                levels = c(1,2), labels = c("square", "sine")),
#   freqBin = c(rep(seq(0, (nrBinsFL-1) * freqResFL, freqResFL), 6),
#               rep(seq(0, (nrBinsLE-1) * freqResLE, freqResLE), 6)),
#   amp = c(specAvgFL6sq, specAvgFL6sin, specAvgFL857sq, specAvgFL857sin, specAvgFL15sq, specAvgFL15sin,
#           specAvgLE6sq, specAvgLE6sin, specAvgLE857sq, specAvgLE857sin, specAvgLE15sq, specAvgLE15sin)
# )

# Create data frame with factors driving frequency & modulation function, 
# as well as frequency bin (x axis) and spectral amplitude (y axis) - across labs
# dfSpectraJoint <- data.frame(
#   freq = factor(c(rep(1, 2*nrBinsFL), rep(2, 2*nrBinsFL), rep(3, 2*nrBinsFL)),
#                 levels = c(1,2,3), labels = c("6Hz", "8.57Hz", "15Hz")),
#   mod = factor(c(rep(c(rep(1, nrBinsFL), rep(2, nrBinsFL)),3)),
#                levels = c(1,2), labels = c("square", "sine")),
#   freqBin = c(rep(seq(0, (nrBinsFL-1) * freqResFL, freqResFL), 6)),
#   amp = c(specAvg6sq, specAvg6sin, specAvg857sq, specAvg857sin, specAvg15sq, specAvg15sin)
# )

# Save data frames
# savename = paste0(parentFolder,"/dataframes/dfSpectra_commPL.csv")
# write.csv(x = dfSpectra, savename, row.names = FALSE)
# 
# savename = paste0(parentFolder,"/dataframes/dfSpectra_commPL_jointLabs.csv")
# write.csv(x = dfSpectraJoint, savename, row.names = FALSE)



# Averaging Amplitudes at Driving Frequencies for All Channels for --------
# Plotting Topographies

# Load channel locations and transform from 3D theta + radius into 2D x & y
# for plotting purposes
loadname <- paste0(parentFolder,"/channelLocations/chanLocs_egi129.txt")
chanLocsFL <- read.csv(loadname, sep = ";")
chanLocsFL$thetaRadian <- pi/180*chanLocsFL$theta
chanLocsFL$x <- chanLocsFL$radius*sin(chanLocsFL$thetaRadian)*200
chanLocsFL$y <- chanLocsFL$radius*cos(chanLocsFL$thetaRadian)*200

loadname <- paste0(parentFolder,"/channelLocations/chanLocs_biosemi64.txt")
chanLocsLE <- read.csv(loadname, sep = ";")
chanLocsLE$thetaRadian <- pi/180*chanLocsLE$theta
chanLocsLE$x <- chanLocsLE$radius*sin(chanLocsLE$thetaRadian)*200
chanLocsLE$y <- chanLocsLE$radius*cos(chanLocsLE$thetaRadian)*200

# Average individual amplitudes across whole sample
topoAvgFL6sq <- rowMeans(topoMatFL6sq)
topoAvgFL6sin <- rowMeans(topoMatFL6sin)
topoAvgFL857sq <- rowMeans(topoMatFL857sq)
topoAvgFL857sin <- rowMeans(topoMatFL857sin)
topoAvgFL15sq <- rowMeans(topoMatFL15sq)
topoAvgFL15sin <- rowMeans(topoMatFL15sin)
topoAvgLE6sq <- rowMeans(topoMatLE6sq)
topoAvgLE6sin <- rowMeans(topoMatLE6sin)
topoAvgLE857sq <- rowMeans(topoMatLE857sq)
topoAvgLE857sin <- rowMeans(topoMatLE857sin)
topoAvgLE15sq <- rowMeans(topoMatLE15sq)
topoAvgLE15sin <- rowMeans(topoMatLE15sin)

# number of electrodes, separate for lab
nrChansFL = length(topoAvgFL6sq)
nrChansLE = length(topoAvgLE6sq)

# Create data frame with factors lab, driving frequency & modulation function, 
# electrode name, x & y coordinates for plot & ssVEP amplitude
dfTopos <- data.frame(
  lab = factor(c(rep(1, 6*nrChansFL), rep(2, 6*nrChansLE)),
                levels = c(1,2), labels = c("Florida","Leipzig")),
  freq = factor(c(rep(1, 2*nrChansFL), rep(2, 2*nrChansFL), rep(3, 2*nrChansFL),
                  rep(1, 2*nrChansLE), rep(2, 2*nrChansLE), rep(3, 2*nrChansLE)),
                levels = c(1,2,3), labels = c("6Hz", "8.57Hz", "15Hz")),
  mod = factor(c(rep(c(rep(1, nrChansFL), rep(2, nrChansFL)),3),
                 rep(c(rep(1, nrChansLE), rep(2, nrChansLE)),3)),
               levels = c(1,2), labels = c("square", "sine")),
  electrode = c(rep(chanLocsFL$name, 6), rep(chanLocsLE$name, 6)),
  x = c(rep(chanLocsFL$x, 6), rep(chanLocsLE$x, 6)),
  y = c(rep(chanLocsFL$y, 6), rep(chanLocsLE$y, 6)),
  amplitude = c(topoAvgFL6sq, topoAvgFL6sin, topoAvgFL857sq, topoAvgFL857sin, topoAvgFL15sq, topoAvgFL15sin,
                topoAvgLE6sq, topoAvgLE6sin, topoAvgLE857sq, topoAvgLE857sin, topoAvgLE15sq, topoAvgLE15sin)
)

# save topo dataframe
savename = paste0(parentFolder,"/dataframes/supp_PLI_dfTopos.csv")
write.csv(x = dfTopos, savename, row.names = FALSE)