# --- author: Christian Panitz
# --- encoding: en_US.UTF-8
# --- R version: 4.0.3 (2020-10-10) -- "Bunny-Wunnies Freak Out"
# --- RStudio version: 2022.02.3
# --- script version: July 2022
# --- content: reading individual spectra and creating data frames for statistics & plots


# Header Parameters -------------------------------------------------------

# indices in spectrum data matrices
# Florida: single bin at driving frequency, at Oz
FLbin6 <- 15
FLbin857 <- 21
FLbin15 <- 36
FLbinChn <- c(75,81)
freqResFL <- 3/7

# Leipzig: mean in range of frequency bins (~ driving frequency +/- 0.1 Hz), at Oz & Iz
LEbin6 <- 15
LEbin857 <- 21
LEbin15 <- 36
LEbinChn <- c(28,29)
freqResLE <- 3/7



# Packages & Path Setting -------------------------------------------------

# load required libraries
library(here)

# set parent folder
parentFolder <- here()



# Creating Data Frame and Reading Data From Text Files For Stats ---------

# create data frame with participant ID, site, condition variables and ssvep amp
df <- data.frame(
  part = character(),
  site = factor(levels = c(1,2), labels = c("FL","LE")),
  freq = factor(levels = c(1,2,3), labels = c("6Hz","8.57Hz","15Hz")),
  mod = factor(levels = c(1,2), labels = c("box","sine")),
  ssvep = numeric()
)



# Reading Individual Spectral Data & Filling Stats Data Frame -------------

# set path for raw data (individual spectra) to read
fileFolder <- paste0(parentFolder,"/individualSpectra/commonPipeline/")

# for each condition, files in the folder are listed, values for participant ID,
# site, condition, as well as the ssvep amplitude at predefined frequencies & channels
list6box <- list.files(path = fileFolder, pattern = "6Hz_box")
for (i in 1:length(list6box)) {
  currData <- read.csv(paste0(fileFolder,list6box[i]), header = FALSE, sep = ",")
  if (substr(list6box[i],1,2) == "FL") {
    df[nrow(df)+1, 1:4] <- c(substr(list6box[i],1,5), substr(list6box[i],1,2), "6Hz", "box")
    df$ssvep[nrow(df)] <- mean(as.matrix(currData[FLbinChn,FLbin6]))
    if (exists("specMatFL6box")) {
      specMatFL6box <- rbind(specMatFL6box, colMeans(currData[FLbinChn,]))
    } else {
      specMatFL6box <- colMeans(currData[FLbinChn,])
    }
    if (exists("topoMatFL6box")) {
      topoMatFL6box <- cbind(topoMatFL6box, colMeans(t(currData[,FLbin6])))
    } else {
      topoMatFL6box <- colMeans(t(currData[,FLbin6]))
    }
  } else if (substr(list6box[i],1,2) == "LE") {
    df[nrow(df)+1, 1:4] <- c(substr(list6box[i],1,5), substr(list6box[i],1,2), "6Hz", "box")
    df$ssvep[nrow(df)] <- mean(as.matrix(currData[LEbinChn,LEbin6]))
    if (exists("specMatLE6box")) {
      specMatLE6box <- rbind(specMatLE6box, colMeans(currData[LEbinChn,]))
    } else {
      specMatLE6box <- colMeans(currData[LEbinChn,])
    }
    if (exists("topoMatLE6box")) {
      topoMatLE6box <- cbind(topoMatLE6box, colMeans(t(currData[,LEbin6])))
    } else {
      topoMatLE6box <- colMeans(t(currData[,LEbin6]))
    }
  }
}

list6sin <- list.files(path = fileFolder, pattern = "6Hz_sine")
for (i in 1:length(list6sin)) {
  currData <- read.csv(paste0(fileFolder,list6sin[i]), header = FALSE, sep = ",")
  if (substr(list6sin[i],1,2) == "FL") {
    df[nrow(df)+1, 1:4] <- c(substr(list6sin[i],1,5), substr(list6sin[i],1,2), "6Hz", "sine")
    df$ssvep[nrow(df)] <- mean(as.matrix(currData[FLbinChn,FLbin6]))
    if (exists("specMatFL6sin")) {
      specMatFL6sin <- rbind(specMatFL6sin, colMeans(currData[FLbinChn,]))
    } else {
      specMatFL6sin <- colMeans(currData[FLbinChn,])
    }
    if (exists("topoMatFL6sin")) {
      topoMatFL6sin <- cbind(topoMatFL6sin, colMeans(t(currData[,FLbin6])))
    } else {
      topoMatFL6sin <- colMeans(t(currData[,FLbin6]))
    }
  } else if (substr(list6sin[i],1,2) == "LE") {
    df[nrow(df)+1, 1:4] <- c(substr(list6sin[i],1,5), substr(list6sin[i],1,2), "6Hz", "sine")
    df$ssvep[nrow(df)] <- mean(as.matrix(currData[LEbinChn,LEbin6]))
    if (exists("specMatLE6sin")) {
      specMatLE6sin <- rbind(specMatLE6sin, colMeans(currData[LEbinChn,]))
    } else {
      specMatLE6sin <- colMeans(currData[LEbinChn,])
    }
    if (exists("topoMatLE6sin")) {
      topoMatLE6sin <- cbind(topoMatLE6sin, colMeans(t(currData[,LEbin6])))
    } else {
      topoMatLE6sin <- colMeans(t(currData[,LEbin6]))
    }
  }
}

list857box <- list.files(path = fileFolder, pattern = "8.57Hz_box")
for (i in 1:length(list857box)) {
  currData <- read.csv(paste0(fileFolder,list857box[i]), header = FALSE, sep = ",")
  if (substr(list857box[i],1,2) == "FL") {
    df[nrow(df)+1, 1:4] <- c(substr(list857box[i],1,5), substr(list857box[i],1,2), "8.57Hz", "box")
    df$ssvep[nrow(df)] <- mean(as.matrix(currData[FLbinChn,FLbin857]))
    if (exists("specMatFL857box")) {
      specMatFL857box <- rbind(specMatFL857box, colMeans(currData[FLbinChn,]))
    } else {
      specMatFL857box <- colMeans(currData[FLbinChn,])
    }
    if (exists("topoMatFL857box")) {
      topoMatFL857box <- cbind(topoMatFL857box, colMeans(t(currData[,FLbin857])))
    } else {
      topoMatFL857box <- colMeans(t(currData[,FLbin857]))
    }
  } else if (substr(list857box[i],1,2) == "LE") {
    df[nrow(df)+1, 1:4] <- c(substr(list857box[i],1,5), substr(list857box[i],1,2), "8.57Hz", "box")
    df$ssvep[nrow(df)] <- mean(as.matrix(currData[LEbinChn,LEbin857]))
    if (exists("specMatLE857box")) {
      specMatLE857box <- rbind(specMatLE857box, colMeans(currData[LEbinChn,]))
    } else {
      specMatLE857box <- colMeans(currData[LEbinChn,])
    }
    if (exists("topoMatLE857box")) {
      topoMatLE857box <- cbind(topoMatLE857box, colMeans(t(currData[,LEbin857])))
    } else {
      topoMatLE857box <- colMeans(t(currData[,LEbin857]))
    }
  }
}

list857sin <- list.files(path = fileFolder, pattern = "8.57Hz_sine")
for (i in 1:length(list857sin)) {
  currData <- read.csv(paste0(fileFolder,list857sin[i]), header = FALSE, sep = ",")
  if (substr(list857sin[i],1,2) == "FL") {
    df[nrow(df)+1, 1:4] <- c(substr(list857sin[i],1,5), substr(list857sin[i],1,2), "8.57Hz", "sine")
    df$ssvep[nrow(df)] <- mean(as.matrix(currData[FLbinChn,FLbin857]))
    if (exists("specMatFL857sin")) {
      specMatFL857sin <- rbind(specMatFL857sin, colMeans(currData[FLbinChn,]))
    } else {
      specMatFL857sin <- colMeans(currData[FLbinChn,])
    }
    if (exists("topoMatFL857sin")) {
      topoMatFL857sin <- cbind(topoMatFL857sin, colMeans(t(currData[,FLbin857])))
    } else {
      topoMatFL857sin <- colMeans(t(currData[,FLbin857]))
    }
  } else if (substr(list857sin[i],1,2) == "LE") {
    df[nrow(df)+1, 1:4] <- c(substr(list857sin[i],1,5), substr(list857sin[i],1,2), "8.57Hz", "sine")
    df$ssvep[nrow(df)] <- mean(as.matrix(currData[LEbinChn,LEbin857]))
    if (exists("specMatLE857sin")) {
      specMatLE857sin <- rbind(specMatLE857sin, colMeans(currData[LEbinChn,]))
    } else {
      specMatLE857sin <- colMeans(currData[LEbinChn,])
    }
    if (exists("topoMatLE857sin")) {
      topoMatLE857sin <- cbind(topoMatLE857sin, colMeans(t(currData[,LEbin857])))
    } else {
      topoMatLE857sin <- colMeans(t(currData[,LEbin857]))
    }
  }
}

list15box <- list.files(path = fileFolder, pattern = "15Hz_box")
for (i in 1:length(list15box)) {
  currData <- read.csv(paste0(fileFolder,list15box[i]), header = FALSE, sep = ",")
  if (substr(list15box[i],1,2) == "FL") {
    df[nrow(df)+1, 1:4] <- c(substr(list15box[i],1,5), substr(list15box[i],1,2), "15Hz", "box")
    df$ssvep[nrow(df)] <- mean(as.matrix(currData[FLbinChn,FLbin15]))
    if (exists("specMatFL15box")) {
      specMatFL15box <- rbind(specMatFL15box, colMeans(currData[FLbinChn,]))
    } else {
      specMatFL15box <- colMeans(currData[FLbinChn,])
    }
    if (exists("topoMatFL15box")) {
      topoMatFL15box <- cbind(topoMatFL15box, colMeans(t(currData[,FLbin15])))
    } else {
      topoMatFL15box <- colMeans(t(currData[,FLbin15]))
    }
  } else if (substr(list15box[i],1,2) == "LE") {
    df[nrow(df)+1, 1:4] <- c(substr(list15box[i],1,5), substr(list15box[i],1,2), "15Hz", "box")
    df$ssvep[nrow(df)] <- mean(as.matrix(currData[LEbinChn,LEbin15]))
    if (exists("specMatLE15box")) {
      specMatLE15box <- rbind(specMatLE15box, colMeans(currData[LEbinChn,]))
    } else {
      specMatLE15box <- colMeans(currData[LEbinChn,])
    }
    if (exists("topoMatLE15box")) {
      topoMatLE15box <- cbind(topoMatLE15box, colMeans(t(currData[,LEbin15])))
    } else {
      topoMatLE15box <- colMeans(t(currData[,LEbin15]))
    }
  }
}

list15sin <- list.files(path = fileFolder, pattern = "15Hz_sine")
for (i in 1:length(list15sin)) {
  currData <- read.csv(paste0(fileFolder,list15sin[i]), header = FALSE, sep = ",")
  if (substr(list15sin[i],1,2) == "FL") {
    df[nrow(df)+1, 1:4] <- c(substr(list15sin[i],1,5), substr(list15sin[i],1,2), "15Hz", "sine")
    df$ssvep[nrow(df)] <- mean(as.matrix(currData[FLbinChn,FLbin15]))
    if (exists("specMatFL15sin")) {
      specMatFL15sin <- rbind(specMatFL15sin, colMeans(currData[FLbinChn,]))
    } else {
      specMatFL15sin <- colMeans(currData[FLbinChn,])
    }
    if (exists("topoMatFL15sin")) {
      topoMatFL15sin <- cbind(topoMatFL15sin, colMeans(t(currData[,FLbin15])))
    } else {
      topoMatFL15sin <- colMeans(t(currData[,FLbin15]))
    }
  } else if (substr(list15sin[i],1,2) == "LE") {
    df[nrow(df)+1, 1:4] <- c(substr(list15sin[i],1,5), substr(list15sin[i],1,2), "15Hz", "sine")
    df$ssvep[nrow(df)] <- mean(as.matrix(currData[LEbinChn,LEbin15]))
    if (exists("specMatLE15sin")) {
      specMatLE15sin <- rbind(specMatLE15sin, colMeans(currData[LEbinChn,]))
    } else {
      specMatLE15sin <- colMeans(currData[LEbinChn,])
    }
    if (exists("topoMatLE15sin")) {
      topoMatLE15sin <- cbind(topoMatLE15sin, colMeans(t(currData[,LEbin15])))
    } else {
      topoMatLE15sin <- colMeans(t(currData[,LEbin15]))
    }
  }
}

# reading data finished



# Finishing Touches & Saving Stats Data Frame -----------------------------

# transform participant variable into factor and rename factor levels for site
df$part <- factor(df$part)
levels(df$site) <- c("Florida","Leipzig")

# z-standardize ssvep amplitudes into new variable, separately for sites
df$ssvepZ[df$site == "Florida"] <- (df$ssvep[df$site == "Florida"] - mean(df$ssvep[df$site == "Florida"])) / 
                                      sd(df$ssvep[df$site == "Florida"])
df$ssvepZ[df$site == "Leipzig"] <- (df$ssvep[df$site == "Leipzig"] - mean(df$ssvep[df$site == "Leipzig"])) / 
                                      sd(df$ssvep[df$site == "Leipzig"])

# save data frame in subflder for data frames
savename = paste0(parentFolder,"/dataframes/dfSSVEP_commPL.csv")
write.csv(x = df, savename, row.names = FALSE)



# Averaging Individual Spectra and Creating Data Frame for Plotting -------

# Averaging individual spectra, separately for and across sites
specAvgFL6box <- colMeans(specMatFL6box)
specAvgFL6sin <- colMeans(specMatFL6sin)
specAvgFL857box <- colMeans(specMatFL857box)
specAvgFL857sin <- colMeans(specMatFL857sin)
specAvgFL15box <- colMeans(specMatFL15box)
specAvgFL15sin <- colMeans(specMatFL15sin)
specAvgLE6box <- colMeans(specMatLE6box)
specAvgLE6sin <- colMeans(specMatLE6sin)
specAvgLE857box <- colMeans(specMatLE857box)
specAvgLE857sin <- colMeans(specMatLE857sin)
specAvgLE15box <- colMeans(specMatLE15box)
specAvgLE15sin <- colMeans(specMatLE15sin)
specAvg6box <- colMeans(rbind(specMatFL6box,specMatLE6box))
specAvg6sin <- colMeans(rbind(specMatFL6sin,specMatLE6sin))
specAvg857box <- colMeans(rbind(specMatFL857box,specMatLE857box))
specAvg857sin <- colMeans(rbind(specMatFL857sin,specMatLE857sin))
specAvg15box <- colMeans(rbind(specMatFL15box,specMatLE15box))
specAvg15sin <- colMeans(rbind(specMatFL15sin,specMatLE15sin))

# number of frequncy bins, separate for site
nrBinsFL = length(specAvgFL6box)
nrBinsLE = length(specAvgLE6box)

# Create data frame with factors site, driving frequency & modulation function, 
# as well as frequency bin (x axis) and spectral amplitude (y axis)
dfSpectra <- data.frame(
  site = factor(c(rep(1, 6*nrBinsFL), rep(2, 6*nrBinsLE)),
                  levels = c(1,2), labels = c("Florida","Leipzig")),
  freq = factor(c(rep(1, 2*nrBinsFL), rep(2, 2*nrBinsFL), rep(3, 2*nrBinsFL),
                  rep(1, 2*nrBinsLE), rep(2, 2*nrBinsLE), rep(3, 2*nrBinsLE)),
                levels = c(1,2,3), labels = c("6Hz", "8.57Hz", "15Hz")),
  mod = factor(c(rep(c(rep(1, nrBinsFL), rep(2, nrBinsFL)),3),
                 rep(c(rep(1, nrBinsLE), rep(2, nrBinsLE)),3)),
               levels = c(1,2), labels = c("box", "sine")),
  freqBin = c(rep(seq(0, (nrBinsFL-1) * freqResFL, freqResFL), 6),
              rep(seq(0, (nrBinsLE-1) * freqResLE, freqResLE), 6)),
  amp = c(specAvgFL6box, specAvgFL6sin, specAvgFL857box, specAvgFL857sin, specAvgFL15box, specAvgFL15sin,
          specAvgLE6box, specAvgLE6sin, specAvgLE857box, specAvgLE857sin, specAvgLE15box, specAvgLE15sin)
)

# Create data frame with factors driving frequency & modulation function, 
# as well as frequency bin (x axis) and spectral amplitude (y axis) - across sites
dfSpectraJoint <- data.frame(
  freq = factor(c(rep(1, 2*nrBinsFL), rep(2, 2*nrBinsFL), rep(3, 2*nrBinsFL)),
                levels = c(1,2,3), labels = c("6Hz", "8.57Hz", "15Hz")),
  mod = factor(c(rep(c(rep(1, nrBinsFL), rep(2, nrBinsFL)),3)),
               levels = c(1,2), labels = c("box", "sine")),
  freqBin = c(rep(seq(0, (nrBinsFL-1) * freqResFL, freqResFL), 6)),
  amp = c(specAvg6box, specAvg6sin, specAvg857box, specAvg857sin, specAvg15box, specAvg15sin)
)

# Save data frames
savename = paste0(parentFolder,"/dataframes/dfSpectra_commPL.csv")
write.csv(x = dfSpectra, savename, row.names = FALSE)

savename = paste0(parentFolder,"/dataframes/dfSpectra_commPL_jointSites.csv")
write.csv(x = dfSpectraJoint, savename, row.names = FALSE)



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
topoAvgFL6box <- rowMeans(topoMatFL6box)
topoAvgFL6sin <- rowMeans(topoMatFL6sin)
topoAvgFL857box <- rowMeans(topoMatFL857box)
topoAvgFL857sin <- rowMeans(topoMatFL857sin)
topoAvgFL15box <- rowMeans(topoMatFL15box)
topoAvgFL15sin <- rowMeans(topoMatFL15sin)
topoAvgLE6box <- rowMeans(topoMatLE6box)
topoAvgLE6sin <- rowMeans(topoMatLE6sin)
topoAvgLE857box <- rowMeans(topoMatLE857box)
topoAvgLE857sin <- rowMeans(topoMatLE857sin)
topoAvgLE15box <- rowMeans(topoMatLE15box)
topoAvgLE15sin <- rowMeans(topoMatLE15sin)

# number of electrodes, separate for site
nrChansFL = length(topoAvgFL6box)
nrChansLE = length(topoAvgLE6box)

# Create data frame with factors site, driving frequency & modulation function, 
# electrode name, x & y coordinates for plot & ssVEP amplitude
dfTopos <- data.frame(
  site = factor(c(rep(1, 6*nrChansFL), rep(2, 6*nrChansLE)),
                levels = c(1,2), labels = c("Florida","Leipzig")),
  freq = factor(c(rep(1, 2*nrChansFL), rep(2, 2*nrChansFL), rep(3, 2*nrChansFL),
                  rep(1, 2*nrChansLE), rep(2, 2*nrChansLE), rep(3, 2*nrChansLE)),
                levels = c(1,2,3), labels = c("6Hz", "8.57Hz", "15Hz")),
  mod = factor(c(rep(c(rep(1, nrChansFL), rep(2, nrChansFL)),3),
                 rep(c(rep(1, nrChansLE), rep(2, nrChansLE)),3)),
               levels = c(1,2), labels = c("box", "sine")),
  electrode = c(rep(chanLocsFL$name, 6), rep(chanLocsLE$name, 6)),
  x = c(rep(chanLocsFL$x, 6), rep(chanLocsLE$x, 6)),
  y = c(rep(chanLocsFL$y, 6), rep(chanLocsLE$y, 6)),
  amplitude = c(topoAvgFL6box, topoAvgFL6sin, topoAvgFL857box, topoAvgFL857sin, topoAvgFL15box, topoAvgFL15sin,
                topoAvgLE6box, topoAvgLE6sin, topoAvgLE857box, topoAvgLE857sin, topoAvgLE15box, topoAvgLE15sin)
)

# save topo dataframe
savename = paste0(parentFolder,"/dataframes/dfTopos_commPL.csv")
write.csv(x = dfTopos, savename, row.names = FALSE)