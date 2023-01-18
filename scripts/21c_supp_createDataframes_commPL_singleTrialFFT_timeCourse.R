# --- author: Christian Panitz
# --- encoding: en_US.UTF-8
# --- R version: 4.0.3 (2020-10-10) -- "Bunny-Wunnies Freak Out"
# --- RStudio version: 2022.02.3
# --- script version: December 2022
# --- content: reading individual single-trial amplitudes and creating data frames for statistics & plots (common pipeline, single-trial FFT, single-trial SNR)


# Header Parameters -------------------------------------------------------
avgWin <- 11 # number of trials for moving average window

# indices in spectrum data matrices

# Florida: single bin at driving frequency, at Oz & Iz
FLbinChn <- c(75,81)

# Leipzig: single bin at driving frequency, at Oz & Iz
LEbinChn <- c(28,29)

freqRes6 <- 6/4;
freqRes857 <- 60/7/4;
freqRes15 <- 15/4;


# Packages & Path Setting -------------------------------------------------

# load required libraries
library(here)
library(ggplot2)
library(RColorBrewer)
library(ggpubr)

# set parent folder
parentFolder <- here()



# Reading Individual Time Courses of Spectral Amplitude --------------------

# set path for raw data (individual spectra) to read
fileFolder <- paste0(parentFolder,"/supplementaryData/singleTrialCourse/")

# for each condition, files in the folder are listed, values for participant ID,
# lab, condition, as well as the spectral amplitude at predefined channels
# (raw data only contains driving frequencies)
list6sqAmp <- list.files(path = fileFolder, pattern = "6Hz_square_amp")
for (i in 1:length(list6sqAmp)) {
  currData <- read.csv(paste0(fileFolder,list6sqAmp[i]), header = FALSE, sep = ",")
  if (substr(list6sqAmp[i],1,2) == "FL") {
    if (exists("trialMatFL6sqAmp")) {
      trialMatFL6sqAmp <- rbind(trialMatFL6sqAmp, colMeans(currData[FLbinChn,]))
    } else {
      trialMatFL6sqAmp <- colMeans(currData[FLbinChn,])
    }
  } else if (substr(list6sqAmp[i],1,2) == "LE") {
    if (exists("trialMatLE6sqAmp")) {
      trialMatLE6sqAmp <- rbind(trialMatLE6sqAmp, colMeans(currData[LEbinChn,]))
    } else {
      trialMatLE6sqAmp <- colMeans(currData[LEbinChn,])
    }
  }
}

list6sinAmp <- list.files(path = fileFolder, pattern = "6Hz_sine_amp")
for (i in 1:length(list6sinAmp)) {
  currData <- read.csv(paste0(fileFolder,list6sinAmp[i]), header = FALSE, sep = ",")
  if (substr(list6sinAmp[i],1,2) == "FL") {
    if (exists("trialMatFL6sinAmp")) {
      trialMatFL6sinAmp <- rbind(trialMatFL6sinAmp, colMeans(currData[FLbinChn,]))
    } else {
      trialMatFL6sinAmp <- colMeans(currData[FLbinChn,])
    }
  } else if (substr(list6sinAmp[i],1,2) == "LE") {
    if (exists("trialMatLE6sinAmp")) {
      trialMatLE6sinAmp <- rbind(trialMatLE6sinAmp, colMeans(currData[LEbinChn,]))
    } else {
      trialMatLE6sinAmp <- colMeans(currData[LEbinChn,])
    }
  }
}

list857sqAmp <- list.files(path = fileFolder, pattern = "8.57Hz_square_amp")
for (i in 1:length(list857sqAmp)) {
  currData <- read.csv(paste0(fileFolder,list857sqAmp[i]), header = FALSE, sep = ",")
  if (substr(list857sqAmp[i],1,2) == "FL") {
    if (exists("trialMatFL857sqAmp")) {
      trialMatFL857sqAmp <- rbind(trialMatFL857sqAmp, colMeans(currData[FLbinChn,]))
    } else {
      trialMatFL857sqAmp <- colMeans(currData[FLbinChn,])
    }
  } else if (substr(list857sqAmp[i],1,2) == "LE") {
    if (exists("trialMatLE857sqAmp")) {
      trialMatLE857sqAmp <- rbind(trialMatLE857sqAmp, colMeans(currData[LEbinChn,]))
    } else {
      trialMatLE857sqAmp <- colMeans(currData[LEbinChn,])
    }
  }
}

list857sinAmp <- list.files(path = fileFolder, pattern = "8.57Hz_sine_amp")
for (i in 1:length(list857sinAmp)) {
  currData <- read.csv(paste0(fileFolder,list857sinAmp[i]), header = FALSE, sep = ",")
  if (substr(list857sinAmp[i],1,2) == "FL") {
    if (exists("trialMatFL857sinAmp")) {
      trialMatFL857sinAmp <- rbind(trialMatFL857sinAmp, colMeans(currData[FLbinChn,]))
    } else {
      trialMatFL857sinAmp <- colMeans(currData[FLbinChn,])
    }
  } else if (substr(list857sinAmp[i],1,2) == "LE") {
    if (exists("trialMatLE857sinAmp")) {
      trialMatLE857sinAmp <- rbind(trialMatLE857sinAmp, colMeans(currData[LEbinChn,]))
    } else {
      trialMatLE857sinAmp <- colMeans(currData[LEbinChn,])
    }
  }
}

list15sqAmp <- list.files(path = fileFolder, pattern = "15Hz_square_amp")
for (i in 1:length(list15sqAmp)) {
  currData <- read.csv(paste0(fileFolder,list15sqAmp[i]), header = FALSE, sep = ",")
  if (substr(list15sqAmp[i],1,2) == "FL") {
    if (exists("trialMatFL15sqAmp")) {
      trialMatFL15sqAmp <- rbind(trialMatFL15sqAmp, colMeans(currData[FLbinChn,]))
    } else {
      trialMatFL15sqAmp <- colMeans(currData[FLbinChn,])
    }
  } else if (substr(list15sqAmp[i],1,2) == "LE") {
    if (exists("trialMatLE15sqAmp")) {
      trialMatLE15sqAmp <- rbind(trialMatLE15sqAmp, colMeans(currData[LEbinChn,]))
    } else {
      trialMatLE15sqAmp <- colMeans(currData[LEbinChn,])
    }
  }
}

list15sinAmp <- list.files(path = fileFolder, pattern = "15Hz_sine_amp")
for (i in 1:length(list15sinAmp)) {
  currData <- read.csv(paste0(fileFolder,list15sinAmp[i]), header = FALSE, sep = ",")
  if (substr(list15sinAmp[i],1,2) == "FL") {
    if (exists("trialMatFL15sinAmp")) {
      trialMatFL15sinAmp <- rbind(trialMatFL15sinAmp, colMeans(currData[FLbinChn,]))
    } else {
      trialMatFL15sinAmp <- colMeans(currData[FLbinChn,])
    }
  } else if (substr(list15sinAmp[i],1,2) == "LE") {
    if (exists("trialMatLE15sinAmp")) {
      trialMatLE15sinAmp <- rbind(trialMatLE15sinAmp, colMeans(currData[LEbinChn,]))
    } else {
      trialMatLE15sinAmp <- colMeans(currData[LEbinChn,])
    }
  }
}

# reading spectral amplitude data finished



# Averaging Individual Time Courses ----------------------------------------

# Averaging individual spectra, separately for labs
trialAvgFL6sqAmp <- colMeans(trialMatFL6sqAmp, na.rm = TRUE)
trialAvgFL6sinAmp <- colMeans(trialMatFL6sinAmp, na.rm = TRUE)
trialAvgFL857sqAmp <- colMeans(trialMatFL857sqAmp, na.rm = TRUE)
trialAvgFL857sinAmp <- colMeans(trialMatFL857sinAmp, na.rm = TRUE)
trialAvgFL15sqAmp <- colMeans(trialMatFL15sqAmp, na.rm = TRUE)
trialAvgFL15sinAmp <- colMeans(trialMatFL15sinAmp, na.rm = TRUE)
trialAvgLE6sqAmp <- colMeans(trialMatLE6sqAmp, na.rm = TRUE)
trialAvgLE6sinAmp <- colMeans(trialMatLE6sinAmp, na.rm = TRUE)
trialAvgLE857sqAmp <- colMeans(trialMatLE857sqAmp, na.rm = TRUE)
trialAvgLE857sinAmp <- colMeans(trialMatLE857sinAmp, na.rm = TRUE)
trialAvgLE15sqAmp <- colMeans(trialMatLE15sqAmp, na.rm = TRUE)
trialAvgLE15sinAmp <- colMeans(trialMatLE15sinAmp, na.rm = TRUE)

# moving average on time series, padded with first and last value of the series, respectively
trialAvgFL6sqAmp_padded <- c(rep(trialAvgFL6sqAmp[1],floor(avgWin/2)), trialAvgFL6sqAmp, 
                          rep(trialAvgFL6sqAmp[length(trialAvgFL6sqAmp)],floor(avgWin/2)))
trialAvgFL6sqAmp_smoothed <- stats::filter(trialAvgFL6sqAmp_padded, rep(1/avgWin,avgWin))
trialAvgFL6sqAmp_smoothed <- trialAvgFL6sqAmp_smoothed[(1+floor(avgWin/2)) : (length(trialAvgFL6sqAmp_smoothed)-floor(avgWin/2))]

trialAvgFL6sinAmp_padded <- c(rep(trialAvgFL6sinAmp[1],floor(avgWin/2)), trialAvgFL6sinAmp, 
                           rep(trialAvgFL6sinAmp[length(trialAvgFL6sinAmp)],floor(avgWin/2)))
trialAvgFL6sinAmp_smoothed <- stats::filter(trialAvgFL6sinAmp_padded, rep(1/avgWin,avgWin))
trialAvgFL6sinAmp_smoothed <- trialAvgFL6sinAmp_smoothed[(1+floor(avgWin/2)) : (length(trialAvgFL6sinAmp_smoothed)-floor(avgWin/2))]

trialAvgFL857sqAmp_padded <- c(rep(trialAvgFL857sqAmp[1],floor(avgWin/2)), trialAvgFL857sqAmp, 
                            rep(trialAvgFL857sqAmp[length(trialAvgFL857sqAmp)],floor(avgWin/2)))
trialAvgFL857sqAmp_smoothed <- stats::filter(trialAvgFL857sqAmp_padded, rep(1/avgWin,avgWin))
trialAvgFL857sqAmp_smoothed <- trialAvgFL857sqAmp_smoothed[(1+floor(avgWin/2)) : (length(trialAvgFL857sqAmp_smoothed)-floor(avgWin/2))]

trialAvgFL857sinAmp_padded <- c(rep(trialAvgFL857sinAmp[1],floor(avgWin/2)), trialAvgFL857sinAmp, 
                             rep(trialAvgFL857sinAmp[length(trialAvgFL857sinAmp)],floor(avgWin/2)))
trialAvgFL857sinAmp_smoothed <- stats::filter(trialAvgFL857sinAmp_padded, rep(1/avgWin,avgWin))
trialAvgFL857sinAmp_smoothed <- trialAvgFL857sinAmp_smoothed[(1+floor(avgWin/2)) : (length(trialAvgFL857sinAmp_smoothed)-floor(avgWin/2))]

trialAvgFL15sqAmp_padded <- c(rep(trialAvgFL15sqAmp[1],floor(avgWin/2)), trialAvgFL15sqAmp, 
                           rep(trialAvgFL15sqAmp[length(trialAvgFL15sqAmp)],floor(avgWin/2)))
trialAvgFL15sqAmp_smoothed <- stats::filter(trialAvgFL15sqAmp_padded, rep(1/avgWin,avgWin))
trialAvgFL15sqAmp_smoothed <- trialAvgFL15sqAmp_smoothed[(1+floor(avgWin/2)) : (length(trialAvgFL15sqAmp_smoothed)-floor(avgWin/2))]

trialAvgFL15sinAmp_padded <- c(rep(trialAvgFL15sinAmp[1],floor(avgWin/2)), trialAvgFL15sinAmp, 
                            rep(trialAvgFL15sinAmp[length(trialAvgFL15sinAmp)],floor(avgWin/2)))
trialAvgFL15sinAmp_smoothed <- stats::filter(trialAvgFL15sinAmp_padded, rep(1/avgWin,avgWin))
trialAvgFL15sinAmp_smoothed <- trialAvgFL15sinAmp_smoothed[(1+floor(avgWin/2)) : (length(trialAvgFL15sinAmp_smoothed)-floor(avgWin/2))]

trialAvgLE6sqAmp_padded <- c(rep(trialAvgLE6sqAmp[1],floor(avgWin/2)), trialAvgLE6sqAmp, 
                          rep(trialAvgLE6sqAmp[length(trialAvgLE6sqAmp)],floor(avgWin/2)))
trialAvgLE6sqAmp_smoothed <- stats::filter(trialAvgLE6sqAmp_padded, rep(1/avgWin,avgWin))
trialAvgLE6sqAmp_smoothed <- trialAvgLE6sqAmp_smoothed[(1+floor(avgWin/2)) : (length(trialAvgLE6sqAmp_smoothed)-floor(avgWin/2))]

trialAvgLE6sinAmp_padded <- c(rep(trialAvgLE6sinAmp[1],floor(avgWin/2)), trialAvgLE6sinAmp, 
                           rep(trialAvgLE6sinAmp[length(trialAvgLE6sinAmp)],floor(avgWin/2)))
trialAvgLE6sinAmp_smoothed <- stats::filter(trialAvgLE6sinAmp_padded, rep(1/avgWin,avgWin))
trialAvgLE6sinAmp_smoothed <- trialAvgLE6sinAmp_smoothed[(1+floor(avgWin/2)) : (length(trialAvgLE6sinAmp_smoothed)-floor(avgWin/2))]

trialAvgLE857sqAmp_padded <- c(rep(trialAvgLE857sqAmp[1],floor(avgWin/2)), trialAvgLE857sqAmp, 
                            rep(trialAvgLE857sqAmp[length(trialAvgLE857sqAmp)],floor(avgWin/2)))
trialAvgLE857sqAmp_smoothed <- stats::filter(trialAvgLE857sqAmp_padded, rep(1/avgWin,avgWin))
trialAvgLE857sqAmp_smoothed <- trialAvgLE857sqAmp_smoothed[(1+floor(avgWin/2)) : (length(trialAvgLE857sqAmp_smoothed)-floor(avgWin/2))]

trialAvgLE857sinAmp_padded <- c(rep(trialAvgLE857sinAmp[1],floor(avgWin/2)), trialAvgLE857sinAmp, 
                             rep(trialAvgLE857sinAmp[length(trialAvgLE857sinAmp)],floor(avgWin/2)))
trialAvgLE857sinAmp_smoothed <- stats::filter(trialAvgLE857sinAmp_padded, rep(1/avgWin,avgWin))
trialAvgLE857sinAmp_smoothed <- trialAvgLE857sinAmp_smoothed[(1+floor(avgWin/2)) : (length(trialAvgLE857sinAmp_smoothed)-floor(avgWin/2))]

trialAvgLE15sqAmp_padded <- c(rep(trialAvgLE15sqAmp[1],floor(avgWin/2)), trialAvgLE15sqAmp, 
                           rep(trialAvgLE15sqAmp[length(trialAvgLE15sqAmp)],floor(avgWin/2)))
trialAvgLE15sqAmp_smoothed <- stats::filter(trialAvgLE15sqAmp_padded, rep(1/avgWin,avgWin))
trialAvgLE15sqAmp_smoothed <- trialAvgLE15sqAmp_smoothed[(1+floor(avgWin/2)) : (length(trialAvgLE15sqAmp_smoothed)-floor(avgWin/2))]

trialAvgLE15sinAmp_padded <- c(rep(trialAvgLE15sinAmp[1],floor(avgWin/2)), trialAvgLE15sinAmp, 
                            rep(trialAvgLE15sinAmp[length(trialAvgLE15sinAmp)],floor(avgWin/2)))
trialAvgLE15sinAmp_smoothed <- stats::filter(trialAvgLE15sinAmp_padded, rep(1/avgWin,avgWin))
trialAvgLE15sinAmp_smoothed <- trialAvgLE15sinAmp_smoothed[(1+floor(avgWin/2)) : (length(trialAvgLE15sinAmp_smoothed)-floor(avgWin/2))]



# Reading Individual Time Courses of SNR -------------------------------------

# set path for raw data (individual spectra) to read
fileFolder <- paste0(parentFolder,"/supplementaryData/singleTrialCourse/")

# create data frame with participant ID, lab, condition variables and average SNR
df <- data.frame(
  part = character(),
  lab = factor(levels = c(1,2), labels = c("FL","LE")),
  freq = factor(levels = c(1,2,3), labels = c("6Hz","8.57Hz","15Hz")),
  mod = factor(levels = c(1,2), labels = c("square","sine")),
  ssvepSNR = numeric()
)

# for each condition, files in the folder are listed, values for participant ID,
# lab, condition, as well as the single-trial SNR at predefined channels
# (raw data only contains driving frequencies)
list6sqSNR <- list.files(path = fileFolder, pattern = "6Hz_square_SNR")
for (i in 1:length(list6sqSNR)) {
  currData <- read.csv(paste0(fileFolder,list6sqSNR[i]), header = FALSE, sep = ",")
  if (substr(list6sqSNR[i],1,2) == "FL") {
    df[nrow(df)+1, 1:4] <- c(substr(list6sqSNR[i],1,5), substr(list6sqSNR[i],1,2), "6Hz", "square")
    df$ssvepSNR[nrow(df)] <- mean(as.matrix(currData[FLbinChn,]), na.rm = TRUE)
    if (exists("trialMatFL6sqSNR")) {
      trialMatFL6sqSNR <- rbind(trialMatFL6sqSNR, colMeans(currData[FLbinChn,]))
    } else {
      trialMatFL6sqSNR <- colMeans(currData[FLbinChn,])
    }
    if (exists("topoMatFL6sqSNR")) {
      topoMatFL6sqSNR <- cbind(topoMatFL6sqSNR, colMeans(t(currData), na.rm = TRUE))
    } else {
      topoMatFL6sqSNR <- colMeans(t(currData), na.rm = TRUE)
    }
  } else if (substr(list6sqSNR[i],1,2) == "LE") {
    df[nrow(df)+1, 1:4] <- c(substr(list6sqSNR[i],1,5), substr(list6sqSNR[i],1,2), "6Hz", "square")
    df$ssvepSNR[nrow(df)] <- mean(as.matrix(currData[LEbinChn,]), na.rm = TRUE)
    if (exists("trialMatLE6sqSNR")) {
      trialMatLE6sqSNR <- rbind(trialMatLE6sqSNR, colMeans(currData[LEbinChn,]))
    } else {
      trialMatLE6sqSNR <- colMeans(currData[LEbinChn,])
    }
    if (exists("topoMatLE6sqSNR")) {
      topoMatLE6sqSNR <- cbind(topoMatLE6sqSNR, colMeans(t(currData), na.rm = TRUE))
    } else {
      topoMatLE6sqSNR <- colMeans(t(currData), na.rm = TRUE)
    }
  }
}

list6sinSNR <- list.files(path = fileFolder, pattern = "6Hz_sine_SNR")
for (i in 1:length(list6sinSNR)) {
  currData <- read.csv(paste0(fileFolder,list6sinSNR[i]), header = FALSE, sep = ",")
  if (substr(list6sinSNR[i],1,2) == "FL") {
    df[nrow(df)+1, 1:4] <- c(substr(list6sqSNR[i],1,5), substr(list6sqSNR[i],1,2), "6Hz", "sine")
    df$ssvepSNR[nrow(df)] <- mean(as.matrix(currData[FLbinChn,]), na.rm = TRUE)
    if (exists("trialMatFL6sinSNR")) {
      trialMatFL6sinSNR <- rbind(trialMatFL6sinSNR, colMeans(currData[FLbinChn,]))
    } else {
      trialMatFL6sinSNR <- colMeans(currData[FLbinChn,])
    }
    if (exists("topoMatFL6sinSNR")) {
      topoMatFL6sinSNR <- cbind(topoMatFL6sinSNR, colMeans(t(currData), na.rm = TRUE))
    } else {
      topoMatFL6sinSNR <- colMeans(t(currData), na.rm = TRUE)
    }
  } else if (substr(list6sinSNR[i],1,2) == "LE") {
    df[nrow(df)+1, 1:4] <- c(substr(list6sqSNR[i],1,5), substr(list6sqSNR[i],1,2), "6Hz", "sine")
    df$ssvepSNR[nrow(df)] <- mean(as.matrix(currData[LEbinChn,]), na.rm = TRUE)
    if (exists("trialMatLE6sinSNR")) {
      trialMatLE6sinSNR <- rbind(trialMatLE6sinSNR, colMeans(currData[LEbinChn,]))
    } else {
      trialMatLE6sinSNR <- colMeans(currData[LEbinChn,])
    }
    if (exists("topoMatLE6sinSNR")) {
      topoMatLE6sinSNR <- cbind(topoMatLE6sinSNR, colMeans(t(currData), na.rm = TRUE))
    } else {
      topoMatLE6sinSNR <- colMeans(t(currData), na.rm = TRUE)
    }
  }
}

list857sqSNR <- list.files(path = fileFolder, pattern = "8.57Hz_square_SNR")
for (i in 1:length(list857sqSNR)) {
  currData <- read.csv(paste0(fileFolder,list857sqSNR[i]), header = FALSE, sep = ",")
  if (substr(list857sqSNR[i],1,2) == "FL") {
    df[nrow(df)+1, 1:4] <- c(substr(list6sqSNR[i],1,5), substr(list6sqSNR[i],1,2), "8.57Hz", "square")
    df$ssvepSNR[nrow(df)] <- mean(as.matrix(currData[FLbinChn,]), na.rm = TRUE)
    if (exists("trialMatFL857sqSNR")) {
      trialMatFL857sqSNR <- rbind(trialMatFL857sqSNR, colMeans(currData[FLbinChn,]))
    } else {
      trialMatFL857sqSNR <- colMeans(currData[FLbinChn,])
    }
    if (exists("topoMatFL857sqSNR")) {
      topoMatFL857sqSNR <- cbind(topoMatFL857sqSNR, colMeans(t(currData), na.rm = TRUE))
    } else {
      topoMatFL857sqSNR <- colMeans(t(currData), na.rm = TRUE)
    }
  } else if (substr(list857sqSNR[i],1,2) == "LE") {
    df[nrow(df)+1, 1:4] <- c(substr(list6sqSNR[i],1,5), substr(list6sqSNR[i],1,2), "8.57Hz", "square")
    df$ssvepSNR[nrow(df)] <- mean(as.matrix(currData[LEbinChn,]), na.rm = TRUE)
    if (exists("trialMatLE857sqSNR")) {
      trialMatLE857sqSNR <- rbind(trialMatLE857sqSNR, colMeans(currData[LEbinChn,]))
    } else {
      trialMatLE857sqSNR <- colMeans(currData[LEbinChn,])
    }
    if (exists("topoMatLE857sqSNR")) {
      topoMatLE857sqSNR <- cbind(topoMatLE857sqSNR, colMeans(t(currData), na.rm = TRUE))
    } else {
      topoMatLE857sqSNR <- colMeans(t(currData), na.rm = TRUE)
    }
  }
}

list857sinSNR <- list.files(path = fileFolder, pattern = "8.57Hz_sine_SNR")
for (i in 1:length(list857sinSNR)) {
  currData <- read.csv(paste0(fileFolder,list857sinSNR[i]), header = FALSE, sep = ",")
  if (substr(list857sinSNR[i],1,2) == "FL") {
    df[nrow(df)+1, 1:4] <- c(substr(list6sqSNR[i],1,5), substr(list6sqSNR[i],1,2), "8.57Hz", "sine")
    df$ssvepSNR[nrow(df)] <- mean(as.matrix(currData[FLbinChn,]), na.rm = TRUE)
    if (exists("trialMatFL857sinSNR")) {
      trialMatFL857sinSNR <- rbind(trialMatFL857sinSNR, colMeans(currData[FLbinChn,]))
    } else {
      trialMatFL857sinSNR <- colMeans(currData[FLbinChn,])
    }
    if (exists("topoMatFL857sinSNR")) {
      topoMatFL857sinSNR <- cbind(topoMatFL857sinSNR, colMeans(t(currData), na.rm = TRUE))
    } else {
      topoMatFL857sinSNR <- colMeans(t(currData), na.rm = TRUE)
    }
  } else if (substr(list857sinSNR[i],1,2) == "LE") {
    df[nrow(df)+1, 1:4] <- c(substr(list6sqSNR[i],1,5), substr(list6sqSNR[i],1,2), "8.57Hz", "sine")
    df$ssvepSNR[nrow(df)] <- mean(as.matrix(currData[LEbinChn,]), na.rm = TRUE)
    if (exists("trialMatLE857sinSNR")) {
      trialMatLE857sinSNR <- rbind(trialMatLE857sinSNR, colMeans(currData[LEbinChn,]))
    } else {
      trialMatLE857sinSNR <- colMeans(currData[LEbinChn,])
    }
    if (exists("topoMatLE857sinSNR")) {
      topoMatLE857sinSNR <- cbind(topoMatLE857sinSNR, colMeans(t(currData), na.rm = TRUE))
    } else {
      topoMatLE857sinSNR <- colMeans(t(currData), na.rm = TRUE)
    }
  }
}

list15sqSNR <- list.files(path = fileFolder, pattern = "15Hz_square_SNR")
for (i in 1:length(list15sqSNR)) {
  currData <- read.csv(paste0(fileFolder,list15sqSNR[i]), header = FALSE, sep = ",")
  if (substr(list15sqSNR[i],1,2) == "FL") {
    df[nrow(df)+1, 1:4] <- c(substr(list6sqSNR[i],1,5), substr(list6sqSNR[i],1,2), "15Hz", "square")
    df$ssvepSNR[nrow(df)] <- mean(as.matrix(currData[FLbinChn,]), na.rm = TRUE)
    if (exists("trialMatFL15sqSNR")) {
      trialMatFL15sqSNR <- rbind(trialMatFL15sqSNR, colMeans(currData[FLbinChn,]))
    } else {
      trialMatFL15sqSNR <- colMeans(currData[FLbinChn,])
    }
    if (exists("topoMatFL15sqSNR")) {
      topoMatFL15sqSNR <- cbind(topoMatFL15sqSNR, colMeans(t(currData), na.rm = TRUE))
    } else {
      topoMatFL15sqSNR <- colMeans(t(currData), na.rm = TRUE)
    }
  } else if (substr(list15sqSNR[i],1,2) == "LE") {
    df[nrow(df)+1, 1:4] <- c(substr(list6sqSNR[i],1,5), substr(list6sqSNR[i],1,2), "15Hz", "square")
    df$ssvepSNR[nrow(df)] <- mean(as.matrix(currData[LEbinChn,]), na.rm = TRUE)
    if (exists("trialMatLE15sqSNR")) {
      trialMatLE15sqSNR <- rbind(trialMatLE15sqSNR, colMeans(currData[LEbinChn,]))
    } else {
      trialMatLE15sqSNR <- colMeans(currData[LEbinChn,])
    }
    if (exists("topoMatLE15sqSNR")) {
      topoMatLE15sqSNR <- cbind(topoMatLE15sqSNR, colMeans(t(currData), na.rm = TRUE))
    } else {
      topoMatLE15sqSNR <- colMeans(t(currData), na.rm = TRUE)
    }
  }
}

list15sinSNR <- list.files(path = fileFolder, pattern = "15Hz_sine_SNR")
for (i in 1:length(list15sinSNR)) {
  currData <- read.csv(paste0(fileFolder,list15sinSNR[i]), header = FALSE, sep = ",")
  if (substr(list15sinSNR[i],1,2) == "FL") {
    df[nrow(df)+1, 1:4] <- c(substr(list6sqSNR[i],1,5), substr(list6sqSNR[i],1,2), "15Hz", "sine")
    df$ssvepSNR[nrow(df)] <- mean(as.matrix(currData[FLbinChn,]), na.rm = TRUE)
    if (exists("trialMatFL15sinSNR")) {
      trialMatFL15sinSNR <- rbind(trialMatFL15sinSNR, colMeans(currData[FLbinChn,]))
    } else {
      trialMatFL15sinSNR <- colMeans(currData[FLbinChn,])
    }
    if (exists("topoMatFL15sinSNR")) {
      topoMatFL15sinSNR <- cbind(topoMatFL15sinSNR, colMeans(t(currData), na.rm = TRUE))
    } else {
      topoMatFL15sinSNR <- colMeans(t(currData), na.rm = TRUE)
    }
  } else if (substr(list15sinSNR[i],1,2) == "LE") {
    df[nrow(df)+1, 1:4] <- c(substr(list6sqSNR[i],1,5), substr(list6sqSNR[i],1,2), "15Hz", "sine")
    df$ssvepSNR[nrow(df)] <- mean(as.matrix(currData[LEbinChn,]), na.rm = TRUE)
    if (exists("trialMatLE15sinSNR")) {
      trialMatLE15sinSNR <- rbind(trialMatLE15sinSNR, colMeans(currData[LEbinChn,]))
    } else {
      trialMatLE15sinSNR <- colMeans(currData[LEbinChn,])
    }
    if (exists("topoMatLE15sinSNR")) {
      topoMatLE15sinSNR <- cbind(topoMatLE15sinSNR, colMeans(t(currData), na.rm = TRUE))
    } else {
      topoMatLE15sinSNR <- colMeans(t(currData), na.rm = TRUE)
    }
  }
}

# reading SNR data finished



# Finishing Touches & Saving Stats Data Frame -----------------------------

# transform participant variable into factor and rename factor levels for lab
df$part <- factor(df$part)
levels(df$lab) <- c("Florida","Leipzig")

# z-standardize ssvep SNR into new variable, separately for labs
df$ssvepSNR_Z[df$lab == "Florida"] <- (df$ssvepSNR[df$lab == "Florida"] - mean(df$ssvepSNR[df$lab == "Florida"])) / 
  sd(df$ssvepSNR[df$lab == "Florida"])
df$ssvepSNR_Z[df$lab == "Leipzig"] <- (df$ssvepSNR[df$lab == "Leipzig"] - mean(df$ssvepSNR[df$lab == "Leipzig"])) / 
  sd(df$ssvepSNR[df$lab == "Leipzig"])

# save data frame in subflder for data frames
savename = paste0(parentFolder,"/dataframes/supp_dfSSVEP_commPL_singleTrialFFT_singleTrialSNR.csv")
write.csv(x = df, savename, row.names = FALSE)



# Averaging Individual SNR Time Courses and ---------------------------------

# Averaging individual spectra, separately for labs
trialAvgFL6sqSNR <- colMeans(trialMatFL6sqSNR, na.rm = TRUE)
trialAvgFL6sinSNR <- colMeans(trialMatFL6sinSNR, na.rm = TRUE)
trialAvgFL857sqSNR <- colMeans(trialMatFL857sqSNR, na.rm = TRUE)
trialAvgFL857sinSNR <- colMeans(trialMatFL857sinSNR, na.rm = TRUE)
trialAvgFL15sqSNR <- colMeans(trialMatFL15sqSNR, na.rm = TRUE)
trialAvgFL15sinSNR <- colMeans(trialMatFL15sinSNR, na.rm = TRUE)
trialAvgLE6sqSNR <- colMeans(trialMatLE6sqSNR, na.rm = TRUE)
trialAvgLE6sinSNR <- colMeans(trialMatLE6sinSNR, na.rm = TRUE)
trialAvgLE857sqSNR <- colMeans(trialMatLE857sqSNR, na.rm = TRUE)
trialAvgLE857sinSNR <- colMeans(trialMatLE857sinSNR, na.rm = TRUE)
trialAvgLE15sqSNR <- colMeans(trialMatLE15sqSNR, na.rm = TRUE)
trialAvgLE15sinSNR <- colMeans(trialMatLE15sinSNR, na.rm = TRUE)

# moving average on time series, padded with first and last value of the series, respectively
trialAvgFL6sqSNR_padded <- c(rep(trialAvgFL6sqSNR[1],floor(avgWin/2)), trialAvgFL6sqSNR, 
                             rep(trialAvgFL6sqSNR[length(trialAvgFL6sqSNR)],floor(avgWin/2)))
trialAvgFL6sqSNR_smoothed <- stats::filter(trialAvgFL6sqSNR_padded, rep(1/avgWin,avgWin))
trialAvgFL6sqSNR_smoothed <- trialAvgFL6sqSNR_smoothed[(1+floor(avgWin/2)) : (length(trialAvgFL6sqSNR_smoothed)-floor(avgWin/2))]

trialAvgFL6sinSNR_padded <- c(rep(trialAvgFL6sinSNR[1],floor(avgWin/2)), trialAvgFL6sinSNR, 
                              rep(trialAvgFL6sinSNR[length(trialAvgFL6sinSNR)],floor(avgWin/2)))
trialAvgFL6sinSNR_smoothed <- stats::filter(trialAvgFL6sinSNR_padded, rep(1/avgWin,avgWin))
trialAvgFL6sinSNR_smoothed <- trialAvgFL6sinSNR_smoothed[(1+floor(avgWin/2)) : (length(trialAvgFL6sinSNR_smoothed)-floor(avgWin/2))]

trialAvgFL857sqSNR_padded <- c(rep(trialAvgFL857sqSNR[1],floor(avgWin/2)), trialAvgFL857sqSNR, 
                               rep(trialAvgFL857sqSNR[length(trialAvgFL857sqSNR)],floor(avgWin/2)))
trialAvgFL857sqSNR_smoothed <- stats::filter(trialAvgFL857sqSNR_padded, rep(1/avgWin,avgWin))
trialAvgFL857sqSNR_smoothed <- trialAvgFL857sqSNR_smoothed[(1+floor(avgWin/2)) : (length(trialAvgFL857sqSNR_smoothed)-floor(avgWin/2))]

trialAvgFL857sinSNR_padded <- c(rep(trialAvgFL857sinSNR[1],floor(avgWin/2)), trialAvgFL857sinSNR, 
                                rep(trialAvgFL857sinSNR[length(trialAvgFL857sinSNR)],floor(avgWin/2)))
trialAvgFL857sinSNR_smoothed <- stats::filter(trialAvgFL857sinSNR_padded, rep(1/avgWin,avgWin))
trialAvgFL857sinSNR_smoothed <- trialAvgFL857sinSNR_smoothed[(1+floor(avgWin/2)) : (length(trialAvgFL857sinSNR_smoothed)-floor(avgWin/2))]

trialAvgFL15sqSNR_padded <- c(rep(trialAvgFL15sqSNR[1],floor(avgWin/2)), trialAvgFL15sqSNR, 
                              rep(trialAvgFL15sqSNR[length(trialAvgFL15sqSNR)],floor(avgWin/2)))
trialAvgFL15sqSNR_smoothed <- stats::filter(trialAvgFL15sqSNR_padded, rep(1/avgWin,avgWin))
trialAvgFL15sqSNR_smoothed <- trialAvgFL15sqSNR_smoothed[(1+floor(avgWin/2)) : (length(trialAvgFL15sqSNR_smoothed)-floor(avgWin/2))]

trialAvgFL15sinSNR_padded <- c(rep(trialAvgFL15sinSNR[1],floor(avgWin/2)), trialAvgFL15sinSNR, 
                               rep(trialAvgFL15sinSNR[length(trialAvgFL15sinSNR)],floor(avgWin/2)))
trialAvgFL15sinSNR_smoothed <- stats::filter(trialAvgFL15sinSNR_padded, rep(1/avgWin,avgWin))
trialAvgFL15sinSNR_smoothed <- trialAvgFL15sinSNR_smoothed[(1+floor(avgWin/2)) : (length(trialAvgFL15sinSNR_smoothed)-floor(avgWin/2))]

trialAvgLE6sqSNR_padded <- c(rep(trialAvgLE6sqSNR[1],floor(avgWin/2)), trialAvgLE6sqSNR, 
                             rep(trialAvgLE6sqSNR[length(trialAvgLE6sqSNR)],floor(avgWin/2)))
trialAvgLE6sqSNR_smoothed <- stats::filter(trialAvgLE6sqSNR_padded, rep(1/avgWin,avgWin))
trialAvgLE6sqSNR_smoothed <- trialAvgLE6sqSNR_smoothed[(1+floor(avgWin/2)) : (length(trialAvgLE6sqSNR_smoothed)-floor(avgWin/2))]

trialAvgLE6sinSNR_padded <- c(rep(trialAvgLE6sinSNR[1],floor(avgWin/2)), trialAvgLE6sinSNR, 
                              rep(trialAvgLE6sinSNR[length(trialAvgLE6sinSNR)],floor(avgWin/2)))
trialAvgLE6sinSNR_smoothed <- stats::filter(trialAvgLE6sinSNR_padded, rep(1/avgWin,avgWin))
trialAvgLE6sinSNR_smoothed <- trialAvgLE6sinSNR_smoothed[(1+floor(avgWin/2)) : (length(trialAvgLE6sinSNR_smoothed)-floor(avgWin/2))]

trialAvgLE857sqSNR_padded <- c(rep(trialAvgLE857sqSNR[1],floor(avgWin/2)), trialAvgLE857sqSNR, 
                               rep(trialAvgLE857sqSNR[length(trialAvgLE857sqSNR)],floor(avgWin/2)))
trialAvgLE857sqSNR_smoothed <- stats::filter(trialAvgLE857sqSNR_padded, rep(1/avgWin,avgWin))
trialAvgLE857sqSNR_smoothed <- trialAvgLE857sqSNR_smoothed[(1+floor(avgWin/2)) : (length(trialAvgLE857sqSNR_smoothed)-floor(avgWin/2))]

trialAvgLE857sinSNR_padded <- c(rep(trialAvgLE857sinSNR[1],floor(avgWin/2)), trialAvgLE857sinSNR, 
                                rep(trialAvgLE857sinSNR[length(trialAvgLE857sinSNR)],floor(avgWin/2)))
trialAvgLE857sinSNR_smoothed <- stats::filter(trialAvgLE857sinSNR_padded, rep(1/avgWin,avgWin))
trialAvgLE857sinSNR_smoothed <- trialAvgLE857sinSNR_smoothed[(1+floor(avgWin/2)) : (length(trialAvgLE857sinSNR_smoothed)-floor(avgWin/2))]

trialAvgLE15sqSNR_padded <- c(rep(trialAvgLE15sqSNR[1],floor(avgWin/2)), trialAvgLE15sqSNR, 
                              rep(trialAvgLE15sqSNR[length(trialAvgLE15sqSNR)],floor(avgWin/2)))
trialAvgLE15sqSNR_smoothed <- stats::filter(trialAvgLE15sqSNR_padded, rep(1/avgWin,avgWin))
trialAvgLE15sqSNR_smoothed <- trialAvgLE15sqSNR_smoothed[(1+floor(avgWin/2)) : (length(trialAvgLE15sqSNR_smoothed)-floor(avgWin/2))]

trialAvgLE15sinSNR_padded <- c(rep(trialAvgLE15sinSNR[1],floor(avgWin/2)), trialAvgLE15sinSNR, 
                               rep(trialAvgLE15sinSNR[length(trialAvgLE15sinSNR)],floor(avgWin/2)))
trialAvgLE15sinSNR_smoothed <- stats::filter(trialAvgLE15sinSNR_padded, rep(1/avgWin,avgWin))
trialAvgLE15sinSNR_smoothed <- trialAvgLE15sinSNR_smoothed[(1+floor(avgWin/2)) : (length(trialAvgLE15sinSNR_smoothed)-floor(avgWin/2))]




# Create and save data frame for plotting time courses --------------------
lengthTS <- length(trialAvgFL6sqAmp)
dfTimeCourse <- data.frame(
  lab = factor(c(rep("Florida",lengthTS*6), rep("Leipzig",lengthTS*6)), 
               levels = c("Florida","Leipzig")),
  freq = factor(rep(c(rep("6Hz",lengthTS*2), rep("8.57Hz",lengthTS*2), rep("15Hz",lengthTS*2)),2), 
                levels = c("6Hz","8.57Hz","15Hz")),
  mod = factor(rep(c(rep("square",lengthTS), rep("sine",lengthTS)),6), 
               levels = c("square","sine")),
  trial = rep(1:lengthTS,12),
  amplitude = c(trialAvgFL6sqAmp, trialAvgFL6sinAmp,
                trialAvgFL857sqAmp, trialAvgFL857sinAmp,
                trialAvgFL15sqAmp, trialAvgFL15sinAmp,
                trialAvgLE6sqAmp, trialAvgLE6sinAmp,
                trialAvgLE857sqAmp, trialAvgLE857sinAmp,
                trialAvgLE15sqAmp, trialAvgLE15sinAmp),
  amplitude_smoothed = c(trialAvgFL6sqAmp_smoothed, trialAvgFL6sinAmp_smoothed,
                         trialAvgFL857sqAmp_smoothed, trialAvgFL857sinAmp_smoothed,
                         trialAvgFL15sqAmp_smoothed, trialAvgFL15sinAmp_smoothed,
                         trialAvgLE6sqAmp_smoothed, trialAvgLE6sinAmp_smoothed,
                         trialAvgLE857sqAmp_smoothed, trialAvgLE857sinAmp_smoothed,
                         trialAvgLE15sqAmp_smoothed, trialAvgLE15sinAmp_smoothed),
  SNR = c(trialAvgFL6sqSNR, trialAvgFL6sinSNR,
          trialAvgFL857sqSNR, trialAvgFL857sinSNR,
          trialAvgFL15sqSNR, trialAvgFL15sinSNR,
          trialAvgLE6sqSNR, trialAvgLE6sinSNR,
          trialAvgLE857sqSNR, trialAvgLE857sinSNR,
          trialAvgLE15sqSNR, trialAvgLE15sinSNR),
  SNR_smoothed = c(trialAvgFL6sqSNR_smoothed, trialAvgFL6sinSNR_smoothed,
                   trialAvgFL857sqSNR_smoothed, trialAvgFL857sinSNR_smoothed,
                   trialAvgFL15sqSNR_smoothed, trialAvgFL15sinSNR_smoothed,
                   trialAvgLE6sqSNR_smoothed, trialAvgLE6sinSNR_smoothed,
                   trialAvgLE857sqSNR_smoothed, trialAvgLE857sinSNR_smoothed,
                   trialAvgLE15sqSNR_smoothed, trialAvgLE15sinSNR_smoothed)
)

# save time course dataframe
savename = paste0(parentFolder,"/dataframes/supp_dfTimeCourses_commPL_singleTrialFFT_plotting.csv")
write.csv(x = dfTimeCourse, savename, row.names = FALSE)



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
topoAvgFL6sqSNR <- rowMeans(topoMatFL6sqSNR)
topoAvgFL6sinSNR <- rowMeans(topoMatFL6sinSNR)
topoAvgFL857sqSNR <- rowMeans(topoMatFL857sqSNR)
topoAvgFL857sinSNR <- rowMeans(topoMatFL857sinSNR)
topoAvgFL15sqSNR <- rowMeans(topoMatFL15sqSNR)
topoAvgFL15sinSNR <- rowMeans(topoMatFL15sinSNR)
topoAvgLE6sqSNR <- rowMeans(topoMatLE6sqSNR)
topoAvgLE6sinSNR <- rowMeans(topoMatLE6sinSNR)
topoAvgLE857sqSNR <- rowMeans(topoMatLE857sqSNR)
topoAvgLE857sinSNR <- rowMeans(topoMatLE857sinSNR)
topoAvgLE15sqSNR <- rowMeans(topoMatLE15sqSNR)
topoAvgLE15sinSNR <- rowMeans(topoMatLE15sinSNR)

# number of electrodes, separate for lab
nrChansFL = length(topoAvgFL6sqSNR)
nrChansLE = length(topoAvgLE6sqSNR)

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
  amplitudeSNR = c(topoAvgFL6sqSNR, topoAvgFL6sinSNR, topoAvgFL857sqSNR, topoAvgFL857sinSNR, topoAvgFL15sqSNR, topoAvgFL15sinSNR,
                   topoAvgLE6sqSNR, topoAvgLE6sinSNR, topoAvgLE857sqSNR, topoAvgLE857sinSNR, topoAvgLE15sqSNR, topoAvgLE15sinSNR)
)

# save topo dataframe
savename = paste0(parentFolder,"/dataframes/supp_dfTopos_commPL_singleTrialFFT_singleTrialSNR.csv")
write.csv(x = dfTopos, savename, row.names = FALSE)