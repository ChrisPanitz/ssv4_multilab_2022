# --- author: Christian Panitz
# --- encoding: en_US.UTF-8
# --- R version: 4.0.3 (2020-10-10) -- "Bunny-Wunnies Freak Out"
# --- RStudio version: 2022.02.3
# --- script version: December 2022
# --- content: reading individual Hilbert transform data and plot averages for each condition & lab (common pipeline, single-trial analyses)

# indices in spectrum data matrices

# because of sliding-window averaging (window length = 4 cycles of driving frequency)
# driving frequency (1f) is always Bin 5, 2f => 9, 3f => 14, ... bin(xf) = 1 + 4x

# Florida: Oz & Iz
FLbinChn <- c(75,81)
timePointsFL <- seq(-600,2400,2) # in ms

# Leipzig: Oz & Iz
LEbinChn <- c(28,29)
timePointsLE <- seq(-250,2332,1000/256) # in ms

# freqRes6 <- 6/4;
# freqRes857 <- 60/7/4;
# freqRes15 <- 15/4;


# Packages & Path Setting -------------------------------------------------

# load required libraries
library(here)
library(ggplot2)
library(RColorBrewer)
library(ggpubr)

# set parent folder
parentFolder <- here()



# Reading Individual Time Courses & Filling Stats Data Frame -------------

# set path for raw data (individual spectra) to read
fileFolder <- paste0(parentFolder,"/supplementaryData/hilbert/")

# for each condition, files in the folder are listed, values for participant ID,
# lab, condition, as well as the ssvep amplitude at predefined frequencies & channels
list6sq <- list.files(path = fileFolder, pattern = "6Hz_square")
for (i in 1:length(list6sq)) {
  currData <- read.csv(paste0(fileFolder,list6sq[i]), header = FALSE, sep = ",")
  if (substr(list6sq[i],1,2) == "FL") {
    if (exists("hilbertMatFL6sq")) {
      hilbertMatFL6sq <- rbind(hilbertMatFL6sq, colMeans(currData[FLbinChn,]))
    } else {
      hilbertMatFL6sq <- colMeans(currData[FLbinChn,])
    }
  } else if (substr(list6sq[i],1,2) == "LE") {
    if (exists("hilbertMatLE6sq")) {
      hilbertMatLE6sq <- rbind(hilbertMatLE6sq, colMeans(currData[LEbinChn,]))
    } else {
      hilbertMatLE6sq <- colMeans(currData[LEbinChn,])
    }
  }
}

list6sin <- list.files(path = fileFolder, pattern = "6Hz_sine")
for (i in 1:length(list6sin)) {
  currData <- read.csv(paste0(fileFolder,list6sin[i]), header = FALSE, sep = ",")
  if (substr(list6sin[i],1,2) == "FL") {
    if (exists("hilbertMatFL6sin")) {
      hilbertMatFL6sin <- rbind(hilbertMatFL6sin, colMeans(currData[FLbinChn,]))
    } else {
      hilbertMatFL6sin <- colMeans(currData[FLbinChn,])
    }
  } else if (substr(list6sin[i],1,2) == "LE") {
    if (exists("hilbertMatLE6sin")) {
      hilbertMatLE6sin <- rbind(hilbertMatLE6sin, colMeans(currData[LEbinChn,]))
    } else {
      hilbertMatLE6sin <- colMeans(currData[LEbinChn,])
    }
  }
}

list857sq <- list.files(path = fileFolder, pattern = "8.57Hz_square")
for (i in 1:length(list857sq)) {
  currData <- read.csv(paste0(fileFolder,list857sq[i]), header = FALSE, sep = ",")
  if (substr(list857sq[i],1,2) == "FL") {
    if (exists("hilbertMatFL857sq")) {
      hilbertMatFL857sq <- rbind(hilbertMatFL857sq, colMeans(currData[FLbinChn,]))
    } else {
      hilbertMatFL857sq <- colMeans(currData[FLbinChn,])
    }
  } else if (substr(list857sq[i],1,2) == "LE") {
    if (exists("hilbertMatLE857sq")) {
      hilbertMatLE857sq <- rbind(hilbertMatLE857sq, colMeans(currData[LEbinChn,]))
    } else {
      hilbertMatLE857sq <- colMeans(currData[LEbinChn,])
    }
  }
}

list857sin <- list.files(path = fileFolder, pattern = "8.57Hz_sine")
for (i in 1:length(list857sin)) {
  currData <- read.csv(paste0(fileFolder,list857sin[i]), header = FALSE, sep = ",")
  if (substr(list857sin[i],1,2) == "FL") {
    if (exists("hilbertMatFL857sin")) {
      hilbertMatFL857sin <- rbind(hilbertMatFL857sin, colMeans(currData[FLbinChn,]))
    } else {
      hilbertMatFL857sin <- colMeans(currData[FLbinChn,])
    }
  } else if (substr(list857sin[i],1,2) == "LE") {
    if (exists("hilbertMatLE857sin")) {
      hilbertMatLE857sin <- rbind(hilbertMatLE857sin, colMeans(currData[LEbinChn,]))
    } else {
      hilbertMatLE857sin <- colMeans(currData[LEbinChn,])
    }
  }
}

list15sq <- list.files(path = fileFolder, pattern = "15Hz_square")
for (i in 1:length(list15sq)) {
  currData <- read.csv(paste0(fileFolder,list15sq[i]), header = FALSE, sep = ",")
  if (substr(list15sq[i],1,2) == "FL") {
    if (exists("hilbertMatFL15sq")) {
      hilbertMatFL15sq <- rbind(hilbertMatFL15sq, colMeans(currData[FLbinChn,]))
    } else {
      hilbertMatFL15sq <- colMeans(currData[FLbinChn,])
    }
  } else if (substr(list15sq[i],1,2) == "LE") {
    if (exists("hilbertMatLE15sq")) {
      hilbertMatLE15sq <- rbind(hilbertMatLE15sq, colMeans(currData[LEbinChn,]))
    } else {
      hilbertMatLE15sq <- colMeans(currData[LEbinChn,])
    }
  }
}

list15sin <- list.files(path = fileFolder, pattern = "15Hz_sine")
for (i in 1:length(list15sin)) {
  currData <- read.csv(paste0(fileFolder,list15sin[i]), header = FALSE, sep = ",")
  if (substr(list15sin[i],1,2) == "FL") {
    if (exists("hilbertMatFL15sin")) {
      hilbertMatFL15sin <- rbind(hilbertMatFL15sin, colMeans(currData[FLbinChn,]))
    } else {
      hilbertMatFL15sin <- colMeans(currData[FLbinChn,])
    }
  } else if (substr(list15sin[i],1,2) == "LE") {
    if (exists("hilbertMatLE15sin")) {
      hilbertMatLE15sin <- rbind(hilbertMatLE15sin, colMeans(currData[LEbinChn,]))
    } else {
      hilbertMatLE15sin <- colMeans(currData[LEbinChn,])
    }
  }
}

# reading data finished



# Averaging Individual Spectra and Creating Data Frame for Plotting -------

# Averaging individual spectra, separately for labs
hilbertAvgFL6sq <- colMeans(hilbertMatFL6sq)
hilbertAvgFL6sin <- colMeans(hilbertMatFL6sin)
hilbertAvgFL857sq <- colMeans(hilbertMatFL857sq)
hilbertAvgFL857sin <- colMeans(hilbertMatFL857sin)
hilbertAvgFL15sq <- colMeans(hilbertMatFL15sq)
hilbertAvgFL15sin <- colMeans(hilbertMatFL15sin)
hilbertAvgLE6sq <- colMeans(hilbertMatLE6sq)
hilbertAvgLE6sin <- colMeans(hilbertMatLE6sin)
hilbertAvgLE857sq <- colMeans(hilbertMatLE857sq)
hilbertAvgLE857sin <- colMeans(hilbertMatLE857sin)
hilbertAvgLE15sq <- colMeans(hilbertMatLE15sq)
hilbertAvgLE15sin <- colMeans(hilbertMatLE15sin)

# number of samples per segment
lengthFL <- length(timePointsFL)
lengthLE <- length(timePointsLE)

dfHilbert <- data.frame(
  lab = factor(c(rep("Florida",lengthFL*6), rep("Leipzig",lengthLE*6)), 
               levels = c("Florida","Leipzig")),
  freq = factor(c(rep("6Hz",lengthFL*2), rep("8.57Hz",lengthFL*2), rep("15Hz",lengthFL*2),
                  rep("6Hz",lengthLE*2), rep("8.57Hz",lengthLE*2), rep("15Hz",lengthLE*2)), 
                levels = c("6Hz","8.57Hz","15Hz")),
  mod = factor(c(rep(c(rep("square",lengthFL), rep("sine",lengthFL)),3),
                 rep(c(rep("square",lengthLE), rep("sine",lengthLE)),3)), 
               levels = c("square","sine")),
  time = c(rep(timePointsFL,6), rep(timePointsLE,6)),
  amplitude = c(hilbertAvgFL6sq, hilbertAvgFL6sin,
                hilbertAvgFL857sq, hilbertAvgFL857sin,
                hilbertAvgFL15sq, hilbertAvgFL15sin,
                hilbertAvgLE6sq, hilbertAvgLE6sin,
                hilbertAvgLE857sq, hilbertAvgLE857sin,
                hilbertAvgLE15sq, hilbertAvgLE15sin)
)

yminFL <- min(dfHilbert$amplitude[dfHilbert$lab == "Florida"])
ymaxFL <- max(dfHilbert$amplitude[dfHilbert$lab == "Florida"])
yminLE <- min(dfHilbert$amplitude[dfHilbert$lab == "Leipzig"])
ymaxLE <- max(dfHilbert$amplitude[dfHilbert$lab == "Leipzig"])

plotHilbertFL6 <- ggplot(dfHilbert[dfHilbert$lab == "Florida" & dfHilbert$freq == "6Hz",], 
                        aes(x = time, y = amplitude, color = mod)) + theme_classic() +
  geom_line() +
  scale_color_manual(values = brewer.pal(4,"Purples")[4:3]) +
  scale_y_continuous(limits = c(yminFL,ymaxFL), name = "ssVEP amplitude") +
  scale_x_continuous(name = "Time in ms", breaks = c(0,1000,2000)) +
  labs(title = "Florida - 6 Hz") +
  theme(
    plot.title = element_text(color = "black", face = "bold", hjust = .5),
    legend.title = element_blank(),
    legend.position = "top"
  )

plotHilbertFL857 <- ggplot(dfHilbert[dfHilbert$lab == "Florida" & dfHilbert$freq == "8.57Hz",], 
                          aes(x = time, y = amplitude, color = mod)) + theme_classic() +
  geom_line() +
  scale_color_manual(values = brewer.pal(4,"Oranges")[4:3]) +
  scale_y_continuous(limits = c(yminFL,ymaxFL), name = "ssVEP amplitude") +
  scale_x_continuous(name = "Time in ms", breaks = c(0,1000,2000)) +
  labs(title = "Florida - 8.57 Hz") +
  theme(
    plot.title = element_text(color = "black", face = "bold", hjust = .5),
    legend.title = element_blank(),
    legend.position = "top"
  )

plotHilbertFL15 <- ggplot(dfHilbert[dfHilbert$lab == "Florida" & dfHilbert$freq == "15Hz",], 
                         aes(x = time, y = amplitude, color = mod)) + theme_classic() +
  geom_line() +
  scale_color_manual(values = brewer.pal(4,"BuGn")[4:3]) +
  scale_y_continuous(limits = c(yminFL,ymaxFL), name = "ssVEP amplitude") +
  scale_x_continuous(name = "Time in ms", breaks = c(0,1000,2000)) +
  labs(title = "Florida - 15 Hz") +
  theme(
    plot.title = element_text(color = "black", face = "bold", hjust = .5),
    legend.title = element_blank(),
    legend.position = "top"
  )

plotHilbertLE6 <- ggplot(dfHilbert[dfHilbert$lab == "Leipzig" & dfHilbert$freq == "6Hz",], 
                        aes(x = time, y = amplitude, color = mod)) + theme_classic() +
  geom_line() +
  scale_color_manual(values = brewer.pal(4,"Purples")[4:3]) +
  scale_y_continuous(limits = c(yminLE,ymaxLE), name = "ssVEP amplitude") +
  scale_x_continuous(name = "Time in ms", breaks = c(0,1000,2000)) +
  labs(title = "Leipzig - 6 Hz") +
  theme(
    plot.title = element_text(color = "black", face = "bold", hjust = .5),
    legend.title = element_blank(),
    legend.position = "top"
  )

plotHilbertLE857 <- ggplot(dfHilbert[dfHilbert$lab == "Leipzig" & dfHilbert$freq == "8.57Hz",], 
                          aes(x = time, y = amplitude, color = mod)) + theme_classic() +
  geom_line() +
  scale_color_manual(values = brewer.pal(4,"Oranges")[4:3]) +
  scale_y_continuous(limits = c(yminLE,ymaxLE), name = "ssVEP amplitude") +
  scale_x_continuous(name = "Time in ms", breaks = c(0,1000,2000)) +
  labs(title = "Leipzig - 8.57 Hz") +
  theme(
    plot.title = element_text(color = "black", face = "bold", hjust = .5),
    legend.title = element_blank(),
    legend.position = "top"
  )

plotHilbertLE15 <- ggplot(dfHilbert[dfHilbert$lab == "Leipzig" & dfHilbert$freq == "15Hz",], 
                         aes(x = time, y = amplitude, color = mod)) + theme_classic() +
  geom_line() +
  scale_color_manual(values = brewer.pal(4,"BuGn")[4:3]) +
  scale_y_continuous(limits = c(yminLE,ymaxLE), name = "ssVEP amplitude") +
  scale_x_continuous(name = "Time in ms", breaks = c(0,1000,2000)) +
  labs(title = "Leipzig - 15 Hz") +
  theme(
    plot.title = element_text(color = "black", face = "bold", hjust = .5),
    legend.title = element_blank(),
    legend.position = "top"
  )


plotHilberts <- ggarrange(plotHilbertFL6, plotHilbertFL857, plotHilbertFL15, 
                          plotHilbertLE6, plotHilbertLE857, plotHilbertLE15,
                          nrow = 2, ncol = 3)
plotHilberts



# Save figure
savename = paste0(parentFolder,"/figures/18_hilbertPlots_commPL.pdf")
ggsave(filename = savename, plot = plotHilberts, device = "pdf",
       width = 21, height = 15, unit = "cm", limitsize = FALSE)

savename = paste0(parentFolder,"/figures/18_hilbertPlots_commPL.jpg")
ggsave(filename = savename, plot = plotHilberts, device = "jpg",
       width = 21, height = 15, unit = "cm", limitsize = FALSE)
