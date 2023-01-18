# --- author: Christian Panitz
# --- encoding: en_US.UTF-8
# --- R version: 4.0.3 (2020-10-10) -- "Bunny-Wunnies Freak Out"
# --- RStudio version: 2022.02.3
# --- script version: December 2022
# --- content: plotting spectra for different conditions (common pipeline, single-trialFFT, topographies with SNR from average)


# Header Parameters -------------------------------------------------------

xmin <- 4
xmax <- 17
fType = "Helvetica"
fSize = 9



# Loading Packages & Data, Setting Path ----------------------------------

# load packages
library(here)
library(ggplot2)
library(RColorBrewer)
library(tidyr)
library(eegUtils)
library(OneR)
library(ggpubr)

# set parent folder
parentFolder <- here()

# load data to plot
loadname <- paste0(parentFolder,"/dataframes/supp_dfSpectra_commPL_singleTrialFFT.csv")
dfSpectra <- read.csv(loadname)
dfSpectra$lab <- factor(dfSpectra$lab, levels = c("Florida","Leipzig"), labels = c("Florida", "Leipzig"))
dfSpectra$freq <- factor(dfSpectra$freq, levels = c("6Hz","8.57Hz","15Hz"), labels = c("6Hz","8.57Hz","15Hz"))
dfSpectra$mod <- factor(dfSpectra$mod, levels = c("square","sine"), labels = c("square","sine"))

# overwrite DC component amplitude with 0 (for y axis scaling)
dfSpectra$amp[dfSpectra$freqBin == 0] = 0



# Single Spectra Plots ----------------------------------------------------

# set axis limits and frequency resolution (for bar width)
yminFL <- 0
ymaxFL <- max(dfSpectra$amp[dfSpectra$lab == "Florida"]) * 1.1
yminLE <- 0
ymaxLE <- max(dfSpectra$amp[dfSpectra$lab == "Leipzig"]) * 1.1

# Create bar plots with vertical, dashed line at driving frequency;
# One plot per Site and Driving Frequency with the two modulation functions in each plot
specFL6 <- ggplot(data = dfSpectra[dfSpectra$lab == "Florida" & dfSpectra$freq == "6Hz",], 
                  aes(x = freqBin, y = amp, fill = mod, color = mod)) + theme_classic() +
  geom_col(data = dfSpectra[dfSpectra$lab == "Florida" & dfSpectra$freq == "6Hz" & dfSpectra$mod == "square",], 
           width = 6/4) +
  geom_col(data = dfSpectra[dfSpectra$lab == "Florida" & dfSpectra$freq == "6Hz" & dfSpectra$mod == "sine",],
           width = 6/4, alpha = .50) +
  #geom_vline(xintercept = 6, color = "gray70", linetype = "dashed", size = .2) +
  scale_x_continuous(name = " ", limits = c(0,3.5*6), breaks = seq(6,3*6,6), labels = as.character(seq(6,3*6,6))) +
  scale_y_continuous(name = "Spectral amplitude ([µV/rad^2] / Hz)", limits = c(yminFL,ymaxFL)) +
  scale_fill_manual(values = brewer.pal(n = 4, "Purples")[4:3], breaks = c("square","sine")) +
  scale_color_manual(values = brewer.pal(n = 4, "Purples")[4:3], breaks = c("square","sine")) +
  labs(title = "Florida - 6 Hz") +
  theme(
    plot.title = element_text(size = fSize, color = "black", face = "bold", hjust = .5),
    axis.title.x = element_text(family = fType, color = "black", size = fSize,
                                face = "bold", margin = margin(t = 10)),
    axis.text.x = element_text(family = fType, color = "black", size = fSize),
    axis.text.y = element_text(family = fType, color = "black", size = fSize),
    axis.title.y = element_text(family = fType, color = "black", size = fSize,
                                face = "bold", margin = margin(r = 10)),
    legend.position = c(.9,.67),
    legend.justification = c(1,1),
    legend.title = element_blank(),
    legend.text = element_text(family = fType, color = "black", size = fSize-2),
    legend.key.size = unit(fSize-2, "pt"),
    legend.background = element_blank()
  )

specFL857 <- ggplot(data = dfSpectra[dfSpectra$lab == "Florida" & dfSpectra$freq == "8.57Hz",], 
                    aes(x = freqBin, y = amp, fill = mod, color = mod)) + theme_classic() +
  geom_col(data = dfSpectra[dfSpectra$lab == "Florida" & dfSpectra$freq == "8.57Hz" & dfSpectra$mod == "square",],
           width = 8.57/4) +
  geom_col(data = dfSpectra[dfSpectra$lab == "Florida" & dfSpectra$freq == "8.57Hz" & dfSpectra$mod == "sine",],
           width = 8.57/4, alpha = .50) +
  #geom_vline(xintercept = 60/7, color = "gray70", linetype = "dashed", size = .2) +
  scale_x_continuous(name = " ", limits = c(0,3.5*8.57), breaks = seq(8.57,3*8.57,8.57), labels = as.character(seq(8.57,3*8.57,8.57))) +
  scale_y_continuous(name = " ", limits = c(yminFL,ymaxFL)) +
  scale_fill_manual(values = brewer.pal(n = 4, "Oranges")[4:3], breaks = c("square","sine")) +
  scale_color_manual(values = brewer.pal(n = 4, "Oranges")[4:3], breaks = c("square","sine")) +
  labs(title = "Florida - 8.57 Hz") +
  theme(
    plot.title = element_text(size = fSize, color = "black", face = "bold", hjust = .5),
    axis.title.x = element_text(family = fType, color = "black", size = fSize,
                                face = "bold", margin = margin(t = 10)),
    axis.text.x = element_text(family = fType, color = "black", size = fSize),
    axis.text.y = element_text(family = fType, color = "black", size = fSize),
    axis.title.y = element_text(family = fType, color = "black", size = fSize,
                                face = "bold", margin = margin(r = 10)),
    legend.position = c(.9,.67),
    legend.justification = c(1,1),
    legend.title = element_blank(),
    legend.text = element_text(family = fType, color = "black", size = fSize-2),
    legend.key.size = unit(fSize-2, "pt"),
    legend.background = element_blank()
  )

specFL15 <- ggplot(data = dfSpectra[dfSpectra$lab == "Florida" & dfSpectra$freq == "15Hz",], 
                   aes(x = freqBin, y = amp, fill = mod, color = mod)) + theme_classic() +
  geom_col(data = dfSpectra[dfSpectra$lab == "Florida" & dfSpectra$freq == "15Hz" & dfSpectra$mod == "square",],
           width = 15/4) +
  geom_col(data = dfSpectra[dfSpectra$lab == "Florida" & dfSpectra$freq == "15Hz" & dfSpectra$mod == "sine",],
           width = 15/4, alpha = .50) +
  #geom_vline(xintercept = 15, color = "gray70", linetype = "dashed", size = .2) +
  scale_x_continuous(name = " ", limits = c(0,3.5*15), breaks = seq(15,3*15,15), labels = as.character(seq(15,3*15,15))) +
  scale_y_continuous(name = " ", limits = c(yminFL,ymaxFL)) +
  scale_fill_manual(values = brewer.pal(n = 4, "BuGn")[4:3], breaks = c("square","sine")) +
  scale_color_manual(values = brewer.pal(n = 4, "BuGn")[4:3], breaks = c("square","sine")) +
  labs(title = "Florida - 15 Hz") +
  theme(
    plot.title = element_text(size = fSize, color = "black", face = "bold", hjust = .5),
    axis.title.x = element_text(family = fType, color = "black", size = fSize,
                                face = "bold", margin = margin(t = 10)),
    axis.text.x = element_text(family = fType, color = "black", size = fSize),
    axis.text.y = element_text(family = fType, color = "black", size = fSize),
    axis.title.y = element_text(family = fType, color = "black", size = fSize,
                                face = "bold", margin = margin(r = 10)),
    legend.position = c(.9,.67),
    legend.justification = c(1,1),
    legend.title = element_blank(),
    legend.text = element_text(family = fType, color = "black", size = fSize-2),
    legend.key.size = unit(fSize-2, "pt"),
    legend.background = element_blank()
  )

specLE6 <- ggplot(data = dfSpectra[dfSpectra$lab == "Leipzig" & dfSpectra$freq == "6Hz",], 
                  aes(x = freqBin, y = amp, fill = mod, color = mod)) + theme_classic() +
  geom_col(data = dfSpectra[dfSpectra$lab == "Leipzig" & dfSpectra$freq == "6Hz" & dfSpectra$mod == "square",],
           width = 6/4) +
  geom_col(data = dfSpectra[dfSpectra$lab == "Leipzig" & dfSpectra$freq == "6Hz" & dfSpectra$mod == "sine",],
           width = 6/4, alpha = .50) +
  #geom_vline(xintercept = 6, color = "gray70", linetype = "dashed", size = .2) +
  scale_x_continuous(name = " ", limits = c(0,3.5*6), breaks = seq(6,3*6,6), labels = as.character(seq(6,3*6,6))) +
  scale_y_continuous(name = "Spectral amplitude ([µV/cm^2] / Hz)", limits = c(yminLE,ymaxLE)) +
  scale_fill_manual(values = brewer.pal(n = 4, "Purples")[4:3], breaks = c("square","sine")) +
  scale_color_manual(values = brewer.pal(n = 4, "Purples")[4:3], breaks = c("square","sine")) +
  labs(title = "Leipzig - 6 Hz") +
  theme(
    plot.title = element_text(size = fSize, color = "black", face = "bold", hjust = .5),
    axis.title.x = element_text(family = fType, color = "black", size = fSize,
                                face = "bold", margin = margin(t = 10)),
    axis.text.x = element_text(family = fType, color = "black", size = fSize),
    axis.text.y = element_text(family = fType, color = "black", size = fSize),
    axis.title.y = element_text(family = fType, color = "black", size = fSize,
                                face = "bold", margin = margin(r = 10)),
    legend.position = c(.9,.67),
    legend.justification = c(1,1),
    legend.title = element_blank(),
    legend.text = element_text(family = fType, color = "black", size = fSize-2),
    legend.key.size = unit(fSize-2, "pt"),
    legend.background = element_blank()
  )

specLE857 <- ggplot(data = dfSpectra[dfSpectra$lab == "Leipzig" & dfSpectra$freq == "8.57Hz",], 
                    aes(x = freqBin, y = amp, fill = mod, color = mod)) + theme_classic() +
  geom_col(data = dfSpectra[dfSpectra$lab == "Leipzig" & dfSpectra$freq == "8.57Hz" & dfSpectra$mod == "square",],
           width = 8.57/4) +
  geom_col(data = dfSpectra[dfSpectra$lab == "Leipzig" & dfSpectra$freq == "8.57Hz" & dfSpectra$mod == "sine",],
           width = 8.57/4, alpha = .50) +
  #geom_vline(xintercept = 60/7, color = "gray70", linetype = "dashed", size = .2) +
  scale_x_continuous(name = " ", limits = c(0,3.5*8.57), breaks = seq(8.57,3*8.57,8.57), labels = as.character(seq(8.57,3*8.57,8.57))) +
  scale_y_continuous(name = " ", limits = c(yminLE,ymaxLE)) +
  scale_fill_manual(values = brewer.pal(n = 4, "Oranges")[4:3], breaks = c("square","sine")) +
  scale_color_manual(values = brewer.pal(n = 4, "Oranges")[4:3], breaks = c("square","sine")) +
  labs(title = "Leipzig - 8.57 Hz") +
  theme(
    plot.title = element_text(size = fSize, color = "black", face = "bold", hjust = .5),
    axis.title.x = element_text(family = fType, color = "black", size = fSize,
                                face = "bold", margin = margin(t = 10)),
    axis.text.x = element_text(family = fType, color = "black", size = fSize),
    axis.text.y = element_text(family = fType, color = "black", size = fSize),
    axis.title.y = element_text(family = fType, color = "black", size = fSize,
                                face = "bold", margin = margin(r = 10)),
    legend.position = c(.9,.67),
    legend.justification = c(1,1),
    legend.title = element_blank(),
    legend.text = element_text(family = fType, color = "black", size = fSize-2),
    legend.key.size = unit(fSize-2, "pt"),
    legend.background = element_blank()
  )

specLE15 <- ggplot(data = dfSpectra[dfSpectra$lab == "Leipzig" & dfSpectra$freq == "15Hz",], 
                   aes(x = freqBin, y = amp, fill = mod, color = mod)) + theme_classic() +
  geom_col(data = dfSpectra[dfSpectra$lab == "Leipzig" & dfSpectra$freq == "15Hz" & dfSpectra$mod == "square",],
           width = 15/4) +
  geom_col(data = dfSpectra[dfSpectra$lab == "Leipzig" & dfSpectra$freq == "15Hz" & dfSpectra$mod == "sine",],
           width = 15/4, alpha = .50) +
  #geom_vline(xintercept = 15, color = "gray70", linetype = "dashed", size = .2) +
  scale_x_continuous(name = " ", limits = c(0,3.5*15), breaks = seq(15,3*15,15), labels = as.character(seq(15,3*15,15))) +
  scale_y_continuous(name = " ", limits = c(yminLE,ymaxLE)) +
  scale_fill_manual(values = brewer.pal(n = 4, "BuGn")[4:3], breaks = c("square","sine")) +
  scale_color_manual(values = brewer.pal(n = 4, "BuGn")[4:3], breaks = c("square","sine")) +
  labs(title = "Leipzig - 15 Hz") +
  theme(
    plot.title = element_text(size = fSize, color = "black", face = "bold", hjust = .5),
    axis.title.x = element_text(family = fType, color = "black", size = fSize,
                                face = "bold", margin = margin(t = 10)),
    axis.text.x = element_text(family = fType, color = "black", size = fSize),
    axis.text.y = element_text(family = fType, color = "black", size = fSize),
    axis.title.y = element_text(family = fType, color = "black", size = fSize,
                                face = "bold", margin = margin(r = 10)),
    legend.position = c(.9,.67),
    legend.justification = c(1,1),
    legend.title = element_blank(),
    legend.text = element_text(family = fType, color = "black", size = fSize-2),
    legend.key.size = unit(fSize-2, "pt"),
    legend.background = element_blank()
  )




# Topography Plots --------------------------------------------------------

topoRes <- 200
chanCol = "black"
nrColors = 8

# load topographical data
loadname <- paste0(parentFolder, "/dataframes/supp_dfTopos_commPL_singleTrialFFT.csv")
dfTopos <- read.csv(loadname, sep = ",")

# collapse topographical data across modulation functions
dfTopos <- pivot_wider(data = dfTopos,
                       id_cols = c(lab,freq,electrode,x,y),
                       names_from = mod,
                       values_from = amplitudeSNR
)
dfTopos$amplitude <- rowMeans(dfTopos[,c("square","sine")])

# create individual topographical plots
topoFL6 <- topoplot(data = dfTopos[dfTopos$lab == "Florida" & dfTopos$freq == "6Hz",],
                    contour = FALSE, scaling = 0.10, chan_marker = "none",
                    grid_res = topoRes) +
  geom_head(size = rel(1.5)*.10, color = "gray70") +
  geom_point(aes(x = dfTopos$x[dfTopos$lab == "Florida" & dfTopos$freq == "6Hz" & dfTopos$electrode == "E75"],
                 y = dfTopos$y[dfTopos$lab == "Florida" & dfTopos$freq == "6Hz" & dfTopos$electrode == "E75"]),
             color = chanCol, size = .25) +
  geom_point(aes(x = dfTopos$x[dfTopos$lab == "Florida" & dfTopos$freq == "6Hz" & dfTopos$electrode == "E81"],
                 y = dfTopos$y[dfTopos$lab == "Florida" & dfTopos$freq == "6Hz" & dfTopos$electrode == "E81"]),
             color = chanCol, size = .25) +
  theme(legend.position = "none")
topoFL6$data$fill <- as.numeric(bin(data = topoFL6$data$fill, nbins = nrColors))
topoFL6$layers <- topoFL6$layers[-c(3,4,5)]

topoFL857 <- topoplot(data = dfTopos[dfTopos$lab == "Florida" & dfTopos$freq == "8.57Hz",],
                      contour = FALSE, scaling = 0.10, chan_marker = "none",
                      grid_res = topoRes) +
  geom_head(size = rel(1.5)*.10, color = "gray70") +
  geom_point(aes(x = dfTopos$x[dfTopos$lab == "Florida" & dfTopos$freq == "8.57Hz" & dfTopos$electrode == "E75"],
                 y = dfTopos$y[dfTopos$lab == "Florida" & dfTopos$freq == "8.57Hz" & dfTopos$electrode == "E75"]),
             color = chanCol, size = .25) +
  geom_point(aes(x = dfTopos$x[dfTopos$lab == "Florida" & dfTopos$freq == "6Hz" & dfTopos$electrode == "E81"],
                 y = dfTopos$y[dfTopos$lab == "Florida" & dfTopos$freq == "6Hz" & dfTopos$electrode == "E81"]),
             color = chanCol, size = .25) +
  theme(legend.position = "none")
topoFL857$data$fill <- as.numeric(bin(data = topoFL857$data$fill, nbins = nrColors))
topoFL857$layers <- topoFL857$layers[-c(3,4,5)]

topoFL15 <- topoplot(data = dfTopos[dfTopos$lab == "Florida" & dfTopos$freq == "15Hz",],
                     contour = FALSE, scaling = 0.10, chan_marker = "none",
                     grid_res = topoRes) +
  geom_head(size = rel(1.5)*.10, color = "gray70") +
  geom_point(aes(x = dfTopos$x[dfTopos$lab == "Florida" & dfTopos$freq == "15Hz" & dfTopos$electrode == "E75"],
                 y = dfTopos$y[dfTopos$lab == "Florida" & dfTopos$freq == "15Hz" & dfTopos$electrode == "E75"]),
             color = chanCol, size = .25) +
  geom_point(aes(x = dfTopos$x[dfTopos$lab == "Florida" & dfTopos$freq == "6Hz" & dfTopos$electrode == "E81"],
                 y = dfTopos$y[dfTopos$lab == "Florida" & dfTopos$freq == "6Hz" & dfTopos$electrode == "E81"]),
             color = chanCol, size = .25) +
  theme(legend.position = "none")
topoFL15$data$fill <- as.numeric(bin(data = topoFL15$data$fill, nbins = nrColors))
topoFL15$layers <- topoFL15$layers[-c(3,4,5)]

topoLE6 <- topoplot(data = dfTopos[dfTopos$lab == "Leipzig" & dfTopos$freq == "6Hz",],
                    contour = FALSE, scaling = 0.10, chan_marker = "none",
                    grid_res = topoRes) +
  geom_head(size = rel(1.5)*.10, color = "gray70") +
  geom_point(aes(x = dfTopos$x[dfTopos$lab == "Leipzig" & dfTopos$freq == "6Hz" & dfTopos$electrode == "Oz"],
                 y = dfTopos$y[dfTopos$lab == "Leipzig" & dfTopos$freq == "6Hz" & dfTopos$electrode == "Oz"]),
             color = chanCol, size = .25) +
  geom_point(aes(x = dfTopos$x[dfTopos$lab == "Leipzig" & dfTopos$freq == "6Hz" & dfTopos$electrode == "Iz"],
                 y = dfTopos$y[dfTopos$lab == "Leipzig" & dfTopos$freq == "6Hz" & dfTopos$electrode == "Iz"]),
             color = chanCol, size = .25) +
  theme(legend.position = "none")
topoLE6$data$fill <- as.numeric(bin(data = topoLE6$data$fill, nbins = nrColors))
topoLE6$layers <- topoLE6$layers[-c(3,4,5)]

topoLE857 <- topoplot(data = dfTopos[dfTopos$lab == "Leipzig" & dfTopos$freq == "8.57Hz",],
                      contour = FALSE, scaling = 0.10, chan_marker = "none",
                      grid_res = topoRes) +
  geom_head(size = rel(1.5)*.10, color = "gray70") +
  geom_point(aes(x = dfTopos$x[dfTopos$lab == "Leipzig" & dfTopos$freq == "8.57Hz" & dfTopos$electrode == "Oz"],
                 y = dfTopos$y[dfTopos$lab == "Leipzig" & dfTopos$freq == "8.57Hz" & dfTopos$electrode == "Oz"]),
             color = chanCol, size = .25) +
  geom_point(aes(x = dfTopos$x[dfTopos$lab == "Leipzig" & dfTopos$freq == "8.57Hz" & dfTopos$electrode == "Iz"],
                 y = dfTopos$y[dfTopos$lab == "Leipzig" & dfTopos$freq == "8.57Hz" & dfTopos$electrode == "Iz"]),
             color = chanCol, size = .25) +
  theme(legend.position = "none")
topoLE857$data$fill <- as.numeric(bin(data = topoLE857$data$fill, nbins = nrColors))
topoLE857$layers <- topoLE857$layers[-c(3,4,5)]

topoLE15 <- topoplot(data = dfTopos[dfTopos$lab == "Leipzig" & dfTopos$freq == "15Hz",],
                     contour = FALSE, scaling = 0.10, chan_marker = "none",
                     grid_res = topoRes) +
  geom_head(size = rel(1.5)*.10, color = "gray70") +
  geom_point(aes(x = dfTopos$x[dfTopos$lab == "Leipzig" & dfTopos$freq == "15Hz" & dfTopos$electrode == "Oz"],
                 y = dfTopos$y[dfTopos$lab == "Leipzig" & dfTopos$freq == "15Hz" & dfTopos$electrode == "Oz"]),
             color = chanCol, size = .25) +
  geom_point(aes(x = dfTopos$x[dfTopos$lab == "Leipzig" & dfTopos$freq == "15Hz" & dfTopos$electrode == "Iz"],
                 y = dfTopos$y[dfTopos$lab == "Leipzig" & dfTopos$freq == "15Hz" & dfTopos$electrode == "Iz"]),
             color = chanCol, size = .25) +
  theme(legend.position = "none")
topoLE15$data$fill <- as.numeric(bin(data = topoLE15$data$fill, nbins = nrColors))
topoLE15$layers <- topoLE15$layers[-c(3,4,5)]



# Joining Plots -----------------------------------------------------------

# Add topographies to spectra
topoFL6 <- topoFL6 + theme(plot.margin = margin(0,0,0,0))
specFL6 <- specFL6 + annotation_custom(ggplotGrob(topoFL6),
                                       xmin = xmin + (3.5*6-xmin)*.60, xmax = 3.5*6,
                                       ymin = yminFL + (ymaxFL-yminFL)*.60, ymax = ymaxFL)
topoFL857 <- topoFL857 + theme(plot.margin = margin(0,0,0,0))
specFL857 <- specFL857 + annotation_custom(ggplotGrob(topoFL857),
                                           xmin = xmin + (3.5*8.57-xmin)*.60, xmax = 3.5*8.57,
                                           ymin = yminFL + (ymaxFL-yminFL)*.60, ymax = ymaxFL)
topoFL15 <- topoFL15 + theme(plot.margin = margin(0,0,0,0))
specFL15 <- specFL15 + annotation_custom(ggplotGrob(topoFL15),
                                         xmin = xmin + (3.5*15-xmin)*.60, xmax = 3.5*15,
                                         ymin = yminFL + (ymaxFL-yminFL)*.60, ymax = ymaxFL)

topoLE6 <- topoLE6 + theme(plot.margin = margin(0,0,0,0))
specLE6 <- specLE6 + annotation_custom(ggplotGrob(topoLE6),
                                       xmin = xmin + (3.5*6-xmin)*.60, xmax = 3.5*6,
                                       ymin = yminLE + (ymaxLE-yminLE)*.60, ymax = ymaxLE)
topoLE857 <- topoLE857 + theme(plot.margin = margin(0,0,0,0))
specLE857 <- specLE857 + annotation_custom(ggplotGrob(topoLE857),
                                           xmin = xmin + (3.5*8.57-xmin)*.60, xmax = 3.5*8.57,
                                           ymin = yminLE + (ymaxLE-yminLE)*.60, ymax = ymaxLE)
topoLE15 <- topoLE15 + theme(plot.margin = margin(0,0,0,0))
specLE15 <- specLE15 + annotation_custom(ggplotGrob(topoLE15),
                                         xmin = xmin + (3.5*15-xmin)*.60, xmax = 3.5*15,
                                         ymin = yminLE + (ymaxLE-yminLE)*.60, ymax = ymaxLE)

# Add margins to single condition plots before joining them
specFL6 <- specFL6 + theme(plot.margin = margin(20,5,10,5))
specFL857 <- specFL857 + theme(plot.margin = margin(20,5,10,5))
specFL15 <- specFL15 + theme(plot.margin = margin(20,5,10,5))
specLE6 <- specLE6 + theme(plot.margin = margin(10,5,20,5))
specLE857 <- specLE857 + theme(plot.margin = margin(10,5,20,5))
specLE15 <- specLE15 + theme(plot.margin = margin(10,5,20,5))

# Join plots and plot in RStudio
specPlots <- ggarrange(specFL6, specFL857, specFL15, specLE6, specLE857, specLE15,
                       nrow = 2, ncol = 3)
specPlots

# Join only topographies plots and plot in RStudio
topoPlots <- ggarrange(topoFL6, topoFL857, topoFL15, topoLE6, topoLE857, topoLE15,
                       labels = c("Florida - 6 Hz", "Florida - 8.57 Hz", "Florida - 15 Hz",
                                  "Leipzig - 6 Hz", "Leipzig - 8.57 Hz", "Leipzig - 15 Hz"),
                       nrow = 2, ncol = 3)
topoPlots
 
# Save plot as jpg & pdf
savename = paste0(parentFolder,"/figures/22b_supp_spectra_commPL_singleTrialFFT_SNRtopos.pdf")
ggsave(filename = savename, plot = specPlots, device = "pdf",
       width = 21, height = 15, unit = "cm", limitsize = FALSE)

savename = paste0(parentFolder,"/figures/22b_supp_spectra_commPL_singleTrialFFT_SNRtopos.jpg")
ggsave(filename = savename, plot = specPlots, device = "jpg",
       width = 21, height = 15, unit = "cm", dpi = 300, limitsize = FALSE)

# topographies only
savename = paste0(parentFolder,"/figures/22b_supp_onlySNRtopos_commPL_singleTrialFFT.pdf")
ggsave(filename = savename, plot = topoPlots, device = "pdf",
       width = 21, height = 15, unit = "cm", limitsize = FALSE)

savename = paste0(parentFolder,"/figures/22b_supp_onlySNRtopos_commPL_singleTrialFFT.jpg")
ggsave(filename = savename, plot = topoPlots, device = "jpg",
       width = 21, height = 15, unit = "cm", dpi = 300, limitsize = FALSE)