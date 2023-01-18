# --- author: Christian Panitz
# --- encoding: en_US.UTF-8
# --- R version: 4.0.3 (2020-10-10) -- "Bunny-Wunnies Freak Out"
# --- RStudio version: 2022.02.3
# --- script version: December 2022
# --- content: plotting topographies for different conditions and labs (common pipeline, single-trial FFT, single-trial SNR)


# Header Parameters -------------------------------------------------------

xmin <- 4
xmax <- 17
fType = "Helvetica"
fSize = 9

topoRes <- 200
chanCol = "black"
nrColors = 8

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




# Topography Plots --------------------------------------------------------

topoRes <- 200
nrColors = 8

# load topographical data
loadname <- paste0(parentFolder, "/dataframes/supp_dfTopos_commPL_singleTrialFFT_singleTrialSNR.csv")
dfTopos <- read.csv(loadname, sep = ",")
# topoplot function needs plotted variable to be called "amplitude", so overwriting amplitude variable with amplitudeSNR values
dfTopos$amplitude <- dfTopos$amplitudeSNR
dfTopos <- subset(dfTopos, select = -amplitudeSNR)

dfToposDiff <- pivot_wider(dfTopos, names_from = mod, values_from = amplitude)
dfToposDiff$amplitude = dfToposDiff$square - dfToposDiff$sine



# determine some minima and maxima
cminFL <- 0
cmaxFL <- max(c(dfTopos$amplitude[dfTopos$lab == "Florida"]))

cminLE <- 0
cmaxLE <- max(c(dfTopos$amplitude[dfTopos$lab == "Leipzig"]))
              
cminFLdiff <- min(dfToposDiff$amplitude[dfToposDiff$lab == "Florida"])
cmaxFLdiff <- max(dfToposDiff$amplitude[dfToposDiff$lab == "Florida"])
cabsFLdiff <- max(abs(c(cminFLdiff,cmaxFLdiff)))

cminLEdiff <- min(dfToposDiff$amplitude[dfToposDiff$lab == "Leipzig"])
cmaxLEdiff <- max(dfToposDiff$amplitude[dfToposDiff$lab == "Leipzig"])
cabsLEdiff <- max(abs(c(cminLEdiff,cmaxLEdiff)))



# Florida
# create individual topographical plots
topoFL6sq <- topoplot(data = dfTopos[dfTopos$lab == "Florida" & dfTopos$freq == "6Hz" & dfTopos$mod == "square",],
                       limits = c(cminFL,cmaxFL), contour = FALSE, grid_res = topoRes, highlights = c("E75", "E81"), chan_marker = "none")

topoFL6sin <- topoplot(data = dfTopos[dfTopos$lab == "Florida" & dfTopos$freq == "6Hz" & dfTopos$mod == "sine",],
                       limits = c(cminFL,cmaxFL), contour = FALSE, grid_res = topoRes, highlights = c("E75", "E81"), chan_marker = "none")

topoFL6diff <- topoplot(data = dfToposDiff[dfToposDiff$lab == "Florida" & dfToposDiff$freq == "6Hz",],
                        limits = c(-cabsFLdiff,cabsFLdiff), contour = FALSE, grid_res = topoRes, highlights = c("E75", "E81"), chan_marker = "none")

topoFL857sq <- topoplot(data = dfTopos[dfTopos$lab == "Florida" & dfTopos$freq == "8.57Hz" & dfTopos$mod == "square",],
                         limits = c(cminFL,cmaxFL), contour = FALSE, grid_res = topoRes, highlights = c("E75", "E81"), chan_marker = "none")

topoFL857sin <- topoplot(data = dfTopos[dfTopos$lab == "Florida" & dfTopos$freq == "8.57Hz" & dfTopos$mod == "sine",],
                         limits = c(cminFL,cmaxFL), contour = FALSE, grid_res = topoRes, highlights = c("E75", "E81"), chan_marker = "none")

topoFL857diff <- topoplot(data = dfToposDiff[dfToposDiff$lab == "Florida" & dfToposDiff$freq == "8.57Hz",],
                          limits = c(-cabsFLdiff,cabsFLdiff), contour = FALSE, grid_res = topoRes, highlights = c("E75", "E81"), chan_marker = "none")

topoFL15sq <- topoplot(data = dfTopos[dfTopos$lab == "Florida" & dfTopos$freq == "15Hz" & dfTopos$mod == "square",],
                        limits = c(cminFL,cmaxFL), contour = FALSE, grid_res = topoRes, highlights = c("E75", "E81"), chan_marker = "none")

topoFL15sin <- topoplot(data = dfTopos[dfTopos$lab == "Florida" & dfTopos$freq == "15Hz" & dfTopos$mod == "sine",],
                        limits = c(cminFL,cmaxFL), contour = FALSE, grid_res = topoRes, highlights = c("E75", "E81"), chan_marker = "none")

topoFL15diff <- topoplot(data = dfToposDiff[dfToposDiff$lab == "Florida" & dfToposDiff$freq == "15Hz",],
                         limits = c(-cabsFLdiff,cabsFLdiff), contour = FALSE, grid_res = topoRes, highlights = c("E75", "E81"), chan_marker = "none")

# change label of colorbar
topoFL6sq$guides$fill$title <- "SNR"
topoFL6sin$guides$fill$title <- "SNR"
topoFL6diff$guides$fill$title <- "SNR"
topoFL857sq$guides$fill$title <- "SNR"
topoFL857sin$guides$fill$title <- "SNR"
topoFL857diff$guides$fill$title <- "SNR"
topoFL15sq$guides$fill$title <- "SNR"
topoFL15sin$guides$fill$title <- "SNR"
topoFL15diff$guides$fill$title <- "SNR"
topoFL6sq$guides$fill$title.hjust <- 0.5
topoFL6sin$guides$fill$title.hjust <- 0.5
topoFL6diff$guides$fill$title.hjust <- 0.5
topoFL857sq$guides$fill$title.hjust <- 0.5
topoFL857sin$guides$fill$title.hjust <- 0.5
topoFL857diff$guides$fill$title.hjust <- 0.5
topoFL15sq$guides$fill$title.hjust <- 0.5
topoFL15sin$guides$fill$title.hjust <- 0.5
topoFL15diff$guides$fill$title.hjust <- 0.5

# binning of data points (for stepwise colors)
# determine number of data points are there in one topography plot
nrPtsFL <- length(topoFL6sq$data$fill)

# join all data from the single conditions
allPtsFLmain <- c(topoFL6sq$data$fill, topoFL6sin$data$fill,
                  topoFL857sq$data$fill,  topoFL857sin$data$fill,
                  topoFL15sq$data$fill, topoFL15sin$data$fill)
# transform them into bins (scaled from 1 to [nrColors])
allPtsFLmain <- as.numeric(bin(allPtsFLmain, nrColors))
# rescale to original min-max range
allPtsFLmain <- (allPtsFLmain-1) / (nrColors-1) * (cmaxFL-cminFL) + cminFL
# overwrite topography data with rescaled values
topoFL6sq$data$fill <- allPtsFLmain[(0*nrPtsFL+1) : (1*nrPtsFL)]
topoFL6sin$data$fill <- allPtsFLmain[(1*nrPtsFL+1) : (2*nrPtsFL)]
topoFL857sq$data$fill <- allPtsFLmain[(2*nrPtsFL+1) : (3*nrPtsFL)]
topoFL857sin$data$fill <- allPtsFLmain[(3*nrPtsFL+1) : (4*nrPtsFL)]
topoFL15sq$data$fill <- allPtsFLmain[(4*nrPtsFL+1) : (5*nrPtsFL)]
topoFL15sin$data$fill <- allPtsFLmain[(5*nrPtsFL+1) : (6*nrPtsFL)]

# join all data from difference topographies
allPtsFLdiff <- c(topoFL6diff$data$fill, topoFL857diff$data$fill, topoFL15diff$data$fill)
# transform them into bins (scaled from 1 to [nrColors])
allPtsFLdiff <- as.numeric(bin(allPtsFLdiff, nrColors))
# rescale to original min-max range
allPtsFLdiff <- (allPtsFLdiff-1) / (nrColors-1) * (cmaxFLdiff-cminFLdiff) + cminFLdiff
# overwrite topography data with rescaled values
topoFL6diff$data$fill <- allPtsFLdiff[(0*nrPtsFL+1) : (1*nrPtsFL)]
topoFL857diff$data$fill <- allPtsFLdiff[(1*nrPtsFL+1) : (2*nrPtsFL)]
topoFL15diff$data$fill <- allPtsFLdiff[(2*nrPtsFL+1) : (3*nrPtsFL)]



# Leipzig
# create individual topographical plots
topoLE6sq <- topoplot(data = dfTopos[dfTopos$lab == "Leipzig" & dfTopos$freq == "6Hz" & dfTopos$mod == "square",],
                       limits = c(cminLE,cmaxLE), contour = FALSE, grid_res = topoRes, highlights = c("Oz", "Iz"), chan_marker = "none")

topoLE6sin <- topoplot(data = dfTopos[dfTopos$lab == "Leipzig" & dfTopos$freq == "6Hz" & dfTopos$mod == "sine",],
                       limits = c(cminLE,cmaxLE), contour = FALSE, grid_res = topoRes, highlights = c("Oz", "Iz"), chan_marker = "none")

topoLE6diff <- topoplot(data = dfToposDiff[dfToposDiff$lab == "Leipzig" & dfToposDiff$freq == "6Hz",],
                        limits = c(-cabsLEdiff,cabsLEdiff), contour = FALSE, grid_res = topoRes, highlights = c("Oz", "Iz"), chan_marker = "none")

topoLE857sq <- topoplot(data = dfTopos[dfTopos$lab == "Leipzig" & dfTopos$freq == "8.57Hz" & dfTopos$mod == "square",],
                         limits = c(cminLE,cmaxLE), contour = FALSE, grid_res = topoRes, highlights = c("Oz", "Iz"), chan_marker = "none")

topoLE857sin <- topoplot(data = dfTopos[dfTopos$lab == "Leipzig" & dfTopos$freq == "8.57Hz" & dfTopos$mod == "sine",],
                         limits = c(cminLE,cmaxLE), contour = FALSE, grid_res = topoRes, highlights = c("Oz", "Iz"), chan_marker = "none")

topoLE857diff <- topoplot(data = dfToposDiff[dfToposDiff$lab == "Leipzig" & dfToposDiff$freq == "8.57Hz",],
                          limits = c(-cabsLEdiff,cabsLEdiff), contour = FALSE, grid_res = topoRes, highlights = c("Oz", "Iz"), chan_marker = "none")

topoLE15sq <- topoplot(data = dfTopos[dfTopos$lab == "Leipzig" & dfTopos$freq == "15Hz" & dfTopos$mod == "square",],
                        limits = c(cminLE,cmaxLE), contour = FALSE, grid_res = topoRes, highlights = c("Oz", "Iz"), chan_marker = "none")

topoLE15sin <- topoplot(data = dfTopos[dfTopos$lab == "Leipzig" & dfTopos$freq == "15Hz" & dfTopos$mod == "sine",],
                        limits = c(cminLE,cmaxLE), contour = FALSE, grid_res = topoRes, highlights = c("Oz", "Iz"), chan_marker = "none")

topoLE15diff <- topoplot(data = dfToposDiff[dfToposDiff$lab == "Leipzig" & dfToposDiff$freq == "15Hz",],
                         limits = c(-cabsLEdiff,cabsLEdiff), contour = FALSE, grid_res = topoRes, highlights = c("Oz", "Iz"), chan_marker = "none")

# change label of colorbar
topoLE6sq$guides$fill$title <- "SNR"
topoLE6sin$guides$fill$title <- "SNR"
topoLE6diff$guides$fill$title <- "SNR"
topoLE857sq$guides$fill$title <- "SNR"
topoLE857sin$guides$fill$title <- "SNR"
topoLE857diff$guides$fill$title <- "SNR"
topoLE15sq$guides$fill$title <- "SNR"
topoLE15sin$guides$fill$title <- "SNR"
topoLE15diff$guides$fill$title <- "SNR"
topoLE6sq$guides$fill$title.hjust <- 0.5
topoLE6sin$guides$fill$title.hjust <- 0.5
topoLE6diff$guides$fill$title.hjust <- 0.5
topoLE857sq$guides$fill$title.hjust <- 0.5
topoLE857sin$guides$fill$title.hjust <- 0.5
topoLE857diff$guides$fill$title.hjust <- 0.5
topoLE15sq$guides$fill$title.hjust <- 0.5
topoLE15sin$guides$fill$title.hjust <- 0.5
topoLE15diff$guides$fill$title.hjust <- 0.5

# determine number of data points are there in one topography plot
nrPtsLE <- length(topoLE6sq$data$fill)

# join all data from the single conditions
allPtsLEmain <- c(topoLE6sq$data$fill, topoLE6sin$data$fill,
                  topoLE857sq$data$fill,  topoLE857sin$data$fill,
                  topoLE15sq$data$fill, topoLE15sin$data$fill)
# transform them into bins (scaled from 1 to [nrColors])
allPtsLEmain <- as.numeric(bin(allPtsLEmain, nrColors))
# rescale to original min-max range
allPtsLEmain <- (allPtsLEmain-1) / (nrColors-1) * (cmaxLE-cminLE) + cminLE
# overwrite topography data with rescaled values
topoLE6sq$data$fill <- allPtsLEmain[(0*nrPtsLE+1) : (1*nrPtsLE)]
topoLE6sin$data$fill <- allPtsLEmain[(1*nrPtsLE+1) : (2*nrPtsLE)]
topoLE857sq$data$fill <- allPtsLEmain[(2*nrPtsLE+1) : (3*nrPtsLE)]
topoLE857sin$data$fill <- allPtsLEmain[(3*nrPtsLE+1) : (4*nrPtsLE)]
topoLE15sq$data$fill <- allPtsLEmain[(4*nrPtsLE+1) : (5*nrPtsLE)]
topoLE15sin$data$fill <- allPtsLEmain[(5*nrPtsLE+1) : (6*nrPtsLE)]

# join all data from difference topographies
allPtsLEdiff <- c(topoLE6diff$data$fill, topoLE857diff$data$fill, topoLE15diff$data$fill)
# transform them into bins (scaled from 1 to [nrColors])
allPtsLEdiff <- as.numeric(bin(allPtsLEdiff, nrColors))
# rescale to original min-max range
allPtsLEdiff <- (allPtsLEdiff-1) / (nrColors-1) * (cmaxLEdiff-cminLEdiff) + cminLEdiff
# overwrite topography data with rescaled values
topoLE6diff$data$fill <- allPtsLEdiff[(0*nrPtsLE+1) : (1*nrPtsLE)]
topoLE857diff$data$fill <- allPtsLEdiff[(1*nrPtsLE+1) : (2*nrPtsLE)]
topoLE15diff$data$fill <- allPtsLEdiff[(2*nrPtsLE+1) : (3*nrPtsLE)]



# join single topographies into plots
toposFL <- ggarrange(topoFL6sq, topoFL6sin, topoFL6diff,
                     topoFL857sq, topoFL857sin, topoFL857diff,
                     topoFL15sq, topoFL15sin, topoFL15diff,
                     nrow = 3, ncol = 3); toposFL

toposLE <- ggarrange(topoLE6sq, topoLE6sin, topoLE6diff,
                     topoLE857sq, topoLE857sin, topoLE857diff,
                     topoLE15sq, topoLE15sin, topoLE15diff,
                     nrow = 3, ncol = 3); toposLE


# save plots
savename = paste0(parentFolder,"/figures/27c_supp_topos_commPL_singleTrialFFT_singleTrialSNR_Florida.pdf")
ggsave(filename = savename, plot = toposFL, device = "pdf",
       width = 21, height = 15, unit = "cm", limitsize = FALSE)

savename = paste0(parentFolder,"/figures/27c_supp_topos_commPL_singleTrialFFT_singleTrialSNR_Florida.jpg")
ggsave(filename = savename, plot = toposFL, device = "jpg",
       width = 21, height = 15, unit = "cm", limitsize = FALSE)

savename = paste0(parentFolder,"/figures/27c_supp_topos_commPL_singleTrialFFT_singleTrialSNR_Leipzig.pdf")
ggsave(filename = savename, plot = toposLE, device = "pdf",
       width = 21, height = 15, unit = "cm", dpi = 300, limitsize = FALSE)

savename = paste0(parentFolder,"/figures/27c_supp_topos_commPL_singleTrialFFT_singleTrialSNR_Leipzig.jpg")
ggsave(filename = savename, plot = toposLE, device = "jpg",
       width = 21, height = 15, unit = "cm", dpi = 300, limitsize = FALSE)
