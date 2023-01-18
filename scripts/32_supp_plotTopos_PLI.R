# --- author: Christian Panitz
# --- encoding: en_US.UTF-8
# --- R version: 4.0.3 (2020-10-10) -- "Bunny-Wunnies Freak Out"
# --- RStudio version: 2022.02.3
# --- script version: December 2022
# --- content: plotting PLI topographies for different conditions and labs (common pipeline)


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



# Topography Plots --------------------------------------------------------

topoRes <- 200
chanCol = "black"
nrColors = 8

# load topographical data
loadname <- paste0(parentFolder, "/dataframes/supp_PLI_dfTopos.csv")
dfTopos <- read.csv(loadname, sep = ",")

# collapse topographical data across modulation functions
dfTopos <- pivot_wider(data = dfTopos,
                       id_cols = c(lab, freq, electrode, x, y),
                       names_from = mod,
                       values_from = amplitude
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
  geom_point(aes(x = dfTopos$x[dfTopos$lab == "Florida" & dfTopos$freq == "15Hz" & dfTopos$electrode == "E81"],
                 y = dfTopos$y[dfTopos$lab == "Florida" & dfTopos$freq == "15Hz" & dfTopos$electrode == "E81"]),
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
  geom_point(aes(x = dfTopos$x[dfTopos$lab == "Florida" & dfTopos$freq == "15Hz" & dfTopos$electrode == "E81"],
                 y = dfTopos$y[dfTopos$lab == "Florida" & dfTopos$freq == "15Hz" & dfTopos$electrode == "E81"]),
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
  geom_point(aes(x = dfTopos$x[dfTopos$lab == "Florida" & dfTopos$freq == "15Hz" & dfTopos$electrode == "E81"],
                 y = dfTopos$y[dfTopos$lab == "Florida" & dfTopos$freq == "15Hz" & dfTopos$electrode == "E81"]),
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

topoFL6 <- topoFL6 + theme(plot.margin = margin(20,5,10,5))
topoFL857 <- topoFL857 + theme(plot.margin = margin(20,5,10,5))
topoFL15 <- topoFL15 + theme(plot.margin = margin(20,5,10,5))
topoLE6 <- topoLE6 + theme(plot.margin = margin(10,5,20,5))
topoLE857 <- topoLE857 + theme(plot.margin = margin(10,5,20,5))
topoLE15 <- topoLE15 + theme(plot.margin = margin(10,5,20,5))

# Join plots and plot in RStudio
# specPlots <- ggarrange(specFL6, specFL857, specFL15, specLE6, specLE857, specLE15,
#                        nrow = 2, ncol = 3)
# specPlots
topoPlots <- ggarrange(topoFL6, topoFL857, topoFL15, topoLE6, topoLE857, topoLE15,
                       nrow = 2, ncol = 3)
topoPlots


# Add margins to single condition plots before joining them 
topoFL6 <- topoFL6 + theme(plot.margin = margin(20,5,10,5))
topoFL857 <- topoFL857 + theme(plot.margin = margin(20,5,10,5))
topoFL15 <- topoFL15 + theme(plot.margin = margin(20,5,10,5))
topoLE6 <- topoLE6 + theme(plot.margin = margin(10,5,20,5))
topoLE857 <- topoLE857 + theme(plot.margin = margin(10,5,20,5))
topoLE15 <- topoLE15 + theme(plot.margin = margin(10,5,20,5))

# Join topographies plots and plot in RStudio
topoPlots <- ggarrange(topoFL6, topoFL857, topoFL15, topoLE6, topoLE857, topoLE15,
                       labels = c("Florida - 6 Hz", "Florida - 8.57 Hz", "Florida - 15 Hz",
                                  "Leipzig - 6 Hz", "Leipzig - 8.57 Hz", "Leipzig - 15 Hz"),
                       nrow = 2, ncol = 3)
topoPlots

# Save plot as jpg & pdf
savename = paste0(parentFolder,"/figures/32_supp_PLI_topos.pdf")
ggsave(filename = savename, plot = topoPlots, device = "pdf",
       width = 21, height = 15, unit = "cm", limitsize = FALSE)

savename = paste0(parentFolder,"/figures/32_supp_PLI_topos.jpg")
ggsave(filename = savename, plot = topoPlots, device = "jpg",
       width = 21, height = 15, unit = "cm", dpi = 300, limitsize = FALSE)