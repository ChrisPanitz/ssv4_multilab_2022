# --- author: Christian Panitz
# --- encoding: en_US.UTF-8
# --- R version: 4.0.3 (2020-10-10) -- "Bunny-Wunnies Freak Out"
# --- RStudio version: 2022.02.3
# --- script version: July 2022
# --- content: plotting spectra for different conditions


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
nrColors = 8

# load topographical data
loadname <- paste0(parentFolder, "/dataframes/dfTopos_commPL.csv")
dfTopos <- read.csv(loadname, sep = ",")

dfToposDiff <- pivot_wider(dfTopos, names_from = mod, values_from = amplitude)
dfToposDiff$amplitude = dfToposDiff$box - dfToposDiff$sine



cminFL <- 0
cmaxFL <- max(c(dfTopos$amplitude[dfTopos$site == "Florida"]))

cminLE <- 0
cmaxLE <- max(c(dfTopos$amplitude[dfTopos$site == "Leipzig"]))
              
cminFLdiff <- min(dfToposDiff$amplitude[dfToposDiff$site == "Florida"])
cmaxFLdiff <- max(dfToposDiff$amplitude[dfToposDiff$site == "Florida"])
cabsFLdiff <- max(abs(c(cminFLdiff,cmaxFLdiff)))

cminLEdiff <- min(dfToposDiff$amplitude[dfToposDiff$site == "Leipzig"])
cmaxLEdiff <- max(dfToposDiff$amplitude[dfToposDiff$site == "Leipzig"])
cabsLEdiff <- max(abs(c(cminLEdiff,cmaxLEdiff)))


# create individual topographical plots
topoFL6box <- topoplot(data = dfTopos[dfTopos$site == "Florida" & dfTopos$freq == "6Hz" & dfTopos$mod == "box",],
                       limits = c(cminFL,cmaxFL), contour = FALSE, grid_res = topoRes, highlights = c("E75", "E81"), chan_marker = "none")

topoFL6sin <- topoplot(data = dfTopos[dfTopos$site == "Florida" & dfTopos$freq == "6Hz" & dfTopos$mod == "sine",],
                       limits = c(cminFL,cmaxFL), contour = FALSE, grid_res = topoRes, highlights = c("E75", "E81"), chan_marker = "none")

topoFL6diff <- topoplot(data = dfToposDiff[dfToposDiff$site == "Florida" & dfToposDiff$freq == "6Hz",],
                        limits = c(-cabsFLdiff,cabsFLdiff), contour = FALSE, grid_res = topoRes, highlights = c("E75", "E81"), chan_marker = "none")

topoFL857box <- topoplot(data = dfTopos[dfTopos$site == "Florida" & dfTopos$freq == "8.57Hz" & dfTopos$mod == "box",],
                         limits = c(cminFL,cmaxFL), contour = FALSE, grid_res = topoRes, highlights = c("E75", "E81"), chan_marker = "none")

topoFL857sin <- topoplot(data = dfTopos[dfTopos$site == "Florida" & dfTopos$freq == "8.57Hz" & dfTopos$mod == "sine",],
                         limits = c(cminFL,cmaxFL), contour = FALSE, grid_res = topoRes, highlights = c("E75", "E81"), chan_marker = "none")

topoFL857diff <- topoplot(data = dfToposDiff[dfToposDiff$site == "Florida" & dfToposDiff$freq == "8.57Hz",],
                          limits = c(-cabsFLdiff,cabsFLdiff), contour = FALSE, grid_res = topoRes, highlights = c("E75", "E81"), chan_marker = "none")

topoFL15box <- topoplot(data = dfTopos[dfTopos$site == "Florida" & dfTopos$freq == "15Hz" & dfTopos$mod == "box",],
                        limits = c(cminFL,cmaxFL), contour = FALSE, grid_res = topoRes, highlights = c("E75", "E81"), chan_marker = "none")

topoFL15sin <- topoplot(data = dfTopos[dfTopos$site == "Florida" & dfTopos$freq == "15Hz" & dfTopos$mod == "sine",],
                        limits = c(cminFL,cmaxFL), contour = FALSE, grid_res = topoRes, highlights = c("E75", "E81"), chan_marker = "none")

topoFL15diff <- topoplot(data = dfToposDiff[dfToposDiff$site == "Florida" & dfToposDiff$freq == "15Hz",],
                         limits = c(-cabsFLdiff,cabsFLdiff), contour = FALSE, grid_res = topoRes, highlights = c("E75", "E81"), chan_marker = "none")





topoLE6box <- topoplot(data = dfTopos[dfTopos$site == "Leipzig" & dfTopos$freq == "6Hz" & dfTopos$mod == "box",],
                       limits = c(cminLE,cmaxLE), contour = FALSE, grid_res = topoRes, highlights = c("Oz", "Iz"), chan_marker = "none")

topoLE6sin <- topoplot(data = dfTopos[dfTopos$site == "Leipzig" & dfTopos$freq == "6Hz" & dfTopos$mod == "sine",],
                       limits = c(cminLE,cmaxLE), contour = FALSE, grid_res = topoRes, highlights = c("Oz", "Iz"), chan_marker = "none")

topoLE6diff <- topoplot(data = dfToposDiff[dfToposDiff$site == "Leipzig" & dfToposDiff$freq == "6Hz",],
                        limits = c(-cabsLEdiff,cabsLEdiff), contour = FALSE, grid_res = topoRes, highlights = c("Oz", "Iz"), chan_marker = "none")

topoLE857box <- topoplot(data = dfTopos[dfTopos$site == "Leipzig" & dfTopos$freq == "8.57Hz" & dfTopos$mod == "box",],
                         limits = c(cminLE,cmaxLE), contour = FALSE, grid_res = topoRes, highlights = c("Oz", "Iz"), chan_marker = "none")

topoLE857sin <- topoplot(data = dfTopos[dfTopos$site == "Leipzig" & dfTopos$freq == "8.57Hz" & dfTopos$mod == "sine",],
                         limits = c(cminLE,cmaxLE), contour = FALSE, grid_res = topoRes, highlights = c("Oz", "Iz"), chan_marker = "none")

topoLE857diff <- topoplot(data = dfToposDiff[dfToposDiff$site == "Leipzig" & dfToposDiff$freq == "8.57Hz",],
                          limits = c(-cabsLEdiff,cabsLEdiff), contour = FALSE, grid_res = topoRes, highlights = c("Oz", "Iz"), chan_marker = "none")

topoLE15box <- topoplot(data = dfTopos[dfTopos$site == "Leipzig" & dfTopos$freq == "15Hz" & dfTopos$mod == "box",],
                        limits = c(cminLE,cmaxLE), contour = FALSE, grid_res = topoRes, highlights = c("Oz", "Iz"), chan_marker = "none")

topoLE15sin <- topoplot(data = dfTopos[dfTopos$site == "Leipzig" & dfTopos$freq == "15Hz" & dfTopos$mod == "sine",],
                        limits = c(cminLE,cmaxLE), contour = FALSE, grid_res = topoRes, highlights = c("Oz", "Iz"), chan_marker = "none")

topoLE15diff <- topoplot(data = dfToposDiff[dfToposDiff$site == "Leipzig" & dfToposDiff$freq == "15Hz",],
                         limits = c(-cabsLEdiff,cabsLEdiff), contour = FALSE, grid_res = topoRes, highlights = c("Oz", "Iz"), chan_marker = "none")


toposFL <- ggarrange(topoFL6box, topoFL6sin, topoFL6diff,
                     topoFL857box, topoFL857sin, topoFL857diff,
                     topoFL15box, topoFL15sin, topoFL15diff,
                     nrow = 3, ncol = 3); toposFL

toposLE <- ggarrange(topoLE6box, topoLE6sin, topoLE6diff,
                     topoLE857box, topoLE857sin, topoLE857diff,
                     topoLE15box, topoLE15sin, topoLE15diff,
                     nrow = 3, ncol = 3); toposLE


savename = paste0(parentFolder,"/figures/15a_topos_commPL_Florida.pdf")
ggsave(filename = savename, plot = toposFL, device = "pdf",
       width = 21, height = 15, unit = "cm", limitsize = FALSE)

savename = paste0(parentFolder,"/figures/15a_topos_commPL_Florida.jpg")
ggsave(filename = savename, plot = toposFL, device = "jpg",
       width = 21, height = 15, unit = "cm", limitsize = FALSE)

savename = paste0(parentFolder,"/figures/15b_topos_commPL_Leipzig.pdf")
ggsave(filename = savename, plot = toposLE, device = "pdf",
       width = 21, height = 15, unit = "cm", dpi = 300, limitsize = FALSE)

savename = paste0(parentFolder,"/figures/15b_topos_commPL_Leipzig.jpg")
ggsave(filename = savename, plot = toposLE, device = "jpg",
       width = 21, height = 15, unit = "cm", dpi = 300, limitsize = FALSE)
