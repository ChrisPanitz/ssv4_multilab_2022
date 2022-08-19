# --- author: Christian Panitz
# --- encoding: en_US.UTF-8
# --- R version: 4.0.3 (2020-10-10) -- "Bunny-Wunnies Freak Out"
# --- RStudio version: 2022.02.3
# --- script version: August 2022
# --- content: plotting group stats for different conditions in aggregated sample (common pipeline)


# Header Parameters -------------------------------------------------------

xminSpec <- 4 
xmaxSpec <- 17
fType = "Helvetica"
fSize = 9



# Loading Packages & Data, Setting Path ----------------------------------

# load packages
library(here)
library(psych)
library(ggplot2)
library(RColorBrewer)
library(ggpubr)

# set parent folder
parentFolder <- here()

# load stats data to plot
loadname <- paste0(parentFolder,"/dataframes/dfSSVEP_commPL.csv")
dfStats <- read.csv(loadname)

dfStats$part <- factor(dfStats$part)
dfStats$lab <- factor(dfStats$lab, levels = c("Florida","Leipzig"), labels = c("Florida", "Leipzig"))
dfStats$freq <- factor(dfStats$freq, levels = c("6Hz","8.57Hz","15Hz"), labels = c("6Hz","8.57Hz","15Hz"))
dfStats$mod <- factor(dfStats$mod, levels = c("square","sine"), labels = c("square","sine"))

# load spectral data to plot
loadname <- paste0(parentFolder,"/dataframes/dfSpectra_commPL_jointLabs.csv")
dfSpectra <- read.csv(loadname)
dfSpectra$freq <- factor(dfSpectra$freq, levels = c("6Hz","8.57Hz","15Hz"), labels = c("6Hz","8.57Hz","15Hz"))
dfSpectra$mod <- factor(dfSpectra$mod, levels = c("square","sine"), labels = c("square","sine"))

# overwrite DC component amplitude with 0 (for y axis scaling)
dfSpectra$amp[dfSpectra$freqBin == 0] = 0



# Single Spectra Plots ----------------------------------------------------

# set axis limits and frequency resolution (for bar width)
yminSpec <- 0
ymaxSpec <- max(dfSpectra$amp) * 1.1
res <- diff(dfSpectra$freqBin)[1]

# Create bar plots with vertical, dashed line at driving frequency;
# One plot per Lab and Driving Frequency with the two modulation functions in each plot
spec6 <- ggplot(data = dfSpectra[dfSpectra$freq == "6Hz",], 
                aes(x = freqBin, y = amp, fill = mod)) + theme_classic() +
  geom_col(data = dfSpectra[dfSpectra$freq == "6Hz" & dfSpectra$mod == "square",], 
           width = res) +
  geom_col(data = dfSpectra[dfSpectra$freq == "6Hz" & dfSpectra$mod == "sine",],
           width = res, alpha = .80) +
  #geom_vline(xintercept = 6, color = "gray70", linetype = "dashed", size = .2) +
  scale_x_continuous(name = " ", limits = c(xminSpec,xmaxSpec), breaks = c(6,8.57,15), labels = c("6","8.57","15")) +
  scale_y_continuous(name = "Normalized spectral amplitude", limits = c(yminSpec,ymaxSpec)) +
  scale_fill_manual(values = brewer.pal(n = 4, "Purples")[4:3], breaks = c("square","sine")) +
  labs(title = "6 Hz") +
  theme(
    plot.title = element_text(size = fSize, color = "black", face = "bold", hjust = .5),
    axis.title.x = element_text(family = fType, color = "black", size = fSize,
                                face = "bold", margin = margin(t = 10)),
    axis.text.x = element_text(family = fType, color = "black", size = fSize),
    axis.text.y = element_text(family = fType, color = "black", size = fSize),
    axis.title.y = element_text(family = fType, color = "black", size = fSize,
                                face = "bold", margin = margin(r = 20)),
    legend.position = c(1,1),
    legend.justification = c(1,1),
    legend.title = element_blank(),
    legend.text = element_text(family = fType, color = "black", size = fSize-2),
    legend.key.size = unit(fSize-2, "pt")
  )

spec857 <- ggplot(data = dfSpectra[dfSpectra$freq == "8.57Hz",], 
                  aes(x = freqBin, y = amp, fill = mod)) + theme_classic() +
  geom_col(data = dfSpectra[dfSpectra$freq == "8.57Hz" & dfSpectra$mod == "square",],
           width = res) +
  geom_col(data = dfSpectra[dfSpectra$freq == "8.57Hz" & dfSpectra$mod == "sine",],
           width = res, alpha = .80) +
  #geom_vline(xintercept = 60/7, color = "gray70", linetype = "dashed", size = .2) +
  scale_x_continuous(name = " ", limits = c(xminSpec,xmaxSpec), breaks = c(6,8.57,15), labels = c("6","8.57","15")) +
  scale_y_continuous(name = " ", limits = c(yminSpec,ymaxSpec)) +
  scale_fill_manual(values = brewer.pal(n = 4, "Oranges")[4:3], breaks = c("square","sine")) +
  labs(title = "8.57 Hz") +
  theme(
    plot.title = element_text(size = fSize, color = "black", face = "bold", hjust = .5),
    axis.title.x = element_text(family = fType, color = "black", size = fSize,
                                face = "bold", margin = margin(t = 10)),
    axis.text.x = element_text(family = fType, color = "black", size = fSize),
    axis.text.y = element_text(family = fType, color = "black", size = fSize),
    axis.title.y = element_text(family = fType, color = "black", size = fSize,
                                face = "bold", margin = margin(r = 20)),
    legend.position = c(1,1),
    legend.justification = c(1,1),
    legend.title = element_blank(),
    legend.text = element_text(family = fType, color = "black", size = fSize-2),
    legend.key.size = unit(fSize-2, "pt")
  )

spec15 <- ggplot(data = dfSpectra[dfSpectra$freq == "15Hz",], 
                 aes(x = freqBin, y = amp, fill = mod)) + theme_classic() +
  geom_col(data = dfSpectra[dfSpectra$freq == "15Hz" & dfSpectra$mod == "square",],
           width = res) +
  geom_col(data = dfSpectra[dfSpectra$freq == "15Hz" & dfSpectra$mod == "sine",],
           width = res, alpha = .80) +
  #geom_vline(xintercept = 15, color = "gray70", linetype = "dashed", size = .2) +
  scale_x_continuous(name = " ", limits = c(xminSpec,xmaxSpec), breaks = c(6,8.57,15), labels = c("6","8.57","15")) +
  scale_y_continuous(name = " ", limits = c(yminSpec,ymaxSpec)) +
  scale_fill_manual(values = brewer.pal(n = 4, "BuGn")[4:3], breaks = c("square","sine")) +
  labs(title = "15 Hz") +
  theme(
    plot.title = element_text(size = fSize, color = "black", face = "bold", hjust = .5),
    axis.title.x = element_text(family = fType, color = "black", size = fSize,
                                face = "bold", margin = margin(t = 10)),
    axis.text.x = element_text(family = fType, color = "black", size = fSize),
    axis.text.y = element_text(family = fType, color = "black", size = fSize),
    axis.title.y = element_text(family = fType, color = "black", size = fSize,
                                face = "bold", margin = margin(r = 20)),
    legend.position = c(1,1),
    legend.justification = c(1,1),
    legend.title = element_blank(),
    legend.text = element_text(family = fType, color = "black", size = fSize-2),
    legend.key.size = unit(fSize-2, "pt")
  )



# Plots with Individual and Mean ssVEPs -----------------------------------

# set y axis limits
ymin = min(dfStats$ssvepZ)
ymax = max(dfStats$ssvepZ)

# compute dataset with valus adjusted for participants' means to eliminate btw-subject variance
# can be used for standard errors but was not used in the end becuase because of small values
# can be used by uncommenting the "geom_errorbar" lines
dfStatsWithin <- dfStats

for (i in 1:length(levels(dfStats$part))) {
  dfStatsWithin$ssvep[dfStats$part == levels(dfStats$part)[i]] <- dfStats$ssvep[dfStats$part == levels(dfStats$part)[i]] - mean(dfStats$ssvep[dfStats$part == levels(dfStats$part)[i]]) + mean(dfStats$ssvep)
}

# Create dataframe with means and sems for each condition (SEMs not used here)
dfGroupedStats <- data.frame(
  freq = factor(c("6Hz","6Hz","8.57Hz","8.57Hz","15Hz","15Hz"), levels = c("6Hz","8.57Hz","15Hz")),
  mod = factor(rep(c("square","sine"),3), levels = c("square","sine")),
  mean = rep(0,6),
  sem = rep(0,6)
)

groupedStats <- describeBy(dfStats, group = c("mod","freq"))

for (i in 1:nrow(dfGroupedStats)) {
  dfGroupedStats$mean[i] <- groupedStats[[i]]$mean[6]
  dfGroupedStats$sem[i] <- groupedStats[[i]]$se[6]
  #dfGroupedStats$mean[i] <- groupedStats[[i]]$mean[5]
  #dfGroupedStats$sem[i] <- groupedStats[[i]]$se[5]
}

# Plotting individual values and means, the two Mod conditions connected by line

plot6 <- ggplot() + theme_classic() +
  geom_line(data = dfStats[dfStats$freq == "6Hz",], 
            aes(x = mod, y = ssvepZ, group = part), color = "gray70") +
  geom_point(data = dfStats[dfStats$freq == "6Hz",], 
             aes(x = mod, y = ssvepZ), color = "gray70") +
  geom_segment(data = dfGroupedStats, x = 1, xend = 2,
               aes(y = mean[freq == "6Hz" & mod == "square"],
                   yend = mean[freq == "6Hz" & mod == "sine"]), 
               color = "black") +
  #geom_errorbar(data = dfGroupedStats[dfGroupedStats$freq == "6Hz",], aes(x = mod, color = mod, ymin = mean-sem, ymax = mean+sem), width = .1) +
  geom_point(data = dfGroupedStats[dfGroupedStats$freq == "6Hz",],
             aes (x = mod, color = mod, y = mean), shape = "diamond", size = 4) +
  scale_color_manual(values = brewer.pal(n = 4, "Purples")[4:3]) +
  scale_y_continuous(name = "ssVEP amplitude (z)", limits = c(ymin,ymax)) +
  labs(title = " ") +
  theme(
    plot.title = element_text(size = fSize, color = "black", face = "bold", hjust = .5),
    axis.title.x = element_blank(),
    axis.text.x = element_text(family = fType, color = "black", size = fSize, face = "bold"),
    axis.ticks.x = element_blank(),
    axis.text.y = element_text(family = fType, color = "black", size = fSize),
    axis.title.y = element_text(family = fType, color = "black", size = fSize,
                                face = "bold", margin = margin(r = 20)),
    legend.position = "none"
  )

plot857 <- ggplot() + theme_classic() +
  geom_line(data = dfStats[dfStats$freq == "8.57Hz",], 
            aes(x = mod, y = ssvepZ, group = part), color = "gray70") +
  geom_point(data = dfStats[dfStats$freq == "8.57Hz",], 
             aes(x = mod, y = ssvepZ), color = "gray70") +
  geom_segment(data = dfGroupedStats, x = 1, xend = 2,
               aes(y = mean[freq == "8.57Hz" & mod == "square"],
                   yend = mean[freq == "8.57Hz" & mod == "sine"]), 
               color = "black") +
  #geom_errorbar(data = dfGroupedStats[dfGroupedStats$freq == "8.57Hz",], aes(x = mod, color = mod, ymin = mean-sem, ymax = mean+sem), width = .1) +
  geom_point(data = dfGroupedStats[dfGroupedStats$freq == "8.57Hz",],
             aes (x = mod, color = mod, y = mean), shape = "diamond", size = 4) +
  scale_color_manual(values = brewer.pal(n = 4, "Oranges")[4:3]) +
  scale_y_continuous(name = " ", limits = c(ymin,ymax)) +
  labs(title = " ") +
  theme(
    plot.title = element_text(size = fSize, color = "black", face = "bold", hjust = .5),
    axis.title.x = element_blank(),
    axis.text.x = element_text(family = fType, color = "black", size = fSize, face = "bold"),
    axis.ticks.x = element_blank(),
    axis.text.y = element_text(family = fType, color = "black", size = fSize),
    axis.title.y = element_text(family = fType, color = "black", size = fSize,
                                face = "bold", margin = margin(r = 20)),
    legend.position = "none"
  )

plot15 <- ggplot() + theme_classic() +
  geom_line(data = dfStats[dfStats$freq == "15Hz",], 
            aes(x = mod, y = ssvepZ, group = part), color = "gray70") +
  geom_point(data = dfStats[dfStats$freq == "15Hz",], 
             aes(x = mod, y = ssvepZ), color = "gray70") +
  geom_segment(data = dfGroupedStats, x = 1, xend = 2,
               aes(y = mean[freq == "15Hz" & mod == "square"],
               yend = mean[freq == "15Hz" & mod == "sine"]), 
               color = "black") +
  #geom_errorbar(data = dfGroupedStats[dfGroupedStats$freq == "15Hz",], aes(x = mod, color = mod, ymin = mean-sem, ymax = mean+sem), width = .1) +
  geom_point(data = dfGroupedStats[dfGroupedStats$freq == "15Hz",],
             aes (x = mod, color = mod, y = mean), shape = "diamond", size = 4) +
  scale_color_manual(values = brewer.pal(n = 4, "BuGn")[4:3]) +
  scale_y_continuous(name = " ", limits = c(ymin,ymax)) +
  labs(title = " ") +
  theme(
    plot.title = element_text(size = fSize, color = "black", face = "bold", hjust = .5),
    axis.title.x = element_blank(),
    axis.text.x = element_text(family = fType, color = "black", size = fSize, face = "bold"),
    axis.ticks.x = element_blank(),
    axis.text.y = element_text(family = fType, color = "black", size = fSize),
    axis.title.y = element_text(family = fType, color = "black", size = fSize,
                                face = "bold", margin = margin(r = 20)),
    legend.position = "none"
  )



# Putting Plots Together --------------------------------------------------

# manually set significance symbols and compute means of means  for each plot
sigLabels = c("***","***","*")
mAcrossMod = c(mean(dfGroupedStats$mean[dfGroupedStats$freq == "6Hz"]),
               mean(dfGroupedStats$mean[dfGroupedStats$freq == "8.57Hz"]),
               mean(dfGroupedStats$mean[dfGroupedStats$freq == "15Hz"]))

# add margins and significance symbols to single plots before joining them
spec6 <- spec6 + theme(plot.margin = margin(20,5,10,5))
spec857 <- spec857 + theme(plot.margin = margin(20,5,10,5))
spec15 <- spec15 + theme(plot.margin = margin(20,5,10,5))

plot6 <- plot6 + 
         theme(plot.margin = margin(10,5,20,5)) +
         geom_text(data = dfGroupedStats,
                   x = 1.5,y = .98*ymax, label = sigLabels[1],
                   size = fSize*.66)
plot857 <- plot857 +
           theme(plot.margin = margin(10,5,20,5)) +
           geom_text(data = dfGroupedStats,
                     x = 1.5,y = .98*ymax, label = sigLabels[2],
                     size = fSize*.66)
plot15 <- plot15 +
          theme(plot.margin = margin(10,5,20,5)) +
          geom_text(data = dfGroupedStats,
                    x = 1.5,y = .98*ymax, label = sigLabels[3],
                    size = fSize*.66)

# join plots and show them in RStudio
jointPlots <- ggarrange(spec6, spec857, spec15, plot6, plot857, plot15,
                        nrow = 2, ncol = 3,
                        labels = c("A","","","B","",""),
                        font.label = list(size = 20, family = fType))
jointPlots

# Save plot asa jpg and pdf
savename = paste0(parentFolder,"/figures/16_means_commPL_jointLabs.pdf")
ggsave(filename = savename, plot = jointPlots, device = "pdf",
       width = 21, height = 15, unit = "cm", limitsize = FALSE)

savename = paste0(parentFolder,"/figures/16_means_commPL_jointLabs.jpg")
ggsave(filename = savename, plot = jointPlots, device = "jpg",
       width = 21, height = 15, unit = "cm", dpi = 300, limitsize = FALSE)


