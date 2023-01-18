# --- author: Christian Panitz
# --- encoding: en_US.UTF-8
# --- R version: 4.0.3 (2020-10-10) -- "Bunny-Wunnies Freak Out"
# --- RStudio version: 2022.02.3
# --- script version: December 2022
# --- content: plotting group stats (amplitudes and SNR) for different conditions in aggregated sample (common pipeline)


# Header Parameters -------------------------------------------------------

xminSpec <- 4 
xmaxSpec <- 31
fType = "Helvetica"
fSize = 9



# Loading Packages & Data, Setting Path ----------------------------------

# load packages
library(here)
library(tidyr)
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
                aes(x = freqBin, y = amp, fill = mod, color = mod)) + theme_classic() +
  geom_col(data = dfSpectra[dfSpectra$freq == "6Hz" & dfSpectra$mod == "square",], 
           width = res) +
  geom_col(data = dfSpectra[dfSpectra$freq == "6Hz" & dfSpectra$mod == "sine",],
           width = res, alpha = .50) +
  #geom_vline(xintercept = 6, color = "gray70", linetype = "dashed", size = .2) +
  scale_x_continuous(name = "Frequency (Hz)", limits = c(xminSpec,xmaxSpec), breaks = seq(6,xmaxSpec,6), labels = as.character(seq(6,xmaxSpec,6))) +
  scale_y_continuous(name = "Normalized spectral\namplitude of CSD", limits = c(yminSpec,ymaxSpec)) +
  scale_fill_manual(values = brewer.pal(n = 4, "Purples")[4:3], breaks = c("square","sine")) +
  scale_color_manual(values = brewer.pal(n = 4, "Purples")[4:3], breaks = c("square","sine")) +
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
                  aes(x = freqBin, y = amp, fill = mod, color = mod)) + theme_classic() +
  geom_col(data = dfSpectra[dfSpectra$freq == "8.57Hz" & dfSpectra$mod == "square",],
           width = res) +
  geom_col(data = dfSpectra[dfSpectra$freq == "8.57Hz" & dfSpectra$mod == "sine",],
           width = res, alpha = .50) +
  #geom_vline(xintercept = 60/7, color = "gray70", linetype = "dashed", size = .2) +
  scale_x_continuous(name = "Frequency (Hz)", limits = c(xminSpec,xmaxSpec), breaks = seq(8.57,xmaxSpec,8.57), labels = as.character(seq(8.57,xmaxSpec,8.57))) +
  scale_y_continuous(name = " ", limits = c(yminSpec,ymaxSpec)) +
  scale_fill_manual(values = brewer.pal(n = 4, "Oranges")[4:3], breaks = c("square","sine")) +
  scale_color_manual(values = brewer.pal(n = 4, "Oranges")[4:3], breaks = c("square","sine")) +
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
                 aes(x = freqBin, y = amp, fill = mod, color = mod)) + theme_classic() +
  geom_col(data = dfSpectra[dfSpectra$freq == "15Hz" & dfSpectra$mod == "square",],
           width = res) +
  geom_col(data = dfSpectra[dfSpectra$freq == "15Hz" & dfSpectra$mod == "sine",],
           width = res, alpha = .50) +
  #geom_vline(xintercept = 15, color = "gray70", linetype = "dashed", size = .2) +
  scale_x_continuous(name = "Frequency (Hz)", limits = c(xminSpec,xmaxSpec), breaks = seq(15,xmaxSpec,15), labels = as.character(seq(15,xmaxSpec,15))) +
  scale_y_continuous(name = " ", limits = c(yminSpec,ymaxSpec)) +
  scale_fill_manual(values = brewer.pal(n = 4, "BuGn")[4:3], breaks = c("square","sine")) +
  scale_color_manual(values = brewer.pal(n = 4, "BuGn")[4:3], breaks = c("square","sine")) +
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



# Plots with Individual and Mean ssVEP amplitudes --------------------------
# set y axis limits
ymin = min(dfStats$ssvepZ)
ymax = max(dfStats$ssvepZ)

# reverse order of square & sine level on Mod factor (easier for plotting)
dfStats$mod <- factor(dfStats$mod, levels = c("sine","square"), labels = c("sine","square"))

# compute dataset with valus adjusted for participants' means to eliminate btw-subject variance
# can be used for standard errors but was not used in the end becuase because of small values
# can be used by uncommenting the "geom_errorbar" lines
dfStatsWithin <- dfStats

for (i in 1:length(levels(dfStats$part))) {
  dfStatsWithin$ssvep[dfStats$part == levels(dfStats$part)[i]] <- 
    dfStats$ssvepZ[dfStats$part == levels(dfStats$part)[i]] - 
    mean(dfStats$ssvepZ[dfStats$part == levels(dfStats$part)[i]]) + 
    mean(dfStats$ssvepZ)
}

# Create dataframe with means and sems for each condition (SEMs not used here)
dfGroupedStats <- data.frame(
  freq = factor(c("6Hz","6Hz","8.57Hz","8.57Hz","15Hz","15Hz"), levels = c("6Hz","8.57Hz","15Hz")),
  mod = factor(rep(c("sine","square"),3), levels = c("sine","square")),
  mean = rep(0,6),
  sem = rep(0,6)
)

groupedStats <- describeBy(dfStats, group = c("mod","freq"))

for (i in 1:nrow(dfGroupedStats)) {
  dfGroupedStats$mean[i] <- groupedStats[[i]]$mean[7]
  dfGroupedStats$sem[i] <- groupedStats[[i]]$se[7]
}

# Plotting individual values and means, the two Mod conditions connected by line

plot6Amp <- ggplot() + theme_classic() +
  geom_line(data = dfStats[dfStats$freq == "6Hz",], 
            aes(x = mod, y = ssvepZ, group = part), color = "gray70") +
  geom_point(data = dfStats[dfStats$freq == "6Hz",], 
             aes(x = mod, y = ssvepZ), color = "gray70") +
  geom_segment(data = dfGroupedStats, x = 1, xend = 2,
               aes(y = mean[freq == "6Hz" & mod == "sine"],
                   yend = mean[freq == "6Hz" & mod == "square"]), 
               color = "black") +
  #geom_errorbar(data = dfGroupedStats[dfGroupedStats$freq == "6Hz",], aes(x = mod, color = mod, ymin = mean-sem, ymax = mean+sem), width = .1) +
  geom_point(data = dfGroupedStats[dfGroupedStats$freq == "6Hz",],
             aes (x = mod, color = mod, y = mean), shape = "diamond", size = 4) +
  scale_color_manual(values = brewer.pal(n = 4, "Purples")[3:4]) +
  scale_y_continuous(name = " \nssVEP amplitude (z)", limits = c(ymin,ymax)) +
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

plot857Amp <- ggplot() + theme_classic() +
  geom_line(data = dfStats[dfStats$freq == "8.57Hz",], 
            aes(x = mod, y = ssvepZ, group = part), color = "gray70") +
  geom_point(data = dfStats[dfStats$freq == "8.57Hz",], 
             aes(x = mod, y = ssvepZ), color = "gray70") +
  geom_segment(data = dfGroupedStats, x = 1, xend = 2,
               aes(y = mean[freq == "8.57Hz" & mod == "sine"],
                   yend = mean[freq == "8.57Hz" & mod == "square"]), 
               color = "black") +
  #geom_errorbar(data = dfGroupedStats[dfGroupedStats$freq == "8.57Hz",], aes(x = mod, color = mod, ymin = mean-sem, ymax = mean+sem), width = .1) +
  geom_point(data = dfGroupedStats[dfGroupedStats$freq == "8.57Hz",],
             aes (x = mod, color = mod, y = mean), shape = "diamond", size = 4) +
  scale_color_manual(values = brewer.pal(n = 4, "Oranges")[3:4]) +
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

plot15Amp <- ggplot() + theme_classic() +
  geom_line(data = dfStats[dfStats$freq == "15Hz",], 
            aes(x = mod, y = ssvepZ, group = part), color = "gray70") +
  geom_point(data = dfStats[dfStats$freq == "15Hz",], 
             aes(x = mod, y = ssvepZ), color = "gray70") +
  geom_segment(data = dfGroupedStats, x = 1, xend = 2,
               aes(y = mean[freq == "15Hz" & mod == "sine"],
               yend = mean[freq == "15Hz" & mod == "square"]), 
               color = "black") +
  #geom_errorbar(data = dfGroupedStats[dfGroupedStats$freq == "15Hz",], aes(x = mod, color = mod, ymin = mean-sem, ymax = mean+sem), width = .1) +
  geom_point(data = dfGroupedStats[dfGroupedStats$freq == "15Hz",],
             aes (x = mod, color = mod, y = mean), shape = "diamond", size = 4) +
  scale_color_manual(values = brewer.pal(n = 4, "BuGn")[3:4]) +
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



# Histograms of square-sine differences (amplitude) -----------------------
# manually set significance symbols and compute means of means for each plot
sigLabels = c("***","***"," ")

# compute difference values [square - sine] for each participant and frequency
dfStatsDiffAmp <- pivot_wider(data = dfStats, id_cols = c(part,lab,freq), names_from = c(mod), values_from = c(ssvepZ))
dfStatsDiffAmp$diff <- dfStatsDiffAmp$square - dfStatsDiffAmp$sine

# set parameters for plotting
binW = .25
xminHist <-  floor(min(dfStatsDiffAmp$diff)/binW) * binW # rounds down from min value to next lower value dividable by binW
xmaxHist <- ceiling(max(dfStatsDiffAmp$diff)/binW) * binW # rounds up from min value to next higher value dividable by binW
histBreaks <- seq(xminHist,xmaxHist,binW)
xbreaks <- seq(ceiling(xminHist),floor(xmaxHist),1)
yminHist <- 0
ymaxHist <- 14 # set to NA for automatic scaling
inlayFill = "gray80"

# make histograms
hist6Amp <- ggplot(data = dfStatsDiffAmp[dfStatsDiffAmp$freq == "6Hz",],
                   aes(x = diff)) + theme_classic() +
  geom_histogram(breaks = histBreaks, color = inlayFill, fill = "gray20") +
  scale_x_continuous(breaks = xbreaks) +
  scale_y_continuous(limits = c(yminHist,ymaxHist), breaks = c(), expand = c(0,0,0,1)) +
  geom_hline(yintercept = seq(1,ymaxHist,1), color = inlayFill) +
  geom_vline(xintercept = 0, color = brewer.pal(n = 4, "Purples")[4], 
             linetype = "dashed", size = .3) +
  labs(title = bquote(Delta[square-sine]), subtitle = sigLabels[1]) +
  theme(
    plot.title = element_text(hjust = .50, size = fSize, face = "bold"),
    plot.subtitle = element_text(hjust = .50, size = fSize+3, face = "bold", 
                                 color = brewer.pal(n = 4, "Purples")[4], 
                                 margin = margin(t = 0, b = -15)),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(face = "bold"),
    axis.line.y = element_blank(),
    axis.line.x = element_line(size = 0),
    panel.background = element_rect(fill = inlayFill)
  )

hist857Amp <- ggplot(data = dfStatsDiffAmp[dfStatsDiffAmp$freq == "8.57Hz",],
                     aes(x = diff)) + theme_classic() +
  geom_histogram(breaks = histBreaks, color = inlayFill, fill = "gray20") +
  scale_x_continuous(breaks = xbreaks) +
  scale_y_continuous(limits = c(yminHist,ymaxHist), breaks = c(), expand = c(0,0,0,1)) +
  geom_hline(yintercept = seq(1,ymaxHist,1), color = inlayFill) +
  geom_vline(xintercept = 0, color = brewer.pal(n = 4, "Oranges")[4], 
             linetype = "dashed", size = .3) +
  labs(title = bquote(Delta[square-sine]), subtitle = sigLabels[2]) +
  theme(
    plot.title = element_text(hjust = .50, size = fSize, face = "bold"),
    plot.subtitle = element_text(hjust = .50, size = fSize+3, face = "bold", 
                                 color = brewer.pal(n = 4, "Oranges")[4], 
                                 margin = margin(t = 0, b = -15)),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(face = "bold"),
    axis.line.y = element_blank(),
    axis.line.x = element_line(size = 0),
    panel.background = element_rect(fill = inlayFill)
  )

hist15Amp <- ggplot(data = dfStatsDiffAmp[dfStatsDiffAmp$freq == "15Hz",],
                    aes(x = diff)) + theme_classic() +
  geom_histogram(breaks = histBreaks, color = inlayFill, fill = "gray20") +
  scale_x_continuous(breaks = xbreaks) +
  scale_y_continuous(limits = c(yminHist,ymaxHist), breaks = c(), expand = c(0,0,0,1)) +
  geom_hline(yintercept = seq(1,ymaxHist,1), color = inlayFill) +
  geom_vline(xintercept = 0, color = brewer.pal(n = 4, "BuGn")[4], 
             linetype = "dashed", size = .3) +
  labs(title = bquote(Delta[square-sine]), subtitle = sigLabels[3]) +
  theme(
    plot.title = element_text(hjust = .50, size = fSize, face = "bold"),
    plot.subtitle = element_text(hjust = .50, size = fSize+3, face = "bold", 
                                 color = brewer.pal(n = 4, "BuGn")[4], 
                                 margin = margin(t = 0, b = -15)),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(face = "bold"),
    axis.line.y = element_blank(),
    axis.line.x = element_line(size = 0),
    panel.background = element_rect(fill = inlayFill)
  )

# Join line plots and histograms for amplitude measure
# combine line plots with histograms
# coordinates of histogram inlay
xminInlay = 2.1 + .25
xmaxInlay = 2.75 + .25
yminInlay = ymin + (ymax-ymin)*1.00
ymaxInlay = ymin + (ymax-ymin)*-0.14

# put histogram inlays into line plots
hist6Amp <- hist6Amp + theme(plot.margin = margin(0,0,0,0))
plot6Amp <- plot6Amp + scale_x_discrete(expand = c(0,.25,0,1))
plot6Amp <- plot6Amp + annotation_custom(ggplotGrob(hist6Amp),
                                       xmin = xminInlay, xmax = xmaxInlay,
                                       ymin = yminInlay, ymax = ymaxInlay)

hist857Amp <- hist857Amp + theme(plot.margin = margin(0,0,0,0))
plot857Amp <- plot857Amp + scale_x_discrete(expand = c(0,.25,0,1))
plot857Amp <- plot857Amp + annotation_custom(ggplotGrob(hist857Amp),
                                           xmin = xminInlay, xmax = xmaxInlay,
                                           ymin = yminInlay, ymax = ymaxInlay)

hist15Amp <- hist15Amp + theme(plot.margin = margin(0,0,0,0))
plot15Amp <- plot15Amp + scale_x_discrete(expand = c(0,.25,0,1))
plot15Amp <- plot15Amp + annotation_custom(ggplotGrob(hist15Amp),
                                         xmin = xminInlay, xmax = xmaxInlay,
                                         ymin = yminInlay, ymax = ymaxInlay)



# Plots with Individual and Mean ssVEP SNR ---------------------------------
# set y axis limits
yminSNR = min(dfStats$ssvepSNR_Z)
ymaxSNR = max(dfStats$ssvepSNR_Z)

# reverse order of square & sine level on Mod factor (should already have been done, still...)
dfStats$mod <- factor(dfStats$mod, levels = c("sine","square"), labels = c("sine","square"))

# compute dataset with valus adjusted for participants' means to eliminate btw-subject variance
# can be used for standard errors but was not used in the end becuase because of small values
# can be used by uncommenting the "geom_errorbar" lines
dfStatsWithin <- dfStats

for (i in 1:length(levels(dfStats$part))) {
  dfStatsWithin$ssvepSNR_Z[dfStats$part == levels(dfStats$part)[i]] <- 
    dfStats$ssvepSNR_Z[dfStats$part == levels(dfStats$part)[i]] - 
    mean(dfStats$ssvepSNR_Z[dfStats$part == levels(dfStats$part)[i]]) + 
    mean(dfStats$ssvepSNR_Z)
}

# Create dataframe with means and sems for each condition (SEMs not used here)
dfGroupedStatsSNR <- data.frame(
  freq = factor(c("6Hz","6Hz","8.57Hz","8.57Hz","15Hz","15Hz"), levels = c("6Hz","8.57Hz","15Hz")),
  mod = factor(rep(c("sine","square"),3), levels = c("sine","square")),
  mean = rep(0,6),
  sem = rep(0,6)
)

groupedStatsSNR <- describeBy(dfStats, group = c("mod","freq"))

for (i in 1:nrow(dfGroupedStatsSNR)) {
  dfGroupedStatsSNR$mean[i] <- groupedStatsSNR[[i]]$mean[8]
  dfGroupedStatsSNR$sem[i] <- groupedStatsSNR[[i]]$se[8]
}

# Plotting individual values and means, the two Mod conditions connected by line

plot6SNR <- ggplot() + theme_classic() +
  geom_line(data = dfStats[dfStats$freq == "6Hz",], 
            aes(x = mod, y = ssvepSNR_Z, group = part), color = "gray70") +
  geom_point(data = dfStats[dfStats$freq == "6Hz",], 
             aes(x = mod, y = ssvepSNR_Z), color = "gray70") +
  geom_segment(data = dfGroupedStatsSNR, x = 1, xend = 2,
               aes(y = mean[freq == "6Hz" & mod == "sine"],
                   yend = mean[freq == "6Hz" & mod == "square"]), 
               color = "black") +
  #geom_errorbar(data = dfGroupedStats[dfGroupedStats$freq == "6Hz",], aes(x = mod, color = mod, ymin = mean-sem, ymax = mean+sem), width = .1) +
  geom_point(data = dfGroupedStatsSNR[dfGroupedStatsSNR$freq == "6Hz",],
             aes (x = mod, color = mod, y = mean), shape = "diamond", size = 4) +
  scale_color_manual(values = brewer.pal(n = 4, "Purples")[3:4]) +
  scale_y_continuous(name = " \nssVEP SNR (z)", limits = c(yminSNR,ymaxSNR)) +
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

plot857SNR <- ggplot() + theme_classic() +
  geom_line(data = dfStats[dfStats$freq == "8.57Hz",], 
            aes(x = mod, y = ssvepSNR_Z, group = part), color = "gray70") +
  geom_point(data = dfStats[dfStats$freq == "8.57Hz",], 
             aes(x = mod, y = ssvepSNR_Z), color = "gray70") +
  geom_segment(data = dfGroupedStatsSNR, x = 1, xend = 2,
               aes(y = mean[freq == "8.57Hz" & mod == "sine"],
                   yend = mean[freq == "8.57Hz" & mod == "square"]), 
               color = "black") +
  #geom_errorbar(data = dfGroupedStatsSNR[dfGroupedStatsSNR$freq == "8.57Hz",], aes(x = mod, color = mod, ymin = mean-sem, ymax = mean+sem), width = .1) +
  geom_point(data = dfGroupedStatsSNR[dfGroupedStatsSNR$freq == "8.57Hz",],
             aes (x = mod, color = mod, y = mean), shape = "diamond", size = 4) +
  scale_color_manual(values = brewer.pal(n = 4, "Oranges")[3:4]) +
  scale_y_continuous(name = " ", limits = c(yminSNR,ymaxSNR)) +
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

plot15SNR <- ggplot() + theme_classic() +
  geom_line(data = dfStats[dfStats$freq == "15Hz",], 
            aes(x = mod, y = ssvepSNR_Z, group = part), color = "gray70") +
  geom_point(data = dfStats[dfStats$freq == "15Hz",], 
             aes(x = mod, y = ssvepSNR_Z), color = "gray70") +
  geom_segment(data = dfGroupedStatsSNR, x = 1, xend = 2,
               aes(y = mean[freq == "15Hz" & mod == "sine"],
                   yend = mean[freq == "15Hz" & mod == "square"]), 
               color = "black") +
  #geom_errorbar(data = dfGroupedStatsSNR[dfGroupedStatsSNR$freq == "15Hz",], aes(x = mod, color = mod, ymin = mean-sem, ymax = mean+sem), width = .1) +
  geom_point(data = dfGroupedStatsSNR[dfGroupedStatsSNR$freq == "15Hz",],
             aes (x = mod, color = mod, y = mean), shape = "diamond", size = 4) +
  scale_color_manual(values = brewer.pal(n = 4, "BuGn")[3:4]) +
  scale_y_continuous(name = " ", limits = c(yminSNR,ymaxSNR)) +
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



# Histograms of square-sine differences (SNR) -----------------------------
# manually set significance symbols and compute means of means for each plot
sigLabelsSNR = c(" ","  ***","*")

# compute difference values [square - sine] for each participant and frequency
dfStatsDiffSNR <- pivot_wider(data = dfStats, id_cols = c(part,lab,freq), names_from = c(mod), values_from = c(ssvepSNR_Z))
dfStatsDiffSNR$diff <- dfStatsDiffSNR$square - dfStatsDiffSNR$sine

# set parameters for plotting
binW = 1/3
xminHist <-  floor(min(dfStatsDiffSNR$diff)/binW) * binW # rounds down from min value to next lower value dividable by binW
xmaxHist <- ceiling(max(dfStatsDiffSNR$diff)/binW) * binW # rounds up from min value to next higher value dividable by binW
histBreaks <- seq(xminHist,xmaxHist,binW)
xbreaks <- seq(ceiling(xminHist),floor(xmaxHist),1)
yminHist <- 0
ymaxHist <- 10 # set to NA for automatic scaling
inlayFill = "gray80"

# make histograms
hist6SNR <- ggplot(data = dfStatsDiffSNR[dfStatsDiffSNR$freq == "6Hz",],
                   aes(x = diff)) + theme_classic() +
  geom_histogram(breaks = histBreaks, color = inlayFill, fill = "gray20") +
  scale_x_continuous(breaks = xbreaks) +
  scale_y_continuous(limits = c(yminHist,ymaxHist), breaks = c(), expand = c(0,0,0,1)) +
  geom_hline(yintercept = seq(1,ymaxHist,1), color = inlayFill) +
  geom_vline(xintercept = 0, color = brewer.pal(n = 4, "Purples")[4], 
             linetype = "dashed", size = .3) +
  labs(title = bquote(Delta[square-sine]), subtitle = sigLabelsSNR[1]) +
  theme(
    plot.title = element_text(hjust = .50, size = fSize, face = "bold"),
    plot.subtitle = element_text(hjust = .50, size = fSize+3, face = "bold", 
                                 color = brewer.pal(n = 4, "Purples")[4], 
                                 margin = margin(t = 0, b = -15)),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(face = "bold"),
    axis.line.y = element_blank(),
    axis.line.x = element_line(size = 0),
    panel.background = element_rect(fill = inlayFill)
  )

hist857SNR <- ggplot(data = dfStatsDiffSNR[dfStatsDiffSNR$freq == "8.57Hz",],
                     aes(x = diff)) + theme_classic() +
  geom_histogram(breaks = histBreaks, color = inlayFill, fill = "gray20") +
  scale_x_continuous(breaks = xbreaks) +
  scale_y_continuous(limits = c(yminHist,ymaxHist), breaks = c(), expand = c(0,0,0,1)) +
  geom_hline(yintercept = seq(1,ymaxHist,1), color = inlayFill) +
  geom_vline(xintercept = 0, color = brewer.pal(n = 4, "Oranges")[4], 
             linetype = "dashed", size = .3) +
  labs(title = bquote(Delta[square-sine]), subtitle = sigLabelsSNR[2]) +
  theme(
    plot.title = element_text(hjust = .50, size = fSize, face = "bold"),
    plot.subtitle = element_text(hjust = .50, size = fSize+3, face = "bold", 
                                 color = brewer.pal(n = 4, "Oranges")[4], 
                                 margin = margin(t = 0, b = -15)),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(face = "bold"),
    axis.line.y = element_blank(),
    axis.line.x = element_line(size = 0),
    panel.background = element_rect(fill = inlayFill)
  )

hist15SNR <- ggplot(data = dfStatsDiffSNR[dfStatsDiffSNR$freq == "15Hz",],
                    aes(x = diff)) + theme_classic() +
  geom_histogram(breaks = histBreaks, color = inlayFill, fill = "gray20") +
  scale_x_continuous(breaks = xbreaks) +
  scale_y_continuous(limits = c(yminHist,ymaxHist), breaks = c(), expand = c(0,0,0,1)) +
  geom_hline(yintercept = seq(1,ymaxHist,1), color = inlayFill) +
  geom_vline(xintercept = 0, color = brewer.pal(n = 4, "BuGn")[4], 
             linetype = "dashed", size = .3) +
  labs(title = bquote(Delta[square-sine]), subtitle = sigLabelsSNR[3]) +
  theme(
    plot.title = element_text(hjust = .50, size = fSize, face = "bold"),
    plot.subtitle = element_text(hjust = .50, size = fSize+3, face = "bold", 
                                 color = brewer.pal(n = 4, "BuGn")[4], 
                                 margin = margin(t = 0, b = -15)),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(face = "bold"),
    axis.line.y = element_blank(),
    axis.line.x = element_line(size = 0),
    panel.background = element_rect(fill = inlayFill)
  )

# Join line plots and histograms for amplitude measure
# combine line plots with histograms
# coordinates of histogram inlay
xminInlay = 2.1 + .25
xmaxInlay = 2.75 + .25
yminInlay = yminSNR + (ymaxSNR-yminSNR)*1.00
ymaxInlay = yminSNR + (ymaxSNR-yminSNR)*-0.14

# put histogram inlays into line plots
hist6SNR <- hist6SNR + theme(plot.margin = margin(0,0,0,0))
plot6SNR <- plot6SNR + scale_x_discrete(expand = c(0,.25,0,1))
plot6SNR <- plot6SNR + annotation_custom(ggplotGrob(hist6SNR),
                                         xmin = xminInlay, xmax = xmaxInlay,
                                         ymin = yminInlay, ymax = ymaxInlay)

hist857SNR <- hist857SNR + theme(plot.margin = margin(0,0,0,0))
plot857SNR <- plot857SNR + scale_x_discrete(expand = c(0,.25,0,1))
plot857SNR <- plot857SNR + annotation_custom(ggplotGrob(hist857SNR),
                                             xmin = xminInlay, xmax = xmaxInlay,
                                             ymin = yminInlay, ymax = ymaxInlay)

hist15SNR <- hist15SNR + theme(plot.margin = margin(0,0,0,0))
plot15SNR <- plot15SNR + scale_x_discrete(expand = c(0,.25,0,1))
plot15SNR <- plot15SNR + annotation_custom(ggplotGrob(hist15SNR),
                                           xmin = xminInlay, xmax = xmaxInlay,
                                           ymin = yminInlay, ymax = ymaxInlay)



# Putting Plots Together --------------------------------------------------
# add margins and significance symbols to single plots before joining them
spec6 <- spec6 + theme(plot.margin = margin(20,5,10,5))
spec857 <- spec857 + theme(plot.margin = margin(20,5,10,5))
spec15 <- spec15 + theme(plot.margin = margin(20,5,10,5))

plot6Amp <- plot6Amp + theme(plot.margin = margin(10,5,10,5))
plot857Amp <- plot857Amp + theme(plot.margin = margin(10,5,10,5))
plot15Amp <- plot15Amp + theme(plot.margin = margin(10,5,10,5))

plot6SNR <- plot6SNR + theme(plot.margin = margin(10,5,20,5))
plot857SNR <- plot857SNR + theme(plot.margin = margin(10,5,20,5))
plot15SNR <- plot15SNR + theme(plot.margin = margin(10,5,20,5))

# join plots and show them in RStudio
jointPlots <- ggarrange(spec6, spec857, spec15, 
                        plot6Amp, plot857Amp, plot15Amp, 
                        plot6SNR, plot857SNR, plot15SNR, 
                        nrow = 3, ncol = 3,
                        labels = c("A","","","B","","","C","",""),
                        font.label = list(size = 20, family = fType))
jointPlots

# Save plot as jpg and pdf
savename = paste0(parentFolder,"/figures/16_means_commPL_jointLabs.pdf")
ggsave(filename = savename, plot = jointPlots, device = "pdf",
       width = 21, height = 23, unit = "cm", limitsize = FALSE)

savename = paste0(parentFolder,"/figures/16_means_commPL_jointLabs.jpg")
ggsave(filename = savename, plot = jointPlots, device = "jpg",
       width = 21, height = 23, unit = "cm", dpi = 300, limitsize = FALSE)


