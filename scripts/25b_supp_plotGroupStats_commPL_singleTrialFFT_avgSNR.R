# --- author: Christian Panitz
# --- encoding: en_US.UTF-8
# --- R version: 4.0.3 (2020-10-10) -- "Bunny-Wunnies Freak Out"
# --- RStudio version: 2022.02.3
# --- script version: December 2022
# --- content: plotting group stats for different conditions and labs (common pipeline, single-trial FFT, SNR from average)


# Header Parameters -------------------------------------------------------

fType = "Helvetica"
fSize = 9



# Loading Packages & Data, Setting Path ----------------------------------

# load packages
library(here)
library(psych)
library(ggplot2)
library(RColorBrewer)
library(tidyr)
library(ggpubr)

# set parent folder
parentFolder <- here()

# load data to plot
loadname <- paste0(parentFolder,"/dataframes/supp_dfSSVEP_commPL_singleTrialFFT.csv")
dfStats <- read.csv(loadname)

dfStats$part <- factor(dfStats$part)
dfStats$lab <- factor(dfStats$lab, levels = c("Florida","Leipzig"), labels = c("Florida", "Leipzig"))
dfStats$freq <- factor(dfStats$freq, levels = c("6Hz","8.57Hz","15Hz"), labels = c("6Hz","8.57Hz","15Hz"))
dfStats$mod <- factor(dfStats$mod, levels = c("sine","square"), labels = c("sine","square"))



# Plots with Individual and Mean ssVEPs -----------------------------------

# set y axis limits
ymin = min(dfStats$ssvepSNR_Z)
ymax = max(dfStats$ssvepSNR_Z)

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

# Create dataframe with means and sems for each lab & condition (SEMs not used here)
dfGroupedStats <- data.frame(
  lab = factor(c(rep("Florida",6),rep("Leipzig",6)), levels = c("Florida", "Leipzig")),
  freq = factor(rep(c("6Hz","6Hz","8.57Hz","8.57Hz","15Hz","15Hz"),2), levels = c("6Hz","8.57Hz","15Hz")),
  mod = factor(rep(c("sine","square"),6), levels = c("sine","square")),
  mean = rep(0,12),
  sem = rep(0,12)
)

groupedStats <- describeBy(dfStats, group = c("mod","freq","lab"))

for (i in 1:nrow(dfGroupedStats)) {
  dfGroupedStats$mean[i] <- groupedStats[[i]]$mean[8]
  dfGroupedStats$sem[i] <- groupedStats[[i]]$se[8]
}

# Plotting individual values and means, the two Mod conditions connected by line

plotFL6 <- ggplot() + theme_classic() +
  geom_line(data = dfStats[dfStats$lab == "Florida" & dfStats$freq == "6Hz",], 
            aes(x = mod, y = ssvepSNR_Z, group = part), color = "gray70") +
  geom_point(data = dfStats[dfStats$lab == "Florida" & dfStats$freq == "6Hz",], 
             aes(x = mod, y = ssvepSNR_Z), color = "gray70") +
  geom_segment(data = dfGroupedStats, x = 1, xend = 2,
               aes(y = mean[lab == "Florida" & freq == "6Hz" & mod == "sine"],
                   yend = mean[lab == "Florida" & freq == "6Hz" & mod == "square"]), 
               color = "black") +
  #geom_errorbar(data = dfGroupedStats[dfGroupedStats$lab == "Florida" & dfGroupedStats$freq == "6Hz",], aes(x = mod, color = mod, ymin = mean-sem, ymax = mean+sem), width = .1) +
  geom_point(data = dfGroupedStats[dfGroupedStats$lab == "Florida" & dfGroupedStats$freq == "6Hz",],
             aes (x = mod, color = mod, y = mean), shape = "diamond", size = 4) +
  scale_color_manual(values = brewer.pal(n = 4, "Purples")[3:4]) +
  scale_y_continuous(name = "ssVEP SNR (z)", limits = c(ymin,ymax)) +
  labs(title = "Florida - 6 Hz") +
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

plotFL857 <- ggplot() + theme_classic() +
  geom_line(data = dfStats[dfStats$lab == "Florida" & dfStats$freq == "8.57Hz",], 
            aes(x = mod, y = ssvepSNR_Z, group = part), color = "gray70") +
  geom_point(data = dfStats[dfStats$lab == "Florida" & dfStats$freq == "8.57Hz",], 
             aes(x = mod, y = ssvepSNR_Z), color = "gray70") +
  geom_segment(data = dfGroupedStats, x = 1, xend = 2,
               aes(y = mean[lab == "Florida" & freq == "8.57Hz" & mod == "sine"],
                   yend = mean[lab == "Florida" & freq == "8.57Hz" & mod == "square"]), 
               color = "black") +
  #geom_errorbar(data = dfGroupedStats[dfGroupedStats$lab == "Florida" & dfGroupedStats$freq == "8.57Hz",], aes(x = mod, color = mod, ymin = mean-sem, ymax = mean+sem), width = .1) +
  geom_point(data = dfGroupedStats[dfGroupedStats$lab == "Florida" & dfGroupedStats$freq == "8.57Hz",],
             aes (x = mod, color = mod, y = mean), shape = "diamond", size = 4) +
  scale_color_manual(values = brewer.pal(n = 4, "Oranges")[3:4]) +
  scale_y_continuous(name = " ", limits = c(ymin,ymax)) +
  labs(title = "Florida - 8.57 Hz") +
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

plotFL15 <- ggplot() + theme_classic() +
  geom_line(data = dfStats[dfStats$lab == "Florida" & dfStats$freq == "15Hz",], 
            aes(x = mod, y = ssvepSNR_Z, group = part), color = "gray70") +
  geom_point(data = dfStats[dfStats$lab == "Florida" & dfStats$freq == "15Hz",], 
             aes(x = mod, y = ssvepSNR_Z), color = "gray70") +
  geom_segment(data = dfGroupedStats, x = 1, xend = 2,
               aes(y = mean[lab == "Florida" & freq == "15Hz" & mod == "sine"],
               yend = mean[lab == "Florida" & freq == "15Hz" & mod == "square"]), 
               color = "black") +
  #geom_errorbar(data = dfGroupedStats[dfGroupedStats$lab == "Florida" & dfGroupedStats$freq == "15Hz",], aes(x = mod, color = mod, ymin = mean-sem, ymax = mean+sem), width = .1) +
  geom_point(data = dfGroupedStats[dfGroupedStats$lab == "Florida" & dfGroupedStats$freq == "15Hz",],
             aes (x = mod, color = mod, y = mean), shape = "diamond", size = 4) +
  scale_color_manual(values = brewer.pal(n = 4, "BuGn")[3:4]) +
  scale_y_continuous(name = " ", limits = c(ymin,ymax)) +
  labs(title = "Florida - 15 Hz") +
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

plotLE6 <- ggplot() + theme_classic() +
  geom_line(data = dfStats[dfStats$lab == "Leipzig" & dfStats$freq == "6Hz",], 
            aes(x = mod, y = ssvepSNR_Z, group = part), color = "gray70") +
  geom_point(data = dfStats[dfStats$lab == "Leipzig" & dfStats$freq == "6Hz",], 
             aes(x = mod, y = ssvepSNR_Z), color = "gray70") +
  geom_segment(data = dfGroupedStats, x = 1, xend = 2,
               aes(y = mean[lab == "Leipzig" & freq == "6Hz" & mod == "sine"],
                   yend = mean[lab == "Leipzig" & freq == "6Hz" & mod == "square"]), 
               color = "black") +
  #geom_errorbar(data = dfGroupedStats[dfGroupedStats$lab == "Leipzig" & dfGroupedStats$freq == "6Hz",], aes(x = mod, color = mod, ymin = mean-sem, ymax = mean+sem), width = .1) +
  geom_point(data = dfGroupedStats[dfGroupedStats$lab == "Leipzig" & dfGroupedStats$freq == "6Hz",],
             aes (x = mod, color = mod, y = mean), shape = "diamond", size = 4) +
  scale_color_manual(values = brewer.pal(n = 4, "Purples")[3:4]) +
  scale_y_continuous(name = "ssVEP SNR (z)", limits = c(ymin,ymax)) +
  labs(title = "Leipzig - 6 Hz") +
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

plotLE857 <- ggplot() + theme_classic() +
  geom_line(data = dfStats[dfStats$lab == "Leipzig" & dfStats$freq == "8.57Hz",], 
            aes(x = mod, y = ssvepSNR_Z, group = part), color = "gray70") +
  geom_point(data = dfStats[dfStats$lab == "Leipzig" & dfStats$freq == "8.57Hz",], 
             aes(x = mod, y = ssvepSNR_Z), color = "gray70") +
  geom_segment(data = dfGroupedStats, x = 1, xend = 2,
               aes(y = mean[lab == "Leipzig" & freq == "8.57Hz" & mod == "sine"],
                   yend = mean[lab == "Leipzig" & freq == "8.57Hz" & mod == "square"]), 
               color = "black") +
  #geom_errorbar(data = dfGroupedStats[dfGroupedStats$lab == "Leipzig" & dfGroupedStats$freq == "8.57Hz",], aes(x = mod, color = mod, ymin = mean-sem, ymax = mean+sem), width = .1) +
  geom_point(data = dfGroupedStats[dfGroupedStats$lab == "Leipzig" & dfGroupedStats$freq == "8.57Hz",],
             aes (x = mod, color = mod, y = mean), shape = "diamond", size = 4) +
  scale_color_manual(values = brewer.pal(n = 4, "Oranges")[3:4]) +
  scale_y_continuous(name = " ", limits = c(ymin,ymax)) +
  labs(title = "Leipzig - 8.57 Hz") +
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

plotLE15 <- ggplot() + theme_classic() +
  geom_line(data = dfStats[dfStats$lab == "Leipzig" & dfStats$freq == "15Hz",], 
            aes(x = mod, y = ssvepSNR_Z, group = part), color = "gray70") +
  geom_point(data = dfStats[dfStats$lab == "Leipzig" & dfStats$freq == "15Hz",], 
             aes(x = mod, y = ssvepSNR_Z), color = "gray70") +
  geom_segment(data = dfGroupedStats, x = 1, xend = 2,
               aes(y = mean[lab == "Leipzig" & freq == "15Hz" & mod == "sine"],
                   yend = mean[lab == "Leipzig" & freq == "15Hz" & mod == "square"]), 
               color = "black") +
  #geom_errorbar(data = dfGroupedStats[dfGroupedStats$lab == "Leipzig" & dfGroupedStats$freq == "15Hz",], aes(x = mod, color = mod, ymin = mean-sem, ymax = mean+sem), width = .1) +
  geom_point(data = dfGroupedStats[dfGroupedStats$lab == "Leipzig" & dfGroupedStats$freq == "15Hz",],
             aes (x = mod, color = mod, y = mean), shape = "diamond", size = 4) +
  scale_color_manual(values = brewer.pal(n = 4, "BuGn")[3:4]) +
  scale_y_continuous(name = " ", limits = c(ymin,ymax)) +
  labs(title = "Leipzig - 15 Hz") +
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



# manually set significance symbols and compute means of means  for each plot
sigLabels = c("*","**"," ",
              "*","**"," ")
mAcrossMod = c(mean(dfGroupedStats$mean[dfGroupedStats$lab == "Florida" & dfGroupedStats$freq == "6Hz"]),
               mean(dfGroupedStats$mean[dfGroupedStats$lab == "Florida" &dfGroupedStats$freq == "8.57Hz"]),
               mean(dfGroupedStats$mean[dfGroupedStats$lab == "Florida" & dfGroupedStats$freq == "15Hz"]),
               mean(dfGroupedStats$mean[dfGroupedStats$lab == "Leipzig" & dfGroupedStats$freq == "6Hz"]),
               mean(dfGroupedStats$mean[dfGroupedStats$lab == "Leipzig" & dfGroupedStats$freq == "8.57Hz"]),
               mean(dfGroupedStats$mean[dfGroupedStats$lab == "Leipzig" & dfGroupedStats$freq == "15Hz"]))

# add margins and significance symbols to single plots before joining them
plotFL6 <- plotFL6 + theme(plot.margin = margin(20,5,20,5))
plotFL857 <- plotFL857 + theme(plot.margin = margin(20,5,20,5))
plotFL15 <- plotFL15 + theme(plot.margin = margin(20,5,20,5))
plotLE6 <- plotLE6 + theme(plot.margin = margin(20,5,20,5))
plotLE857 <- plotLE857 + theme(plot.margin = margin(20,5,20,5))
plotLE15 <- plotLE15 + theme(plot.margin = margin(20,5,20,5))



# Histograms of square-sine differences -----------------------------------
# compute difference values [square - sine] for each participant and frequency
dfStatsDiff <- pivot_wider(data = dfStats, id_cols = c(part,lab,freq), names_from = c(mod), values_from = c(ssvepSNR_Z))
dfStatsDiff$diff <- dfStatsDiff$square - dfStatsDiff$sine

# set parameters for plotting
binW = 1/3
xminHist <-  floor(min(dfStatsDiff$diff)/binW) * binW # rounds down from min value to next lower value dividable by binW
xmaxHist <- ceiling(max(dfStatsDiff$diff)/binW) * binW # rounds up from min value to next higher value dividable by binW
histBreaks <- seq(xminHist,xmaxHist,binW)
xbreaks <- seq(ceiling(xminHist),floor(xmaxHist),1)
yminHist <- 0
ymaxHist <- 10 # set to NA for automatic scaling
inlayFill = "gray80"

# make histograms
histFL6 <- ggplot(data = dfStatsDiff[dfStatsDiff$lab == "Florida" & dfStatsDiff$freq == "6Hz",],
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

histFL857 <- ggplot(data = dfStatsDiff[dfStatsDiff$lab == "Florida" & dfStatsDiff$freq == "8.57Hz",],
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

histFL15 <- ggplot(data = dfStatsDiff[dfStatsDiff$lab == "Florida" & dfStatsDiff$freq == "15Hz",],
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

histLE6 <- ggplot(data = dfStatsDiff[dfStatsDiff$lab == "Leipzig" & dfStatsDiff$freq == "6Hz",],
                  aes(x = diff)) + theme_classic() +
  geom_histogram(breaks = histBreaks, color = inlayFill, fill = "gray20") +
  scale_x_continuous(breaks = xbreaks) +
  scale_y_continuous(limits = c(yminHist,ymaxHist), breaks = c(), expand = c(0,0,0,1)) +
  geom_hline(yintercept = seq(1,ymaxHist,1), color = inlayFill) +
  geom_vline(xintercept = 0, color = brewer.pal(n = 4, "Purples")[4], 
             linetype = "dashed", size = .3) +
  labs(title = bquote(Delta[square-sine]), subtitle = sigLabels[4]) +
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

histLE857 <- ggplot(data = dfStatsDiff[dfStatsDiff$lab == "Leipzig" & dfStatsDiff$freq == "8.57Hz",],
                    aes(x = diff)) + theme_classic() +
  geom_histogram(breaks = histBreaks, color = inlayFill, fill = "gray20") +
  scale_x_continuous(breaks = xbreaks) +
  scale_y_continuous(limits = c(yminHist,ymaxHist), breaks = c(), expand = c(0,0,0,1)) +
  geom_hline(yintercept = seq(1,ymaxHist,1), color = inlayFill) +
  geom_vline(xintercept = 0, color = brewer.pal(n = 4, "Oranges")[4], 
             linetype = "dashed", size = .3) +
  labs(title = bquote(Delta[square-sine]), subtitle = sigLabels[5]) +
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

histLE15 <- ggplot(data = dfStatsDiff[dfStatsDiff$lab == "Leipzig" & dfStatsDiff$freq == "15Hz",],
                   aes(x = diff)) + theme_classic() +
  geom_histogram(breaks = histBreaks, color = inlayFill, fill = "gray20") +
  scale_x_continuous(breaks = xbreaks) +
  scale_y_continuous(limits = c(yminHist,ymaxHist), breaks = c(), expand = c(0,0,0,1)) +
  geom_hline(yintercept = seq(1,ymaxHist,1), color = inlayFill) +
  geom_vline(xintercept = 0, color = brewer.pal(n = 4, "BuGn")[4], 
             linetype = "dashed", size = .3) +
  labs(title = bquote(Delta[square-sine]), subtitle = sigLabels[6]) +
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


# combine line plots with histograms
# coordinates of histogram inlay
xminInlay = 2.1 + .25
xmaxInlay = 2.75 + .25
yminInlay = ymin + (ymax-ymin)*1.00
ymaxInlay = ymin + (ymax-ymin)*-0.15

# put histogram inlays into line plots
histFL6 <- histFL6 + theme(plot.margin = margin(0,0,0,0))
plotFL6 <- plotFL6 + scale_x_discrete(expand = c(0,.25,0,1))
plotFL6 <- plotFL6 + annotation_custom(ggplotGrob(histFL6),
                                       xmin = xminInlay, xmax = xmaxInlay,
                                       ymin = yminInlay, ymax = ymaxInlay)

histFL857 <- histFL857 + theme(plot.margin = margin(0,0,0,0))
plotFL857 <- plotFL857 + scale_x_discrete(expand = c(0,.25,0,1))
plotFL857 <- plotFL857 + annotation_custom(ggplotGrob(histFL857),
                                           xmin = xminInlay, xmax = xmaxInlay,
                                           ymin = yminInlay, ymax = ymaxInlay)

histFL15 <- histFL15 + theme(plot.margin = margin(0,0,0,0))
plotFL15 <- plotFL15 + scale_x_discrete(expand = c(0,.25,0,1))
plotFL15 <- plotFL15 + annotation_custom(ggplotGrob(histFL15),
                                         xmin = xminInlay, xmax = xmaxInlay,
                                         ymin = yminInlay, ymax = ymaxInlay)

histLE6 <- histLE6 + theme(plot.margin = margin(0,0,0,0))
plotLE6 <- plotLE6 + scale_x_discrete(expand = c(0,.25,0,1))
plotLE6 <- plotLE6 + annotation_custom(ggplotGrob(histLE6),
                                       xmin = xminInlay, xmax = xmaxInlay,
                                       ymin = yminInlay, ymax = ymaxInlay)

histLE857 <- histLE857 + theme(plot.margin = margin(0,0,0,0))
plotLE857 <- plotLE857 + scale_x_discrete(expand = c(0,.25,0,1))
plotLE857 <- plotLE857 + annotation_custom(ggplotGrob(histLE857),
                                           xmin = xminInlay, xmax = xmaxInlay,
                                           ymin = yminInlay, ymax = ymaxInlay)

histLE15 <- histLE15 + theme(plot.margin = margin(0,0,0,0))
plotLE15 <- plotLE15 + scale_x_discrete(expand = c(0,.25,0,1))
plotLE15 <- plotLE15 + annotation_custom(ggplotGrob(histLE15),
                                         xmin = xminInlay, xmax = xmaxInlay,
                                         ymin = yminInlay, ymax = ymaxInlay)



# Check and save end result -----------------------------------------------
# join plots and show them in RStudio
meanPlots <- ggarrange(plotFL6, plotFL857, plotFL15, plotLE6, plotLE857, plotLE15,
                       nrow = 2, ncol = 3)
meanPlots

# Save plot as jpg and pdf
savename = paste0(parentFolder,"/figures/23b_supp_means_commPL_singleTrialFFT_avgSNR.pdf")
ggsave(filename = savename, plot = meanPlots, device = "pdf",
       width = 21, height = 15, unit = "cm", limitsize = FALSE)

savename = paste0(parentFolder,"/figures/23b_supp_means_commPL_singleTrialFFT_avgSNR.jpg")
ggsave(filename = savename, plot = meanPlots, device = "jpg",
       width = 21, height = 15, unit = "cm", dpi = 300, limitsize = FALSE)