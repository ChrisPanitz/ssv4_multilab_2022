# --- author: Christian Panitz
# --- encoding: en_US.UTF-8
# --- R version: 4.0.3 (2020-10-10) -- "Bunny-Wunnies Freak Out"
# --- RStudio version: 2022.02.3
# --- script version: July 2022
# --- content: plotting group stats for different conditions


# Header Parameters -------------------------------------------------------

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

# load data to plot
loadname <- paste0(parentFolder,"/dataframes/dfSSVEP_commPL.csv")
dfStats <- read.csv(loadname)

dfStats$part <- factor(dfStats$part)
dfStats$site <- factor(dfStats$site, levels = c("Florida","Leipzig"), labels = c("Florida", "Leipzig"))
dfStats$freq <- factor(dfStats$freq, levels = c("6Hz","8.57Hz","15Hz"), labels = c("6Hz","8.57Hz","15Hz"))
dfStats$mod <- factor(dfStats$mod, levels = c("box","sine"), labels = c("box","sine"))



# Plots with Individual and Mean ssVEPs -----------------------------------

# set y axis limits
ymin = min(dfStats$ssvepZ)
ymax = max(dfStats$ssvepZ)

# compute dataset with valus adjusted for participants' means to eliminate btw-subject variance
# can be used for standard errors but was not used in the end becuase because of small values
# can be used by uncommenting the "geom_errorbar" lines
dfStatsWithin <- dfStats

for (i in 1:length(levels(dfStats$part))) {
  dfStatsWithin$ssvepZ[dfStats$part == levels(dfStats$part)[i]] <- dfStats$ssvepZ[dfStats$part == levels(dfStats$part)[i]] - mean(dfStats$ssvepZ[dfStats$part == levels(dfStats$part)[i]]) + mean(dfStats$ssvepZ)
}

# Create dataframe with means and sems for each site & condition (SEMs not used here)
dfGroupedStats <- data.frame(
  site = factor(c(rep("Florida",6),rep("Leipzig",6)), levels = c("Florida", "Leipzig")),
  freq = factor(rep(c("6Hz","6Hz","8.57Hz","8.57Hz","15Hz","15Hz"),2), levels = c("6Hz","8.57Hz","15Hz")),
  mod = factor(rep(c("box","sine"),6), levels = c("box","sine")),
  mean = rep(0,12),
  sem = rep(0,12)
)

groupedStats <- describeBy(dfStats, group = c("mod","freq","site"))

for (i in 1:nrow(dfGroupedStats)) {
  dfGroupedStats$mean[i] <- groupedStats[[i]]$mean[6]
  dfGroupedStats$sem[i] <- groupedStats[[i]]$se[6]
}

# Plotting individual values and means, the two Mod conditions connected by line

plotFL6 <- ggplot() + theme_classic() +
  geom_line(data = dfStats[dfStats$site == "Florida" & dfStats$freq == "6Hz",], 
            aes(x = mod, y = ssvepZ, group = part), color = "gray70") +
  geom_point(data = dfStats[dfStats$site == "Florida" & dfStats$freq == "6Hz",], 
             aes(x = mod, y = ssvepZ), color = "gray70") +
  geom_segment(data = dfGroupedStats, x = 1, xend = 2,
               aes(y = mean[site == "Florida" & freq == "6Hz" & mod == "box"],
                   yend = mean[site == "Florida" & freq == "6Hz" & mod == "sine"]), 
               color = "black") +
  #geom_errorbar(data = dfGroupedStats[dfGroupedStats$site == "Florida" & dfGroupedStats$freq == "6Hz",], aes(x = mod, color = mod, ymin = mean-sem, ymax = mean+sem), width = .1) +
  geom_point(data = dfGroupedStats[dfGroupedStats$site == "Florida" & dfGroupedStats$freq == "6Hz",],
             aes (x = mod, color = mod, y = mean), shape = "diamond", size = 4) +
  scale_color_manual(values = brewer.pal(n = 4, "Purples")[4:3]) +
  scale_y_continuous(name = "ssVEP amplitude (z)", limits = c(ymin,ymax)) +
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
  geom_line(data = dfStats[dfStats$site == "Florida" & dfStats$freq == "8.57Hz",], 
            aes(x = mod, y = ssvepZ, group = part), color = "gray70") +
  geom_point(data = dfStats[dfStats$site == "Florida" & dfStats$freq == "8.57Hz",], 
             aes(x = mod, y = ssvepZ), color = "gray70") +
  geom_segment(data = dfGroupedStats, x = 1, xend = 2,
               aes(y = mean[site == "Florida" & freq == "8.57Hz" & mod == "box"],
                   yend = mean[site == "Florida" & freq == "8.57Hz" & mod == "sine"]), 
               color = "black") +
  #geom_errorbar(data = dfGroupedStats[dfGroupedStats$site == "Florida" & dfGroupedStats$freq == "8.57Hz",], aes(x = mod, color = mod, ymin = mean-sem, ymax = mean+sem), width = .1) +
  geom_point(data = dfGroupedStats[dfGroupedStats$site == "Florida" & dfGroupedStats$freq == "8.57Hz",],
             aes (x = mod, color = mod, y = mean), shape = "diamond", size = 4) +
  scale_color_manual(values = brewer.pal(n = 4, "Oranges")[4:3]) +
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
  geom_line(data = dfStats[dfStats$site == "Florida" & dfStats$freq == "15Hz",], 
            aes(x = mod, y = ssvepZ, group = part), color = "gray70") +
  geom_point(data = dfStats[dfStats$site == "Florida" & dfStats$freq == "15Hz",], 
             aes(x = mod, y = ssvepZ), color = "gray70") +
  geom_segment(data = dfGroupedStats, x = 1, xend = 2,
               aes(y = mean[site == "Florida" & freq == "15Hz" & mod == "box"],
               yend = mean[site == "Florida" & freq == "15Hz" & mod == "sine"]), 
               color = "black") +
  #geom_errorbar(data = dfGroupedStats[dfGroupedStats$site == "Florida" & dfGroupedStats$freq == "15Hz",], aes(x = mod, color = mod, ymin = mean-sem, ymax = mean+sem), width = .1) +
  geom_point(data = dfGroupedStats[dfGroupedStats$site == "Florida" & dfGroupedStats$freq == "15Hz",],
             aes (x = mod, color = mod, y = mean), shape = "diamond", size = 4) +
  scale_color_manual(values = brewer.pal(n = 4, "BuGn")[4:3]) +
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
  geom_line(data = dfStats[dfStats$site == "Leipzig" & dfStats$freq == "6Hz",], 
            aes(x = mod, y = ssvepZ, group = part), color = "gray70") +
  geom_point(data = dfStats[dfStats$site == "Leipzig" & dfStats$freq == "6Hz",], 
             aes(x = mod, y = ssvepZ), color = "gray70") +
  geom_segment(data = dfGroupedStats, x = 1, xend = 2,
               aes(y = mean[site == "Leipzig" & freq == "6Hz" & mod == "box"],
                   yend = mean[site == "Leipzig" & freq == "6Hz" & mod == "sine"]), 
               color = "black") +
  #geom_errorbar(data = dfGroupedStats[dfGroupedStats$site == "Leipzig" & dfGroupedStats$freq == "6Hz",], aes(x = mod, color = mod, ymin = mean-sem, ymax = mean+sem), width = .1) +
  geom_point(data = dfGroupedStats[dfGroupedStats$site == "Leipzig" & dfGroupedStats$freq == "6Hz",],
             aes (x = mod, color = mod, y = mean), shape = "diamond", size = 4) +
  scale_color_manual(values = brewer.pal(n = 4, "Purples")[4:3]) +
  scale_y_continuous(name = "ssVEP amplitude (z)", limits = c(ymin,ymax)) +
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
  geom_line(data = dfStats[dfStats$site == "Leipzig" & dfStats$freq == "8.57Hz",], 
            aes(x = mod, y = ssvepZ, group = part), color = "gray70") +
  geom_point(data = dfStats[dfStats$site == "Leipzig" & dfStats$freq == "8.57Hz",], 
             aes(x = mod, y = ssvepZ), color = "gray70") +
  geom_segment(data = dfGroupedStats, x = 1, xend = 2,
               aes(y = mean[site == "Leipzig" & freq == "8.57Hz" & mod == "box"],
                   yend = mean[site == "Leipzig" & freq == "8.57Hz" & mod == "sine"]), 
               color = "black") +
  #geom_errorbar(data = dfGroupedStats[dfGroupedStats$site == "Leipzig" & dfGroupedStats$freq == "8.57Hz",], aes(x = mod, color = mod, ymin = mean-sem, ymax = mean+sem), width = .1) +
  geom_point(data = dfGroupedStats[dfGroupedStats$site == "Leipzig" & dfGroupedStats$freq == "8.57Hz",],
             aes (x = mod, color = mod, y = mean), shape = "diamond", size = 4) +
  scale_color_manual(values = brewer.pal(n = 4, "Oranges")[4:3]) +
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
  geom_line(data = dfStats[dfStats$site == "Leipzig" & dfStats$freq == "15Hz",], 
            aes(x = mod, y = ssvepZ, group = part), color = "gray70") +
  geom_point(data = dfStats[dfStats$site == "Leipzig" & dfStats$freq == "15Hz",], 
             aes(x = mod, y = ssvepZ), color = "gray70") +
  geom_segment(data = dfGroupedStats, x = 1, xend = 2,
               aes(y = mean[site == "Leipzig" & freq == "15Hz" & mod == "box"],
                   yend = mean[site == "Leipzig" & freq == "15Hz" & mod == "sine"]), 
               color = "black") +
  #geom_errorbar(data = dfGroupedStats[dfGroupedStats$site == "Leipzig" & dfGroupedStats$freq == "15Hz",], aes(x = mod, color = mod, ymin = mean-sem, ymax = mean+sem), width = .1) +
  geom_point(data = dfGroupedStats[dfGroupedStats$site == "Leipzig" & dfGroupedStats$freq == "15Hz",],
             aes (x = mod, color = mod, y = mean), shape = "diamond", size = 4) +
  scale_color_manual(values = brewer.pal(n = 4, "BuGn")[4:3]) +
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
sigLabels = c("*","**","*",
              "***","***","")
mAcrossMod = c(mean(dfGroupedStats$mean[dfGroupedStats$site == "Florida" & dfGroupedStats$freq == "6Hz"]),
               mean(dfGroupedStats$mean[dfGroupedStats$site == "Florida" &dfGroupedStats$freq == "8.57Hz"]),
               mean(dfGroupedStats$mean[dfGroupedStats$site == "Florida" & dfGroupedStats$freq == "15Hz"]),
               mean(dfGroupedStats$mean[dfGroupedStats$site == "Leipzig" & dfGroupedStats$freq == "6Hz"]),
               mean(dfGroupedStats$mean[dfGroupedStats$site == "Leipzig" & dfGroupedStats$freq == "8.57Hz"]),
               mean(dfGroupedStats$mean[dfGroupedStats$site == "Leipzig" & dfGroupedStats$freq == "15Hz"]))

# add margins and significance symbols to single plots before joining them
plotFL6 <- plotFL6 + 
           theme(plot.margin = margin(20,5,20,5)) +
           geom_text(data = dfGroupedStats,
                     x = 1.5,y = .98*ymax, label = sigLabels[1],
                     size = fSize*.66)
plotFL857 <- plotFL857 +
             theme(plot.margin = margin(20,5,20,5)) +
             geom_text(data = dfGroupedStats,
                       x = 1.5,y = .98*ymax, label = sigLabels[2],
                       size = fSize*.66)
plotFL15 <- plotFL15 +
            theme(plot.margin = margin(20,5,20,5)) +
            geom_text(data = dfGroupedStats,
                      x = 1.5,y = .98*ymax, label = sigLabels[3],
                      size = fSize*.66)
plotLE6 <- plotLE6 +
           theme(plot.margin = margin(20,5,20,5)) +
           geom_text(data = dfGroupedStats,
                     x = 1.5,y = .98*ymax, label = sigLabels[4],
                     size = fSize*.66)
plotLE857 <- plotLE857 +
             theme(plot.margin = margin(20,5,20,5)) +
             geom_text(data = dfGroupedStats,
                       x = 1.5,y = .98*ymax, label = sigLabels[5],
                       size = fSize*.66)
plotLE15 <- plotLE15 +
            theme(plot.margin = margin(20,5,20,5)) +
            geom_text(data = dfGroupedStats,
                      x = 1.5,y = .98*ymax, label = sigLabels[6],
                      size = fSize*.66)

# join plots and show them in RStudio
meanPlots <- ggarrange(plotFL6, plotFL857, plotFL15, plotLE6, plotLE857, plotLE15,
                      nrow = 2, ncol = 3)
meanPlots

# Save plot asa jpg and pdf
savename = paste0(parentFolder,"/figures/13_means_commPL.pdf")
ggsave(filename = savename, plot = meanPlots, device = "pdf",
       width = 21, height = 15, unit = "cm", limitsize = FALSE)

savename = paste0(parentFolder,"/figures/13_means_commPL.jpg")
ggsave(filename = savename, plot = meanPlots, device = "jpg",
       width = 21, height = 15, unit = "cm", dpi = 300, limitsize = FALSE)




# Alternative: boxplots ---------------------------------------------------

plotFL6 <- ggplot(data = dfStats[dfStats$site == "Florida" & dfStats$freq == "6Hz",], 
                  aes(x = mod, y = ssvepZ, color = mod, fill = mod)) + theme_classic() +
  geom_line(aes(group = part), color = "gray70") +
  geom_boxplot(outlier.shape = NA, alpha = .5) + 
  geom_point() +
  scale_y_continuous(name = "ssVEP amplitude (z)", limits = c(ymin,ymax)) +
  scale_color_manual(values = brewer.pal(n = 4, "Blues")[4:3]) +
  scale_fill_manual(values = brewer.pal(n = 4, "Blues")[4:3]) +
  labs(title = "Florida - 6 Hz") +
  theme(
    plot.title = element_text(size = fSize, color = "black", face = "bold", hjust = .5),
    axis.title.x = element_blank(),
    axis.text.x = element_text(family = fType, color = "black", size = fSize, face = "bold"),
    axis.ticks.x = element_blank(),
    axis.text.y = element_text(family = fType, color = "black", size = fSize),
    axis.title.y = element_text(family = fType, color = "black", size = fSize, face = "bold", margin = margin(r = 20)),
    legend.position = "none"
  )

plotFL857 <- ggplot(data = dfStats[dfStats$site == "Florida" & dfStats$freq == "8.57Hz",], 
                    aes(x = mod, y = ssvepZ, color = mod, fill = mod)) + theme_classic() +
  geom_line(aes(group = part), color = "gray70") +
  geom_boxplot(outlier.shape = NA, alpha = .5) + 
  geom_point() +
  scale_y_continuous(name = "ssVEP amplitude (z)", limits = c(ymin,ymax)) +
  scale_color_manual(values = brewer.pal(n = 4, "Reds")[4:3]) +
  scale_fill_manual(values = brewer.pal(n = 4, "Reds")[4:3]) +
  labs(title = "Florida - 8.57 Hz") +
  theme(
    plot.title = element_text(size = fSize, color = "black", face = "bold", hjust = .5),
    axis.title.x = element_blank(),
    axis.text.x = element_text(family = fType, color = "black", size = fSize, face = "bold"),
    axis.ticks.x = element_blank(),
    axis.text.y = element_text(family = fType, color = "black", size = fSize),
    axis.title.y = element_text(family = fType, color = "black", size = fSize, face = "bold", margin = margin(r = 20)),
    legend.position = "none"
  )

plotFL15 <- ggplot(data = dfStats[dfStats$site == "Florida" & dfStats$freq == "15Hz",], 
                   aes(x = mod, y = ssvepZ, color = mod, fill = mod)) + theme_classic() +
  geom_line(aes(group = part), color = "gray70") +
  geom_boxplot(outlier.shape = NA, alpha = .5) + 
  geom_point() +
  scale_y_continuous(name = "ssVEP amplitude (z)", limits = c(ymin,ymax)) +
  scale_color_manual(values = brewer.pal(n = 4, "Greens")[4:3]) +
  scale_fill_manual(values = brewer.pal(n = 4, "Greens")[4:3]) +
  labs(title = "Florida - 15 Hz") +
  theme(
    plot.title = element_text(size = fSize, color = "black", face = "bold", hjust = .5),
    axis.title.x = element_blank(),
    axis.text.x = element_text(family = fType, color = "black", size = fSize, face = "bold"),
    axis.ticks.x = element_blank(),
    axis.text.y = element_text(family = fType, color = "black", size = fSize),
    axis.title.y = element_text(family = fType, color = "black", size = fSize, face = "bold", margin = margin(r = 20)),
    legend.position = "none"
  )

plotLE6 <- ggplot(data = dfStats[dfStats$site == "Leipzig" & dfStats$freq == "6Hz",], 
                  aes(x = mod, y = ssvepZ, color = mod, fill = mod)) + theme_classic() +
  geom_line(aes(group = part), color = "gray70") +
  geom_boxplot(outlier.shape = NA, alpha = .5) + 
  geom_point() +
  scale_y_continuous(name = "ssVEP amplitude (z)", limits = c(ymin,ymax)) +
  scale_color_manual(values = brewer.pal(n = 4, "Blues")[4:3]) +
  scale_fill_manual(values = brewer.pal(n = 4, "Blues")[4:3]) +
  labs(title = "Leipzig - 6 Hz") +
  theme(
    plot.title = element_text(size = fSize, color = "black", face = "bold", hjust = .5),
    axis.title.x = element_blank(),
    axis.text.x = element_text(family = fType, color = "black", size = fSize, face = "bold"),
    axis.ticks.x = element_blank(),
    axis.text.y = element_text(family = fType, color = "black", size = fSize),
    axis.title.y = element_text(family = fType, color = "black", size = fSize, face = "bold", margin = margin(r = 20)),
    legend.position = "none"
  )

plotLE857 <- ggplot(data = dfStats[dfStats$site == "Leipzig" & dfStats$freq == "8.57Hz",], 
                    aes(x = mod, y = ssvepZ, color = mod, fill = mod)) + theme_classic() +
  geom_line(aes(group = part), color = "gray70") +
  geom_boxplot(outlier.shape = NA, alpha = .5) + 
  geom_point() +
  scale_y_continuous(name = "ssVEP amplitude (z)", limits = c(ymin,ymax)) +
  scale_color_manual(values = brewer.pal(n = 4, "Reds")[4:3]) +
  scale_fill_manual(values = brewer.pal(n = 4, "Reds")[4:3]) +
  labs(title = "Leipzig - 8.57 Hz") +
  theme(
    plot.title = element_text(size = fSize, color = "black", face = "bold", hjust = .5),
    axis.title.x = element_blank(),
    axis.text.x = element_text(family = fType, color = "black", size = fSize, face = "bold"),
    axis.ticks.x = element_blank(),
    axis.text.y = element_text(family = fType, color = "black", size = fSize),
    axis.title.y = element_text(family = fType, color = "black", size = fSize, face = "bold", margin = margin(r = 20)),
    legend.position = "none"
  )

plotLE15 <- ggplot(data = dfStats[dfStats$site == "Leipzig" & dfStats$freq == "15Hz",], 
                   aes(x = mod, y = ssvepZ, color = mod, fill = mod)) + theme_classic() +
  geom_line(aes(group = part), color = "gray70") +
  geom_boxplot(outlier.shape = NA, alpha = .5) + 
  geom_point() +
  scale_y_continuous(name = "ssVEP amplitude (z)", limits = c(ymin,ymax)) +
  scale_color_manual(values = brewer.pal(n = 4, "Greens")[4:3]) +
  scale_fill_manual(values = brewer.pal(n = 4, "Greens")[4:3]) +
  labs(title = "Leipzig - 15 Hz") +
  theme(
    plot.title = element_text(size = fSize, color = "black", face = "bold", hjust = .5),
    axis.title.x = element_blank(),
    axis.text.x = element_text(family = fType, color = "black", size = fSize, face = "bold"),
    axis.ticks.x = element_blank(),
    axis.text.y = element_text(family = fType, color = "black", size = fSize),
    axis.title.y = element_text(family = fType, color = "black", size = fSize, face = "bold", margin = margin(r = 20)),
    legend.position = "none"
  )

plotFL6 <- plotFL6 + theme(plot.margin = margin(20,20,20,20))
plotFL857 <- plotFL857 + theme(plot.margin = margin(20,20,20,20))
plotFL15 <- plotFL15 + theme(plot.margin = margin(20,20,20,20))
plotLE6 <- plotLE6 + theme(plot.margin = margin(20,20,20,20))
plotLE857 <- plotLE857 + theme(plot.margin = margin(20,20,20,20))
plotLE15 <- plotLE15 + theme(plot.margin = margin(20,20,20,20))

boxPlots <- ggarrange(plotFL6, plotFL857, plotFL15, plotLE6, plotLE857, plotLE15,
                      nrow = 2, ncol = 3)

boxPlots