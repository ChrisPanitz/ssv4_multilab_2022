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
library(ggpubr)

# set parent folder
parentFolder <- here()

# load data to plot
loadname <- paste0(parentFolder,"/dataframes/dfSpectra_commPL.csv")
dfSpectra <- read.csv(loadname)
dfSpectra$site <- factor(dfSpectra$site, levels = c("Florida","Leipzig"), labels = c("Florida", "Leipzig"))
dfSpectra$freq <- factor(dfSpectra$freq, levels = c("6Hz","8.57Hz","15Hz"), labels = c("6Hz","8.57Hz","15Hz"))
dfSpectra$mod <- factor(dfSpectra$mod, levels = c("box","sine"), labels = c("box","sine"))

# overwrite DC component amplitude with 0 (for y axis scaling)
dfSpectra$amp[dfSpectra$freqBin == 0] = 0



# Single Spectra Plots ----------------------------------------------------

# set axis limits and frequency resolution (for bar width)
yminFL <- 0
ymaxFL <- max(dfSpectra$amp[dfSpectra$site == "Florida"]) * 1.1
yminLE <- 0
ymaxLE <- max(dfSpectra$amp[dfSpectra$site == "Leipzig"]) * 1.1
resFL <- diff(dfSpectra$freqBin[dfSpectra$site == "Florida"])[1]
resLE <- diff(dfSpectra$freqBin[dfSpectra$site == "Leipzig"])[1]

# Create bar plots with vertical, dashed line at driving frequency;
# One plot per Site and Driving Frequency with the two modulation functions in each plot
specFL6 <- ggplot(data = dfSpectra[dfSpectra$site == "Florida" & dfSpectra$freq == "6Hz",], 
                  aes(x = freqBin, y = amp, fill = mod)) + theme_classic() +
  geom_col(data = dfSpectra[dfSpectra$site == "Florida" & dfSpectra$freq == "6Hz" & dfSpectra$mod == "box",], 
           width = resFL) +
  geom_col(data = dfSpectra[dfSpectra$site == "Florida" & dfSpectra$freq == "6Hz" & dfSpectra$mod == "sine",],
           width = resFL, alpha = .80) +
  geom_vline(xintercept = 6, color = "gray70", linetype = "dashed", size = .2) +
  scale_x_continuous(name = " ", limits = c(xmin,xmax)) +
  scale_y_continuous(name = "Normalized spectral amplitude", limits = c(yminFL,ymaxFL)) +
  scale_color_manual(values = brewer.pal(n = 4, "Purples")[4:3]) +
  scale_fill_manual(values = brewer.pal(n = 4, "Purples")[4:3]) +
  labs(title = "Florida - 6 Hz") +
  theme(
    plot.title = element_text(size = fSize, color = "black", face = "bold", hjust = .5),
    axis.title.x = element_text(family = fType, color = "black", size = fSize,
                                face = "bold", margin = margin(t = 10)),
    axis.text.x = element_text(family = fType, color = "black", size = fSize),
    axis.text.y = element_text(family = fType, color = "black", size = fSize),
    axis.title.y = element_text(family = fType, color = "black", size = fSize,
                                face = "bold", margin = margin(r = 10)),
    legend.position = c(1,1),
    legend.justification = c(1,1),
    legend.title = element_blank(),
    legend.text = element_text(family = fType, color = "black", size = fSize-2),
    legend.key.size = unit(fSize-2, "pt")
  )

specFL857 <- ggplot(data = dfSpectra[dfSpectra$site == "Florida" & dfSpectra$freq == "8.57Hz",], 
                    aes(x = freqBin, y = amp, fill = mod)) + theme_classic() +
  geom_col(data = dfSpectra[dfSpectra$site == "Florida" & dfSpectra$freq == "8.57Hz" & dfSpectra$mod == "box",],
           width = resFL) +
  geom_col(data = dfSpectra[dfSpectra$site == "Florida" & dfSpectra$freq == "8.57Hz" & dfSpectra$mod == "sine",],
           width = resFL, alpha = .80) +
  geom_vline(xintercept = 60/7, color = "gray70", linetype = "dashed", size = .2) +
  scale_x_continuous(name = " ", limits = c(xmin,xmax)) +
  scale_y_continuous(name = " ", limits = c(yminFL,ymaxFL)) +
  scale_color_manual(values = brewer.pal(n = 4, "Oranges")[4:3]) +
  scale_fill_manual(values = brewer.pal(n = 4, "Oranges")[4:3]) +
  labs(title = "Florida - 8.57 Hz") +
  theme(
    plot.title = element_text(size = fSize, color = "black", face = "bold", hjust = .5),
    axis.title.x = element_text(family = fType, color = "black", size = fSize,
                                face = "bold", margin = margin(t = 10)),
    axis.text.x = element_text(family = fType, color = "black", size = fSize),
    axis.text.y = element_text(family = fType, color = "black", size = fSize),
    axis.title.y = element_text(family = fType, color = "black", size = fSize,
                                face = "bold", margin = margin(r = 10)),
    legend.position = c(1,1),
    legend.justification = c(1,1),
    legend.title = element_blank(),
    legend.text = element_text(family = fType, color = "black", size = fSize-2),
    legend.key.size = unit(fSize-2, "pt")
  )

specFL15 <- ggplot(data = dfSpectra[dfSpectra$site == "Florida" & dfSpectra$freq == "15Hz",], 
                   aes(x = freqBin, y = amp, fill = mod)) + theme_classic() +
  geom_col(data = dfSpectra[dfSpectra$site == "Florida" & dfSpectra$freq == "15Hz" & dfSpectra$mod == "box",],
           width = resFL) +
  geom_col(data = dfSpectra[dfSpectra$site == "Florida" & dfSpectra$freq == "15Hz" & dfSpectra$mod == "sine",],
           width = resFL, alpha = .80) +
  geom_vline(xintercept = 15, color = "gray70", linetype = "dashed", size = .2) +
  scale_x_continuous(name = " ", limits = c(xmin,xmax)) +
  scale_y_continuous(name = " ", limits = c(yminFL,ymaxFL)) +
  scale_color_manual(values = brewer.pal(n = 4, "BuGn")[4:3]) +
  scale_fill_manual(values = brewer.pal(n = 4, "BuGn")[4:3]) +
  labs(title = "Florida - 15 Hz") +
  theme(
    plot.title = element_text(size = fSize, color = "black", face = "bold", hjust = .5),
    axis.title.x = element_text(family = fType, color = "black", size = fSize,
                                face = "bold", margin = margin(t = 10)),
    axis.text.x = element_text(family = fType, color = "black", size = fSize),
    axis.text.y = element_text(family = fType, color = "black", size = fSize),
    axis.title.y = element_text(family = fType, color = "black", size = fSize,
                                face = "bold", margin = margin(r = 10)),
    legend.position = c(1,1),
    legend.justification = c(1,1),
    legend.title = element_blank(),
    legend.text = element_text(family = fType, color = "black", size = fSize-2),
    legend.key.size = unit(fSize-2, "pt")
  )

specLE6 <- ggplot(data = dfSpectra[dfSpectra$site == "Leipzig" & dfSpectra$freq == "6Hz",], 
                  aes(x = freqBin, y = amp, fill = mod)) + theme_classic() +
  geom_col(data = dfSpectra[dfSpectra$site == "Leipzig" & dfSpectra$freq == "6Hz" & dfSpectra$mod == "box",],
           width = resLE) +
  geom_col(data = dfSpectra[dfSpectra$site == "Leipzig" & dfSpectra$freq == "6Hz" & dfSpectra$mod == "sine",],
           width = resLE, alpha = .80) +
  geom_vline(xintercept = 6, color = "gray70", linetype = "dashed", size = .2) +
  scale_x_continuous(name = " ", limits = c(xmin,xmax)) +
  scale_y_continuous(name = "Normalized spectral amplitude", limits = c(yminLE,ymaxLE)) +
  scale_color_manual(values = brewer.pal(n = 4, "Purples")[4:3]) +
  scale_fill_manual(values = brewer.pal(n = 4, "Purples")[4:3]) +
  labs(title = "Leipzig - 6 Hz") +
  theme(
    plot.title = element_text(size = fSize, color = "black", face = "bold", hjust = .5),
    axis.title.x = element_text(family = fType, color = "black", size = fSize,
                                face = "bold", margin = margin(t = 10)),
    axis.text.x = element_text(family = fType, color = "black", size = fSize),
    axis.text.y = element_text(family = fType, color = "black", size = fSize),
    axis.title.y = element_text(family = fType, color = "black", size = fSize,
                                face = "bold", margin = margin(r = 10)),
    legend.position = c(1,1),
    legend.justification = c(1,1),
    legend.title = element_blank(),
    legend.text = element_text(family = fType, color = "black", size = fSize-2),
    legend.key.size = unit(fSize-2, "pt")
  )

specLE857 <- ggplot(data = dfSpectra[dfSpectra$site == "Leipzig" & dfSpectra$freq == "8.57Hz",], 
                    aes(x = freqBin, y = amp, fill = mod)) + theme_classic() +
  geom_col(data = dfSpectra[dfSpectra$site == "Leipzig" & dfSpectra$freq == "8.57Hz" & dfSpectra$mod == "box",],
           width = resLE) +
  geom_col(data = dfSpectra[dfSpectra$site == "Leipzig" & dfSpectra$freq == "8.57Hz" & dfSpectra$mod == "sine",],
           width = resLE, alpha = .80) +
  geom_vline(xintercept = 60/7, color = "gray70", linetype = "dashed", size = .2) +
  scale_x_continuous(name = "Frequency (Hz)", limits = c(xmin,xmax)) +
  scale_y_continuous(name = " ", limits = c(yminLE,ymaxLE)) +
  scale_color_manual(values = brewer.pal(n = 4, "Oranges")[4:3]) +
  scale_fill_manual(values = brewer.pal(n = 4, "Oranges")[4:3]) +
  labs(title = "Leipzig - 8.57 Hz") +
  theme(
    plot.title = element_text(size = fSize, color = "black", face = "bold", hjust = .5),
    axis.title.x = element_text(family = fType, color = "black", size = fSize,
                                face = "bold", margin = margin(t = 10)),
    axis.text.x = element_text(family = fType, color = "black", size = fSize),
    axis.text.y = element_text(family = fType, color = "black", size = fSize),
    axis.title.y = element_text(family = fType, color = "black", size = fSize,
                                face = "bold", margin = margin(r = 10)),
    legend.position = c(1,1),
    legend.justification = c(1,1),
    legend.title = element_blank(),
    legend.text = element_text(family = fType, color = "black", size = fSize-2),
    legend.key.size = unit(fSize-2, "pt")
  )

specLE15 <- ggplot(data = dfSpectra[dfSpectra$site == "Leipzig" & dfSpectra$freq == "15Hz",], 
                   aes(x = freqBin, y = amp, fill = mod)) + theme_classic() +
  geom_col(data = dfSpectra[dfSpectra$site == "Leipzig" & dfSpectra$freq == "15Hz" & dfSpectra$mod == "box",],
           width = resLE) +
  geom_col(data = dfSpectra[dfSpectra$site == "Leipzig" & dfSpectra$freq == "15Hz" & dfSpectra$mod == "sine",],
           width = resLE, alpha = .80) +
  geom_vline(xintercept = 15, color = "gray70", linetype = "dashed", size = .2) +
  scale_x_continuous(name = " ", limits = c(xmin,xmax)) +
  scale_y_continuous(name = " ", limits = c(yminLE,ymaxLE)) +
  scale_color_manual(values = brewer.pal(n = 4, "BuGn")[4:3]) +
  scale_fill_manual(values = brewer.pal(n = 4, "BuGn")[4:3]) +
  labs(title = "Leipzig - 15 Hz") +
  theme(
    plot.title = element_text(size = fSize, color = "black", face = "bold", hjust = .5),
    axis.title.x = element_text(family = fType, color = "black", size = fSize,
                                face = "bold", margin = margin(t = 10)),
    axis.text.x = element_text(family = fType, color = "black", size = fSize),
    axis.text.y = element_text(family = fType, color = "black", size = fSize),
    axis.title.y = element_text(family = fType, color = "black", size = fSize,
                                face = "bold", margin = margin(r = 10)),
    legend.position = c(1,1),
    legend.justification = c(1,1),
    legend.title = element_blank(),
    legend.text = element_text(family = fType, color = "black", size = fSize-2),
    legend.key.size = unit(fSize-2, "pt")
  )

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

# Save plot as jpg & pdf
savename = paste0(parentFolder,"/figures/12_spectra_commPL.pdf")
ggsave(filename = savename, plot = specPlots, device = "pdf",
       width = 21, height = 15, unit = "cm", limitsize = FALSE)

savename = paste0(parentFolder,"/figures/12_spectra_commPL.jpg")
ggsave(filename = savename, plot = specPlots, device = "jpg",
       width = 21, height = 15, unit = "cm", dpi = 300, limitsize = FALSE)




# Alternative Plot With Areas Instead of Bars -----------------------------

specFL6 <- ggplot(data = dfSpectra[dfSpectra$site == "Florida" & dfSpectra$freq == "6Hz",], 
                  aes(x = freqBin, y = amp, fill = mod)) + theme_classic() +
  geom_vline(xintercept = 6, color = "gray70") +
  geom_area(data = dfSpectra[dfSpectra$site == "Florida" & dfSpectra$freq == "6Hz" & dfSpectra$mod == "box",]) +
  geom_area(data = dfSpectra[dfSpectra$site == "Florida" & dfSpectra$freq == "6Hz" & dfSpectra$mod == "sine",], alpha = .80) +
  scale_x_continuous(name = "Frequency (Hz)", limits = c(xmin,xmax)) +
  scale_y_continuous(name = "Spectral amplitude (µV)", limits = c(yminFL,ymaxFL)) +
  scale_fill_manual(values = brewer.pal(n = 4, "Blues")[4:3]) +
  labs(title = "Florida - 6 Hz") +
  theme(
    plot.title = element_text(size = fSize, color = "black", face = "bold", hjust = .5),
    axis.title.x = element_text(family = fType, color = "black", size = fSize, face = "bold", margin = margin(t = 20)),
    axis.text.x = element_text(family = fType, color = "black", size = fSize, face = "bold"),
    axis.text.y = element_text(family = fType, color = "black", size = fSize),
    axis.title.y = element_text(family = fType, color = "black", size = fSize, face = "bold", margin = margin(r = 20)),
    legend.position = "none"
  )

specFL857 <- ggplot(data = dfSpectra[dfSpectra$site == "Florida" & dfSpectra$freq == "8.57Hz",], 
                    aes(x = freqBin, y = amp, fill = mod)) + theme_classic() +
  geom_vline(xintercept = 60/7, color = "gray70") +
  geom_area(data = dfSpectra[dfSpectra$site == "Florida" & dfSpectra$freq == "8.57Hz" & dfSpectra$mod == "box",]) +
  geom_area(data = dfSpectra[dfSpectra$site == "Florida" & dfSpectra$freq == "8.57Hz" & dfSpectra$mod == "sine",], alpha = .80) +
  scale_x_continuous(name = "Frequency (Hz)", limits = c(xmin,xmax)) +
  scale_y_continuous(name = "Spectral amplitude (µV)", limits = c(yminFL,ymaxFL)) +
  scale_fill_manual(values = brewer.pal(n = 4, "Oranges")[4:3]) +
  labs(title = "Florida - 8.57 Hz") +
  theme(
    plot.title = element_text(size = fSize, color = "black", face = "bold", hjust = .5),
    axis.title.x = element_text(family = fType, color = "black", size = fSize, face = "bold", margin = margin(t = 20)),
    axis.text.x = element_text(family = fType, color = "black", size = fSize, face = "bold"),
    axis.text.y = element_text(family = fType, color = "black", size = fSize),
    axis.title.y = element_text(family = fType, color = "black", size = fSize, face = "bold", margin = margin(r = 20)),
    legend.position = "none"
  )

specFL15 <- ggplot(data = dfSpectra[dfSpectra$site == "Florida" & dfSpectra$freq == "15Hz",], 
                   aes(x = freqBin, y = amp, fill = mod)) + theme_classic() +
  geom_vline(xintercept = 15, color = "gray70") +
  geom_area(data = dfSpectra[dfSpectra$site == "Florida" & dfSpectra$freq == "15Hz" & dfSpectra$mod == "box",]) +
  geom_area(data = dfSpectra[dfSpectra$site == "Florida" & dfSpectra$freq == "15Hz" & dfSpectra$mod == "sine",], alpha = .80) +
  scale_x_continuous(name = "Frequency (Hz)", limits = c(xmin,xmax)) +
  scale_y_continuous(name = "Spectral amplitude (µV)", limits = c(yminFL,ymaxFL)) +
  scale_fill_manual(values = brewer.pal(n = 4, "Greens")[4:3]) +
  labs(title = "Florida - 15 Hz") +
  theme(
    plot.title = element_text(size = fSize, color = "black", face = "bold", hjust = .5),
    axis.title.x = element_text(family = fType, color = "black", size = fSize, face = "bold", margin = margin(t = 20)),
    axis.text.x = element_text(family = fType, color = "black", size = fSize, face = "bold"),
    axis.text.y = element_text(family = fType, color = "black", size = fSize),
    axis.title.y = element_text(family = fType, color = "black", size = fSize, face = "bold", margin = margin(r = 20)),
    legend.position = "none"
  )

specLE6 <- ggplot(data = dfSpectra[dfSpectra$site == "Leipzig" & dfSpectra$freq == "6Hz",], 
                  aes(x = freqBin, y = amp, fill = mod)) + theme_classic() +
  geom_vline(xintercept = 6, color = "gray70") +
  geom_area(data = dfSpectra[dfSpectra$site == "Leipzig" & dfSpectra$freq == "6Hz" & dfSpectra$mod == "box",]) +
  geom_area(data = dfSpectra[dfSpectra$site == "Leipzig" & dfSpectra$freq == "6Hz" & dfSpectra$mod == "sine",], alpha = .80) +
  scale_x_continuous(name = "Frequency (Hz)", limits = c(xmin,xmax)) +
  scale_y_continuous(name = "Spectral amplitude (µV)", limits = c(yminLE,ymaxLE)) +
  scale_fill_manual(values = brewer.pal(n = 4, "Blues")[4:3]) +
  labs(title = "Leipzig - 6 Hz") +
  theme(
    plot.title = element_text(size = fSize, color = "black", face = "bold", hjust = .5),
    axis.title.x = element_text(family = fType, color = "black", size = fSize, face = "bold", margin = margin(t = 20)),
    axis.text.x = element_text(family = fType, color = "black", size = fSize, face = "bold"),
    axis.text.y = element_text(family = fType, color = "black", size = fSize),
    axis.title.y = element_text(family = fType, color = "black", size = fSize, face = "bold", margin = margin(r = 20)),
    legend.position = "none"
  )

specLE857 <- ggplot(data = dfSpectra[dfSpectra$site == "Leipzig" & dfSpectra$freq == "8.57Hz",], 
                    aes(x = freqBin, y = amp, fill = mod)) + theme_classic() +
  geom_vline(xintercept = 60/7, color = "gray70") +
  geom_area(data = dfSpectra[dfSpectra$site == "Leipzig" & dfSpectra$freq == "8.57Hz" & dfSpectra$mod == "box",]) +
  geom_area(data = dfSpectra[dfSpectra$site == "Leipzig" & dfSpectra$freq == "8.57Hz" & dfSpectra$mod == "sine",], alpha = .80) +
  scale_x_continuous(name = "Frequency (Hz)", limits = c(xmin,xmax)) +
  scale_y_continuous(name = "Spectral amplitude (µV)", limits = c(yminLE,ymaxLE)) +
  scale_fill_manual(values = brewer.pal(n = 4, "Oranges")[4:3]) +
  labs(title = "Leipzig - 8.57 Hz") +
  theme(
    plot.title = element_text(size = fSize, color = "black", face = "bold", hjust = .5),
    axis.title.x = element_text(family = fType, color = "black", size = fSize, face = "bold", margin = margin(t = 20)),
    axis.text.x = element_text(family = fType, color = "black", size = fSize, face = "bold"),
    axis.text.y = element_text(family = fType, color = "black", size = fSize),
    axis.title.y = element_text(family = fType, color = "black", size = fSize, face = "bold", margin = margin(r = 20)),
    legend.position = "none"
  )

specLE15 <- ggplot(data = dfSpectra[dfSpectra$site == "Leipzig" & dfSpectra$freq == "15Hz",], 
                   aes(x = freqBin, y = amp, fill = mod)) + theme_classic() +
  geom_vline(xintercept = 15, color = "gray70") +
  geom_area(data = dfSpectra[dfSpectra$site == "Leipzig" & dfSpectra$freq == "15Hz" & dfSpectra$mod == "box",]) +
  geom_area(data = dfSpectra[dfSpectra$site == "Leipzig" & dfSpectra$freq == "15Hz" & dfSpectra$mod == "sine",], alpha = .80) +
  scale_x_continuous(name = "Frequency (Hz)", limits = c(xmin,xmax)) +
  scale_y_continuous(name = "Spectral amplitude (µV)", limits = c(yminLE,ymaxLE)) +
  scale_fill_manual(values = brewer.pal(n = 4, "Greens")[4:3]) +
  labs(title = "Leipzig - 15 Hz") +
  theme(
    plot.title = element_text(size = fSize, color = "black", face = "bold", hjust = .5),
    axis.title.x = element_text(family = fType, color = "black", size = fSize, face = "bold", margin = margin(t = 20)),
    axis.text.x = element_text(family = fType, color = "black", size = fSize, face = "bold"),
    axis.text.y = element_text(family = fType, color = "black", size = fSize),
    axis.title.y = element_text(family = fType, color = "black", size = fSize, face = "bold", margin = margin(r = 20)),
    legend.position = "none"
  )


specFL6 <- specFL6 + theme(plot.margin = margin(20,20,20,20))
specFL857 <- specFL857 + theme(plot.margin = margin(20,20,20,20))
specFL15 <- specFL15 + theme(plot.margin = margin(20,20,20,20))
specLE6 <- specLE6 + theme(plot.margin = margin(20,20,20,20))
specLE857 <- specLE857 + theme(plot.margin = margin(20,20,20,20))
specLE15 <- specLE15 + theme(plot.margin = margin(20,20,20,20))

specPlots <- ggarrange(specFL6, specFL857, specFL15, specLE6, specLE857, specLE15,
                       nrow = 2, ncol = 3)

specPlots