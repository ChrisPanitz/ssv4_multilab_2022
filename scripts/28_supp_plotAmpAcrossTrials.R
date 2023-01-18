# --- author: Christian Panitz
# --- encoding: en_US.UTF-8
# --- R version: 4.0.3 (2020-10-10) -- "Bunny-Wunnies Freak Out"
# --- RStudio version: 2022.02.3
# --- script version: December 2022
# --- content: plotting single-trial amplitudes across trials (common pipeline, single-trial FFT)



# Packages & Path Setting -------------------------------------------------

# load required libraries
library(here)
library(ggplot2)
library(RColorBrewer)
library(ggpubr)

# set parent folder
parentFolder <- here()

# set path for raw data (individual spectra) to read
loadname <- paste0(parentFolder,"/dataframes/supp_dfTimeCourses_commPL_singleTrialFFT_plotting.csv")
dfTimeCourse <- read.csv(loadname)

# for each condition, files in the folder are listed, values for participant ID,
# lab, condition, as well as the ssvep amplitude at predefined frequencies & channels

dfTimeCourse$lab <- factor(dfTimeCourse$lab, levels = c("Florida","Leipzig"))
dfTimeCourse$freq <- factor(dfTimeCourse$freq, levels = c("6Hz","8.57Hz","15Hz"))
dfTimeCourse$mod <- factor(dfTimeCourse$mod, levels = c("square","sine"))

yminFL <- min(dfTimeCourse$amplitude_smoothed[dfTimeCourse$lab == "Florida"])
ymaxFL <- max(dfTimeCourse$amplitude_smoothed[dfTimeCourse$lab == "Florida"])
yminLE <- min(dfTimeCourse$amplitude_smoothed[dfTimeCourse$lab == "Leipzig"])
ymaxLE <- max(dfTimeCourse$amplitude_smoothed[dfTimeCourse$lab == "Leipzig"])

timeCourseFL6 <- ggplot(dfTimeCourse[dfTimeCourse$lab == "Florida" & dfTimeCourse$freq == "6Hz",], 
                        aes(x = trial, y = amplitude_smoothed, color = mod)) + theme_classic() +
  geom_line() +
  scale_color_manual(values = brewer.pal(4,"Purples")[4:3]) +
  scale_y_continuous(limits = c(yminFL,ymaxFL), name = "ssVEP amplitude") +
  scale_x_continuous(name = "trial #") +
  labs(title = "Florida - 6 Hz") +
  theme(
    plot.title = element_text(color = "black", face = "bold", hjust = .5),
    legend.title = element_blank(),
    legend.position = "top"
  )

timeCourseFL857 <- ggplot(dfTimeCourse[dfTimeCourse$lab == "Florida" & dfTimeCourse$freq == "8.57Hz",], 
                          aes(x = trial, y = amplitude_smoothed, color = mod)) + theme_classic() +
  geom_line() +
  scale_color_manual(values = brewer.pal(4,"Oranges")[4:3]) +
  scale_y_continuous(limits = c(yminFL,ymaxFL), name = "ssVEP amplitude") +
  scale_x_continuous(name = "trial #") +
  labs(title = "Florida - 8.57 Hz") +
  theme(
    plot.title = element_text(color = "black", face = "bold", hjust = .5),
    legend.title = element_blank(),
    legend.position = "top"
  )

timeCourseFL15 <- ggplot(dfTimeCourse[dfTimeCourse$lab == "Florida" & dfTimeCourse$freq == "15Hz",], 
                         aes(x = trial, y = amplitude_smoothed, color = mod)) + theme_classic() +
  geom_line() +
  scale_color_manual(values = brewer.pal(4,"BuGn")[4:3]) +
  scale_y_continuous(limits = c(yminFL,ymaxFL), name = "ssVEP amplitude") +
  scale_x_continuous(name = "trial #") +
  labs(title = "Florida - 15 Hz") +
  theme(
    plot.title = element_text(color = "black", face = "bold", hjust = .5),
    legend.title = element_blank(),
    legend.position = "top"
  )

timeCourseLE6 <- ggplot(dfTimeCourse[dfTimeCourse$lab == "Leipzig" & dfTimeCourse$freq == "6Hz",], 
                        aes(x = trial, y = amplitude_smoothed, color = mod)) + theme_classic() +
  geom_line() +
  scale_color_manual(values = brewer.pal(4,"Purples")[4:3]) +
  scale_y_continuous(limits = c(yminLE,ymaxLE), name = "ssVEP amplitude") +
  scale_x_continuous(name = "trial #") +
  labs(title = "Leipzig - 6 Hz") +
  theme(
    plot.title = element_text(color = "black", face = "bold", hjust = .5),
    legend.title = element_blank(),
    legend.position = "top"
  )

timeCourseLE857 <- ggplot(dfTimeCourse[dfTimeCourse$lab == "Leipzig" & dfTimeCourse$freq == "8.57Hz",], 
                          aes(x = trial, y = amplitude_smoothed, color = mod)) + theme_classic() +
  geom_line() +
  scale_color_manual(values = brewer.pal(4,"Oranges")[4:3]) +
  scale_y_continuous(limits = c(yminLE,ymaxLE), name = "ssVEP amplitude") +
  scale_x_continuous(name = "trial #") +
  labs(title = "Leipzig - 8.57 Hz") +
  theme(
    plot.title = element_text(color = "black", face = "bold", hjust = .5),
    legend.title = element_blank(),
    legend.position = "top"
  )

timeCourseLE15 <- ggplot(dfTimeCourse[dfTimeCourse$lab == "Leipzig" & dfTimeCourse$freq == "15Hz",], 
                         aes(x = trial, y = amplitude_smoothed, color = mod)) + theme_classic() +
  geom_line() +
  scale_color_manual(values = brewer.pal(4,"BuGn")[4:3]) +
  scale_y_continuous(limits = c(yminLE,ymaxLE), name = "ssVEP amplitude") +
  scale_x_continuous(name = "trial #") +
  labs(title = "Leipzig - 15 Hz") +
  theme(
    plot.title = element_text(color = "black", face = "bold", hjust = .5),
    legend.title = element_blank(),
    legend.position = "top"
  )


timeCourses <- ggarrange(timeCourseFL6, timeCourseFL857, timeCourseFL15, 
                         timeCourseLE6, timeCourseLE857, timeCourseLE15,
                         nrow = 2, ncol = 3)
timeCourses



# Save figure
savename = paste0(parentFolder,"/figures/28_supp_amplitudeAcrossTrials.pdf")
ggsave(filename = savename, plot = timeCourses, device = "pdf",
       width = 21, height = 15, unit = "cm", limitsize = FALSE)

savename = paste0(parentFolder,"/figures/28_supp_amplitudeAcrossTrials.jpg")
ggsave(filename = savename, plot = timeCourses, device = "jpg",
       width = 21, height = 15, unit = "cm", limitsize = FALSE)