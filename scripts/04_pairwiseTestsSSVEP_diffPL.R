# --- author: Christian Panitz
# --- encoding: en_US.UTF-8
# --- R version: 4.0.3 (2020-10-10) -- "Bunny-Wunnies Freak Out"
# --- RStudio version: 2022.02.3
# --- script version: August 2022
# --- content: pairwise tests for ssVEP (different pipelines)


# Loading Packages, Setting Path & Random Number Generator Seed -----------

# load required libraries
library(here)
library(tidyr)
library(rstatix)
library(BayesFactor)

# set parent folder
parentFolder <- here()

# seed for random number generator
rngSeed <- 249336527



# Loading Data & Creating Data Frames -------------------------------------

# load data frame
loadname <- paste0(parentFolder,"/dataframes/dfSSVEP_diffPL.csv")
df <- read.csv(loadname)
df$part <- factor(df$part)
df$lab <- factor(df$lab, levels = c("Florida","Leipzig"), labels = c("Florida","Leipzig"))
df$freq <- factor(df$freq, levels = c("6Hz","8.57Hz","15Hz"), labels = c("6Hz","8.57Hz","15Hz"))
df$mod = factor(df$mod, levels = c("square","sine"), labels = c("square","sine"))

# create dfs to test main effects
# average across mod conditions to test freq effects
dfFreqEffects <- pivot_wider(data = df, id_cols = c(part,lab,freq), names_from = c(mod), values_from = c(ssvepZ))
dfFreqEffects$ssvep <- rowMeans(dfFreqEffects[,c("square","sine")])
# average across freq conditions to test mod effects
dfModEffects <- pivot_wider(data = df, id_cols = c(part,lab,mod), names_from = c(freq), values_from = c(ssvepZ))
dfModEffects$ssvep <- rowMeans(dfModEffects[,c("6Hz","8.57Hz","15Hz")])



# Pairwise Comparisons in Florida sample ----------------------------------
# Frequentist t-tests, Cohen's d, and Bayesian t-tests

# Main Effect Frequency Florida
t_test(dfFreqEffects[dfFreqEffects$lab == "Florida",], ssvep ~ freq, paired = TRUE, p.adjust.method = "none")
cohens_d(dfFreqEffects[dfFreqEffects$lab == "Florida",], ssvep ~ freq, paired = TRUE)
ttestBF(x = dfFreqEffects$ssvep[dfFreqEffects$lab == "Florida" & dfFreqEffects$freq == "6Hz"],
        y = dfFreqEffects$ssvep[dfFreqEffects$lab == "Florida" & dfFreqEffects$freq == "8.57Hz"],
        nullInterval = NULL, paired = TRUE);
ttestBF(x = dfFreqEffects$ssvep[dfFreqEffects$lab == "Florida" & dfFreqEffects$freq == "6Hz"],
        y = dfFreqEffects$ssvep[dfFreqEffects$lab == "Florida" & dfFreqEffects$freq == "15Hz"],
        nullInterval = NULL, paired = TRUE);
ttestBF(x = dfFreqEffects$ssvep[dfFreqEffects$lab == "Florida" & dfFreqEffects$freq == "8.57Hz"],
        y = dfFreqEffects$ssvep[dfFreqEffects$lab == "Florida" & dfFreqEffects$freq == "15Hz"],
        nullInterval = NULL, paired = TRUE);

# Main Effect Mod Florida
t_test(dfModEffects[dfModEffects$lab == "Florida",], ssvep ~ mod, paired = TRUE, p.adjust.method = "none")
cohens_d(dfModEffects[dfModEffects$lab == "Florida",], ssvep ~ mod, paired = TRUE)
ttestBF(x = dfModEffects$ssvep[dfModEffects$lab == "Florida" & dfModEffects$mod == "square"],
        y = dfModEffects$ssvep[dfModEffects$lab == "Florida" & dfModEffects$mod == "sine"],
        nullInterval = NULL, paired = TRUE);

# Mod effects for different frequencies (simple  effects)
t_test(group_by(df[df$lab == "Florida",], freq),
                ssvep ~ mod, paired = TRUE, p.adjust.method = "none")
cohens_d(group_by(df[df$lab == "Florida",], freq),
         ssvep ~ mod, paired = TRUE)
ttestBF(x = df$ssvepZ[df$lab == "Florida" & df$freq == "6Hz" & df$mod == "square"],
        y = df$ssvepZ[df$lab == "Florida" & df$freq == "6Hz" & df$mod == "sine"],
        nullInterval = NULL, paired = TRUE)
ttestBF(x = df$ssvepZ[df$lab == "Florida" & df$freq == "8.57Hz" & df$mod == "square"],
        y = df$ssvepZ[df$lab == "Florida" & df$freq == "8.57Hz" & df$mod == "sine"],
        nullInterval = NULL, paired = TRUE)
ttestBF(x = df$ssvepZ[df$lab == "Florida" & df$freq == "15Hz" & df$mod == "square"],
        y = df$ssvepZ[df$lab == "Florida" & df$freq == "15Hz" & df$mod == "sine"],
        nullInterval = NULL, paired = TRUE)



# Pairwise Comparisons in Leipzig sample ----------------------------------
# Frequentist t-tests, Cohen's d, and Bayesian t-tests

# Main Effect Frequency Leipzig
t_test(dfFreqEffects[dfFreqEffects$lab == "Leipzig",], ssvep ~ freq, paired = TRUE, p.adjust.method = "none")
cohens_d(dfFreqEffects[dfFreqEffects$lab == "Leipzig",], ssvep ~ freq, paired = TRUE)
ttestBF(x = dfFreqEffects$ssvep[dfFreqEffects$lab == "Leipzig" & dfFreqEffects$freq == "6Hz"],
        y = dfFreqEffects$ssvep[dfFreqEffects$lab == "Leipzig" & dfFreqEffects$freq == "8.57Hz"],
        nullInterval = NULL, paired = TRUE);
ttestBF(x = dfFreqEffects$ssvep[dfFreqEffects$lab == "Leipzig" & dfFreqEffects$freq == "6Hz"],
        y = dfFreqEffects$ssvep[dfFreqEffects$lab == "Leipzig" & dfFreqEffects$freq == "15Hz"],
        nullInterval = NULL, paired = TRUE);
ttestBF(x = dfFreqEffects$ssvep[dfFreqEffects$lab == "Leipzig" & dfFreqEffects$freq == "8.57Hz"],
        y = dfFreqEffects$ssvep[dfFreqEffects$lab == "Leipzig" & dfFreqEffects$freq == "15Hz"],
        nullInterval = NULL, paired = TRUE);

# Main Effect Mod Leipzig
t_test(dfModEffects[dfModEffects$lab == "Leipzig",], ssvep ~ mod, paired = TRUE, p.adjust.method = "none")
cohens_d(dfModEffects[dfModEffects$lab == "Leipzig",], ssvep ~ mod, paired = TRUE)
ttestBF(x = dfModEffects$ssvep[dfModEffects$lab == "Leipzig" & dfModEffects$mod == "square"],
        y = dfModEffects$ssvep[dfModEffects$lab == "Leipzig" & dfModEffects$mod == "sine"],
        nullInterval = NULL, paired = TRUE);

# Mod effects for different frequencies (simple effects)
t_test(group_by(df[df$lab == "Leipzig",], freq),
                ssvep ~ mod, paired = TRUE, p.adjust.method = "none")
cohens_d(group_by(df[df$lab == "Leipzig",], freq),
         ssvep ~ mod, paired = TRUE)
ttestBF(x = df$ssvepZ[df$lab == "Leipzig" & df$freq == "6Hz" & df$mod == "square"],
        y = df$ssvepZ[df$lab == "Leipzig" & df$freq == "6Hz" & df$mod == "sine"],
        nullInterval = NULL, paired = TRUE)
ttestBF(x = df$ssvepZ[df$lab == "Leipzig" & df$freq == "8.57Hz" & df$mod == "square"],
        y = df$ssvepZ[df$lab == "Leipzig" & df$freq == "8.57Hz" & df$mod == "sine"],
        nullInterval = NULL, paired = TRUE)
ttestBF(x = df$ssvepZ[df$lab == "Leipzig" & df$freq == "15Hz" & df$mod == "square"],
        y = df$ssvepZ[df$lab == "Leipzig" & df$freq == "15Hz" & df$mod == "sine"],
        nullInterval = NULL, paired = TRUE)
