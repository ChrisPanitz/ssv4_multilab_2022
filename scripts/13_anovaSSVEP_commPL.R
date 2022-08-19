# --- author: Christian Panitz
# --- encoding: en_US.UTF-8
# --- R version: 4.0.3 (2020-10-10) -- "Bunny-Wunnies Freak Out"
# --- RStudio version: 2022.02.3
# --- script version: August 2022
# --- content: ANOVAs for ssVEP data based (common pipeline)


# Loading Packages, Setting Path & Random Number Generator Seed -----------

# load required libraries
library(here)
library(rstatix)
library(ez)
library(BayesFactor)
library(bayestestR)

# set parent folder
parentFolder <- here()

# seed for random number generator (picked by random number generator on random.org)
rngSeed <- 249336527



# Loading Data ------------------------------------------------------------

# load data frame for ANOVA
loadname <- paste0(parentFolder,"/dataframes/dfSSVEP_commPL.csv")
df <- read.csv(loadname)
df$part <- factor(df$part)
df$lab <- factor(df$lab, levels = c("Florida","Leipzig"), labels = c("Florida","Leipzig"))
df$freq <- factor(df$freq, levels = c("6Hz","8.57Hz","15Hz"), labels = c("6Hz","8.57Hz","15Hz"))
df$mod = factor(df$mod, levels = c("square","sine"), labels = c("square","sine"))



# Frequentist ANOVA -------------------------------------------------------

# frequentist Frequency x Modulation x Site ANOVA on z-standardized ssvep amplitudes
anovaSSVEP <- anova_test(
  data = df,
  dv = ssvepZ,
  wid = part,
  within = c(freq, mod),
  between = lab,
  type = 3,
  effect.size = "pes",
  detailed = TRUE
); anovaSSVEP
# quick plot of means and SEMs
plotSSVEP <- ezPlot(
  data = df,
  dv = ssvepZ,
  wid = part,
  within = .(freq, mod),
  between = .(lab),
  x = freq,
  split = mod,
  col = lab
); plotSSVEP



# Bayesian ANOVA ----------------------------------------------------------

# Bayesian Frequency x Modulation x lab ANOVA on z-standardized ssvep amplitudes
set.seed(rngSeed); bayAnovaSSVEP <- anovaBF(
                                    formula = ssvepZ ~ freq*mod*lab + part,
                                    data = df,
                                    whichRandom = "part",
                                    whichModels = "all",
                                    iterations = 10000
); print(bayAnovaSSVEP)

# extract 5 best models: sort all Bayes Factors in decending order, and get indices
# of Factors in the Bayes ANOVA object; also return BF relative to null model
# and relative to best model
bfSorted <- sort(exp(bayAnovaSSVEP@bayesFactor[["bf"]]), decreasing = TRUE)
match(bfSorted[1:5], exp(bayAnovaSSVEP@bayesFactor[["bf"]]))
bfSorted[1:5]
bfSorted[1:5] / bfSorted[1]

# compute Bayes Inclusion Factors
bf_inclusion(bayAnovaSSVEP, match_models = FALSE)