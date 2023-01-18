# --- author: Christian Panitz
# --- encoding: en_US.UTF-8
# --- R version: 4.0.3 (2020-10-10) -- "Bunny-Wunnies Freak Out"
# --- RStudio version: 2022.02.3
# --- script version: December 2022
# --- content: Gender x Driving Frequency x Modualation Function ANOVAs with ssVEP amplitudes & SNR (common pipeline)



# Header ------------------------------------------------------------------
# For quicker computation of Bayesian ANOVA. From BayesFactor help:
# "if TRUE use multiple cores through the doMC package. Unavailable on Windows."
useMulticore = TRUE



# Loading Packages, Setting Path & Random Number Generator Seed -----------
# load required libraries
library(here)
library(rstatix)
library(ez)
library(BayesFactor)
library(bayestestR)
if (useMulticore) {library(doMC)}

# set parent folder
parentFolder <- here()

# seed for random number generator (picked by random number generator on random.org)
rngSeed <- 249336527



# Loading Data ------------------------------------------------------------

# load data frame for ANOVA
loadname <- paste0(parentFolder,"/supplementaryData/dfSSVEP_commPL_withGender.csv")
df <- read.csv(loadname)
df$part <- factor(df$part)
df$lab <- factor(df$lab, levels = c("Florida","Leipzig"), labels = c("Florida","Leipzig"))
df$gender <- factor(df$gender, levels = c("Female","Male"), labels = c("Female","Male"))
df$freq <- factor(df$freq, levels = c("6Hz","8.57Hz","15Hz"), labels = c("6Hz","8.57Hz","15Hz"))
df$mod = factor(df$mod, levels = c("square","sine"), labels = c("square","sine"))



# Frequentist ANOVA on spectral amplitudes ----------------------------------

# frequentist Frequency x Modulation x Site ANOVA on z-standardized ssvep amplitudes
anovaSSVEP <- anova_test(
  data = df,
  dv = ssvepZ,
  wid = part,
  within = c(freq, mod),
  between = c(gender),
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
  between = .(gender),
  x = freq,
  split = mod,
  col = gender
); plotSSVEP



# Bayesian ANOVA on spectral amplitudes ------------------------------------

# generTestBF is used instead of anovaBF to model random slopes of within-subject effects
# this modeling approach is more in line with frequentist ANOVA
# anovaBF models did not account for stable interivididual differences when estimating effects
# which can lead to distorted Bayes Factors in designs with 2+ within-subject factors
# see van den Bergh et al. (2022, https://psyarxiv.com/fb8zn/)
set.seed(rngSeed); bayAnovaSSVEP <- generalTestBF(
  formula = ssvepZ ~ freq*mod*gender + part + part:freq + part:mod,
  data = df,
  whichRandom = c("part","part:freq","part:mod"),
  whichModels = "all",
  iterations = 10000,
  neverExclude = c("part","part:mod","part:freq"),
  multicore = useMulticore
); print(bayAnovaSSVEP)

# extract 5 best models: sort all Bayes Factors in decending order, and get indices
# of Factors in the Bayes ANOVA object; also return BF relative to null model
# and relative to best model
bfVec <- exp(bayAnovaSSVEP@bayesFactor[["bf"]])
bfToNull <- bfVec/bfVec[length(bfVec)]
bfSorted <- sort(bfToNull, decreasing = TRUE)
match(bfSorted[1:5], bfToNull)
bfSorted[1:5]
bfSorted[1:5] / bfSorted[1]

# compute Bayes Inclusion Factors
bfInc <- bf_inclusion(bayAnovaSSVEP, match_models = FALSE)
bfInc
bfInc$BF # to show exact values



# Frequentist ANOVA on SNR ----------------------------------

# frequentist Frequency x Modulation x Site ANOVA on z-standardized ssvep amplitudes
anovaSSVEP <- anova_test(
  data = df,
  dv = ssvepSNR_Z,
  wid = part,
  within = c(freq, mod),
  between = c(gender),
  type = 3,
  effect.size = "pes",
  detailed = TRUE
); anovaSSVEP
# quick plot of means and SEMs
plotSSVEP <- ezPlot(
  data = df,
  dv = ssvepSNR_Z,
  wid = part,
  within = .(freq, mod),
  between = .(gender),
  x = freq,
  split = mod,
  col = gender
); plotSSVEP



# Bayesian ANOVA on SNR ------------------------------------

# generTestBF is used instead of anovaBF to model random slopes of within-subject effects
# this modeling approach is more in line with frequentist ANOVA
# anovaBF models did not account for stable interivididual differences when estimating effects
# which can lead to biased Bayes Factors in designs with 2+ within-subject factors
# see van den Bergh et al. (2022, https://psyarxiv.com/fb8zn/)
set.seed(rngSeed); bayAnovaSSVEP <- generalTestBF(
  formula = ssvepSNR_Z ~ freq*mod*gender + part + part:freq + part:mod,
  data = df,
  whichRandom = c("part","part:freq","part:mod"),
  whichModels = "all",
  iterations = 10000,
  neverExclude = c("part","part:freq","part:mod"),
  multicore = useMulticore
); print(bayAnovaSSVEP)

# extract 5 best models: sort all Bayes Factors in decending order, and get indices
# of Factors in the Bayes ANOVA object; also return BF relative to null model
# and relative to best model
bfVec <- exp(bayAnovaSSVEP@bayesFactor[["bf"]])
bfToNull <- bfVec/bfVec[length(bfVec)]
bfSorted <- sort(bfToNull, decreasing = TRUE)
match(bfSorted[1:5], bfToNull)
bfSorted[1:5]
bfSorted[1:5] / bfSorted[1]

# compute Bayes Inclusion Factors
bfInc <- bf_inclusion(bayAnovaSSVEP, match_models = FALSE)
bfInc
bfInc$BF # to show exact values
