# --- author: Christian Panitz
# --- encoding: en_US.UTF-8
# --- R version: 4.0.3 (2020-10-10) -- "Bunny-Wunnies Freak Out"
# --- RStudio version: 2022.02.3
# --- script version: December 2022
# --- content: ANOVAs for ssVEP data (common pipeline, single-trial FFT, SNR from average)



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
loadname <- paste0(parentFolder,"/dataframes/supp_dfSSVEP_commPL_singleTrialFFT.csv")
df <- read.csv(loadname)
df$part <- factor(df$part)
df$lab <- factor(df$lab, levels = c("Florida","Leipzig"), labels = c("Florida","Leipzig"))
df$freq <- factor(df$freq, levels = c("6Hz","8.57Hz","15Hz"), labels = c("6Hz","8.57Hz","15Hz"))
df$mod = factor(df$mod, levels = c("square","sine"), labels = c("square","sine"))



# Frequentist ANOVA -------------------------------------------------------

# frequentist Frequency x Modulation x Site ANOVA on z-standardized ssvep amplitudes
anovaSSVEP <- anova_test(
  data = df,
  dv = ssvepSNR_Z,
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
  dv = ssvepSNR_Z,
  wid = part,
  within = .(freq, mod),
  between = .(lab),
  x = freq,
  split = mod,
  col = lab
); plotSSVEP



# Bayesian ANOVA ----------------------------------------------------------

# generTestBF is used instead of anovaBF to model random slopes of within-subject effects
# this modeling approach is more in line with frequentist ANOVA
# anovaBF models did not account for stable interivididual differences when estimating effects
# which can lead to biased Bayes Factors in designs with 2+ within-subject factors
# see van den Bergh et al. (2022, https://psyarxiv.com/fb8zn/)
set.seed(rngSeed); bayAnovaSSVEP <- generalTestBF(
  formula = ssvepSNR_Z ~ freq*mod*lab + part + part:freq + part:mod,
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


# use of anovaBF not recommended as of December 2022
# # Bayesian Frequency x Modulation x lab ANOVA on z-standardized ssvep amplitudes
# set.seed(rngSeed); bayAnovaSSVEP <- anovaBF(
#                                     formula = ssvepSNR_Z ~ freq*mod*lab + part,
#                                     data = df,
#                                     whichRandom = "part",
#                                     whichModels = "all",
#                                     iterations = 10000
# ); print(bayAnovaSSVEP)
# 
# # extract 5 best models: sort all Bayes Factors in decending order, and get indices
# # of Factors in the Bayes ANOVA object; also return BF relative to null model
# # and relative to best model
# bfSorted <- sort(exp(bayAnovaSSVEP@bayesFactor[["bf"]]), decreasing = TRUE)
# match(bfSorted[1:5], exp(bayAnovaSSVEP@bayesFactor[["bf"]]))
# bfSorted[1:5]
# bfSorted[1:5] / bfSorted[1]
# 
# # compute Bayes Inclusion Factors
# bf_inclusion(bayAnovaSSVEP, match_models = FALSE)