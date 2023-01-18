  # --- author: Christian Panitz
  # --- encoding: en_US.UTF-8
  # --- R version: 4.0.3 (2020-10-10) -- "Bunny-Wunnies Freak Out"
  # --- RStudio version: 2022.02.3
  # --- script version: December 2022
  # --- content: pairwise tests for ssVEP data (from common pipeline, singleTrial FFT, single-trial SNR)
  
  
  # Loading Packages, Setting Path & Random Number Generator Seed -----------
  
  # load required libraries
  library(here)
  library(tidyr)
  library(psych)
  library(rstatix)
  library(BayesFactor)
  
  # set parent folder
  parentFolder <- here()
  
  # seed for random number generator
  rngSeed <- 249336527
  
  
  
  # Loading Data & Creating Data Frames -------------------------------------
  
  # load data frame
  loadname <- paste0(parentFolder,"/dataframes/supp_dfSSVEP_commPL_singleTrialFFT_singleTrialSNR.csv")
  df <- read.csv(loadname)
  df$part <- factor(df$part)
  df$lab <- factor(df$lab, levels = c("Florida","Leipzig"), labels = c("Florida","Leipzig"))
  df$freq <- factor(df$freq, levels = c("6Hz","8.57Hz","15Hz"), labels = c("6Hz","8.57Hz","15Hz"))
  df$mod = factor(df$mod, levels = c("square","sine"), labels = c("square","sine"))
  
  # create dfs to test main effects
  # average across mod conditions to test freq effects
  dfFreqEffects <- pivot_wider(data = df, id_cols = c(part,lab,freq), names_from = c(mod), values_from = c(ssvepSNR_Z))
  dfFreqEffects$ssvepSNR_Z <- rowMeans(dfFreqEffects[,c("square","sine")])
  # average across freq conditions to test mod effects
  dfModEffects <- pivot_wider(data = df, id_cols = c(part,lab,mod), names_from = c(freq), values_from = c(ssvepSNR_Z))
  dfModEffects$ssvepSNR_Z <- rowMeans(dfModEffects[,c("6Hz","8.57Hz","15Hz")])
  
  
  
  # create dfs for descriptive statistics of difference values
  dfDiff = data.frame(
    lab = factor(c(rep("Florida",15),rep("Leipzig",15)), levels = c("Florida","Leipzig"),
                 labels = c("Florida","Leipzig")),
    diff_6_857 = dfFreqEffects$ssvepSNR_Z[dfFreqEffects$freq == "6Hz"] - 
      dfFreqEffects$ssvepSNR_Z[dfFreqEffects$freq == "8.57Hz"],
    diff_6_15 = dfFreqEffects$ssvepSNR_Z[dfFreqEffects$freq == "6Hz"] - 
      dfFreqEffects$ssvepSNR_Z[dfFreqEffects$freq == "15Hz"],
    diff_857_15 = dfFreqEffects$ssvepSNR_Z[dfFreqEffects$freq == "8.57Hz"] - 
      dfFreqEffects$ssvepSNR_Z[dfFreqEffects$freq == "15Hz"],
    diff_sq_sin = dfModEffects$ssvepSNR_Z[dfModEffects$mod == "square"] - 
      dfModEffects$ssvepSNR_Z[dfModEffects$mod == "sine"],
    diff_sq_sin_6Hz = df$ssvepSNR_Z[df$mod == "square" & df$freq == "6Hz"] - 
      df$ssvepSNR_Z[df$mod == "sine" & df$freq == "6Hz"],
    diff_sq_sin_857Hz = df$ssvepSNR_Z[df$mod == "square" & df$freq == "8.57Hz"] - 
      df$ssvepSNR_Z[df$mod == "sine" & df$freq == "8.57Hz"],
    diff_sq_sin_15Hz = df$ssvepSNR_Z[df$mod == "square" & df$freq == "15Hz"] - 
      df$ssvepSNR_Z[df$mod == "sine" & df$freq == "15Hz"]
  )
  
  
  
  # Descriptive Statistics for Mean Differences + 95% Confidence Intervals --------
  descStatsJoint <- describe(dfDiff)
  descStatsJoint$low.ci <- descStatsJoint$mean - qt(0.975,dim(dfDiff)[1]-1)*descStatsJoint$se
  descStatsJoint$upp.ci <- descStatsJoint$mean + qt(0.975,dim(dfDiff)[1]-1)*descStatsJoint$se
  descStatsJoint
  
  descStats <- describeBy(dfDiff, group = dfDiff$lab)
  descStats$Florida$low.ci <- descStats$Florida$mean - qt(0.975,sum(dfDiff$lab == "Florida")-1)*descStats$Florida$se
  descStats$Florida$upp.ci <- descStats$Florida$mean + qt(0.975,sum(dfDiff$lab == "Florida")-1)*descStats$Florida$se
  descStats$Leipzig$low.ci <- descStats$Leipzig$mean - qt(0.975,sum(dfDiff$lab == "Leipzig")-1)*descStats$Leipzig$se
  descStats$Leipzig$upp.ci <- descStats$Leipzig$mean + qt(0.975,sum(dfDiff$lab == "Leipzig")-1)*descStats$Leipzig$se
  descStats
  
  
  
  # Pairwise Comparisons across samples -------------------------------------
  # Frequentist t-tests, Cohen's d, and Bayesian t-tests
  
  # Main Effect Frequency Across Samples
  t_test(dfFreqEffects, ssvepSNR_Z ~ freq, paired = TRUE, p.adjust.method = "none")
  cohens_d(dfFreqEffects, ssvepSNR_Z ~ freq, paired = TRUE)
  ttestBF(x = dfFreqEffects$ssvepSNR_Z[dfFreqEffects$freq == "6Hz"],
          y = dfFreqEffects$ssvepSNR_Z[dfFreqEffects$freq == "8.57Hz"],
          nullInterval = NULL, paired = TRUE);
  ttestBF(x = dfFreqEffects$ssvepSNR_Z[dfFreqEffects$freq == "6Hz"],
          y = dfFreqEffects$ssvepSNR_Z[dfFreqEffects$freq == "15Hz"],
          nullInterval = NULL, paired = TRUE);
  ttestBF(x = dfFreqEffects$ssvepSNR_Z[dfFreqEffects$freq == "8.57Hz"],
          y = dfFreqEffects$ssvepSNR_Z[dfFreqEffects$freq == "15Hz"],
          nullInterval = NULL, paired = TRUE);
  
  # Main Effect Mod Across Samples
  t_test(dfModEffects, ssvepSNR_Z ~ mod, paired = TRUE, p.adjust.method = "none")
  cohens_d(dfModEffects, ssvepSNR_Z ~ mod, paired = TRUE)
  ttestBF(x = dfModEffects$ssvepSNR_Z[dfModEffects$mod == "square"],
          y = dfModEffects$ssvepSNR_Z[dfModEffects$mod == "sine"],
          nullInterval = NULL, paired = TRUE);
  
  # Mod effects for different frequencies (simple effects)
  t_test(group_by(df, freq),
         ssvepSNR_Z ~ mod, paired = TRUE, p.adjust.method = "none")
  cohens_d(group_by(df, freq),
           ssvepSNR_Z ~ mod, paired = TRUE)
  ttestBF(x = df$ssvepSNR_Z[df$freq == "6Hz" & df$mod == "square"],
          y = df$ssvepSNR_Z[df$freq == "6Hz" & df$mod == "sine"],
          nullInterval = NULL, paired = TRUE)
  ttestBF(x = df$ssvepSNR_Z[df$freq == "8.57Hz" & df$mod == "square"],
          y = df$ssvepSNR_Z[df$freq == "8.57Hz" & df$mod == "sine"],
          nullInterval = NULL, paired = TRUE)
  ttestBF(x = df$ssvepSNR_Z[df$freq == "15Hz" & df$mod == "square"],
          y = df$ssvepSNR_Z[df$freq == "15Hz" & df$mod == "sine"],
          nullInterval = NULL, paired = TRUE)
  
  
  
  # Pairwise Comparisons in Florida sample ----------------------------------
  # Frequentist t-tests, Cohen's d, and Bayesian t-tests
  
  # Main Effect Frequency Florida
  t_test(dfFreqEffects[dfFreqEffects$lab == "Florida",], ssvepSNR_Z ~ freq, paired = TRUE, p.adjust.method = "none")
  cohens_d(dfFreqEffects[dfFreqEffects$lab == "Florida",], ssvepSNR_Z ~ freq, paired = TRUE)
  ttestBF(x = dfFreqEffects$ssvepSNR_Z[dfFreqEffects$lab == "Florida" & dfFreqEffects$freq == "6Hz"],
          y = dfFreqEffects$ssvepSNR_Z[dfFreqEffects$lab == "Florida" & dfFreqEffects$freq == "8.57Hz"],
          nullInterval = NULL, paired = TRUE);
  ttestBF(x = dfFreqEffects$ssvepSNR_Z[dfFreqEffects$lab == "Florida" & dfFreqEffects$freq == "6Hz"],
          y = dfFreqEffects$ssvepSNR_Z[dfFreqEffects$lab == "Florida" & dfFreqEffects$freq == "15Hz"],
          nullInterval = NULL, paired = TRUE);
  ttestBF(x = dfFreqEffects$ssvepSNR_Z[dfFreqEffects$lab == "Florida" & dfFreqEffects$freq == "8.57Hz"],
          y = dfFreqEffects$ssvepSNR_Z[dfFreqEffects$lab == "Florida" & dfFreqEffects$freq == "15Hz"],
          nullInterval = NULL, paired = TRUE);
  
  # Main Effect Mod Florida
  t_test(dfModEffects[dfModEffects$lab == "Florida",], ssvepSNR_Z ~ mod, paired = TRUE, p.adjust.method = "none")
  cohens_d(dfModEffects[dfModEffects$lab == "Florida",], ssvepSNR_Z ~ mod, paired = TRUE)
  ttestBF(x = dfModEffects$ssvepSNR_Z[dfModEffects$lab == "Florida" & dfModEffects$mod == "square"],
          y = dfModEffects$ssvepSNR_Z[dfModEffects$lab == "Florida" & dfModEffects$mod == "sine"],
          nullInterval = NULL, paired = TRUE);
  
  # Mod effects for different frequencies (simple effects)
  t_test(group_by(df[df$lab == "Florida",], freq),
                  ssvepSNR_Z ~ mod, paired = TRUE, p.adjust.method = "none")
  cohens_d(group_by(df[df$lab == "Florida",], freq),
           ssvepSNR_Z ~ mod, paired = TRUE)
  ttestBF(x = df$ssvepSNR_Z[df$lab == "Florida" & df$freq == "6Hz" & df$mod == "square"],
          y = df$ssvepSNR_Z[df$lab == "Florida" & df$freq == "6Hz" & df$mod == "sine"],
          nullInterval = NULL, paired = TRUE)
  ttestBF(x = df$ssvepSNR_Z[df$lab == "Florida" & df$freq == "8.57Hz" & df$mod == "square"],
          y = df$ssvepSNR_Z[df$lab == "Florida" & df$freq == "8.57Hz" & df$mod == "sine"],
          nullInterval = NULL, paired = TRUE)
  ttestBF(x = df$ssvepSNR_Z[df$lab == "Florida" & df$freq == "15Hz" & df$mod == "square"],
          y = df$ssvepSNR_Z[df$lab == "Florida" & df$freq == "15Hz" & df$mod == "sine"],
          nullInterval = NULL, paired = TRUE)
  
  
  
  # Pairwise Comparisons in Leipzig sample ----------------------------------
  # Frequentist t-tests, Cohen's d, and Bayesian t-tests
  
  # Main Effect Frequency Leipzig
  t_test(dfFreqEffects[dfFreqEffects$lab == "Leipzig",], ssvepSNR_Z ~ freq, paired = TRUE, p.adjust.method = "none")
  cohens_d(dfFreqEffects[dfFreqEffects$lab == "Leipzig",], ssvepSNR_Z ~ freq, paired = TRUE)
  ttestBF(x = dfFreqEffects$ssvepSNR_Z[dfFreqEffects$lab == "Leipzig" & dfFreqEffects$freq == "6Hz"],
          y = dfFreqEffects$ssvepSNR_Z[dfFreqEffects$lab == "Leipzig" & dfFreqEffects$freq == "8.57Hz"],
          nullInterval = NULL, paired = TRUE);
  ttestBF(x = dfFreqEffects$ssvepSNR_Z[dfFreqEffects$lab == "Leipzig" & dfFreqEffects$freq == "6Hz"],
          y = dfFreqEffects$ssvepSNR_Z[dfFreqEffects$lab == "Leipzig" & dfFreqEffects$freq == "15Hz"],
          nullInterval = NULL, paired = TRUE);
  ttestBF(x = dfFreqEffects$ssvepSNR_Z[dfFreqEffects$lab == "Leipzig" & dfFreqEffects$freq == "8.57Hz"],
          y = dfFreqEffects$ssvepSNR_Z[dfFreqEffects$lab == "Leipzig" & dfFreqEffects$freq == "15Hz"],
          nullInterval = NULL, paired = TRUE);
  
  # Main Effect Mod Leipzig
  t_test(dfModEffects[dfModEffects$lab == "Leipzig",], ssvepSNR_Z ~ mod, paired = TRUE, p.adjust.method = "none")
  cohens_d(dfModEffects[dfModEffects$lab == "Leipzig",], ssvepSNR_Z ~ mod, paired = TRUE)
  ttestBF(x = dfModEffects$ssvepSNR_Z[dfModEffects$lab == "Leipzig" & dfModEffects$mod == "square"],
          y = dfModEffects$ssvepSNR_Z[dfModEffects$lab == "Leipzig" & dfModEffects$mod == "sine"],
          nullInterval = NULL, paired = TRUE);
  
  # Mod effects for different frequencies (simple effects)
  t_test(group_by(df[df$lab == "Leipzig",], freq),
                  ssvepSNR_Z ~ mod, paired = TRUE, p.adjust.method = "none")
  cohens_d(group_by(df[df$lab == "Leipzig",], freq),
           ssvepSNR_Z ~ mod, paired = TRUE)
  ttestBF(x = df$ssvepSNR_Z[df$lab == "Leipzig" & df$freq == "6Hz" & df$mod == "square"],
          y = df$ssvepSNR_Z[df$lab == "Leipzig" & df$freq == "6Hz" & df$mod == "sine"],
          nullInterval = NULL, paired = TRUE)
  ttestBF(x = df$ssvepSNR_Z[df$lab == "Leipzig" & df$freq == "8.57Hz" & df$mod == "square"],
          y = df$ssvepSNR_Z[df$lab == "Leipzig" & df$freq == "8.57Hz" & df$mod == "sine"],
          nullInterval = NULL, paired = TRUE)
  ttestBF(x = df$ssvepSNR_Z[df$lab == "Leipzig" & df$freq == "15Hz" & df$mod == "square"],
          y = df$ssvepSNR_Z[df$lab == "Leipzig" & df$freq == "15Hz" & df$mod == "sine"],
          nullInterval = NULL, paired = TRUE)
