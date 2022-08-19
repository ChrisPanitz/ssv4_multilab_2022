### readme scripts
R scripts for data reshaping, statistical analyses, and plotting


00_installPackages.R
- installs all required R packages but only if not already installed (will not overwrite existing packages with different version)

01_createDataframes_diffPL.R
- reads spectral data from "individualSpectra" folder for independent pipelines
- creates dataframes for statistical analyses and plotting data, saves them in "dataframes" folder

02_plotSpectra_diffPL_withTopos.R
- plots spectra and topographies for all conditions & labs for independent pipeline analysis

03_anovaSSVEP_diffPL.R
- computes frequentist & Bayesian ANOVAs (Laboratory x Driving Frequency x Modulation function) with data from independent pipelines
- computes inclusion Bayes factors for ANOVA factors

04_pairwiseTestsSSVEP_diffPL.R
- computes t-tests and Cohen's d for pairwise comparisons of within-subject factors (independent pipelines)

05_plotGroupStats_diffPL
- plots mean ssVEP amplitudes for each participant and group means; for each condition (independent pipelines)

11_createDataframes_commPL.R
- reads spectral data from "individualSpectra" folder for common pipeline
- creates dataframes for statistical analyses and plotting data, saves them in "dataframes" folder

12_plotSpectra_commPL_withTopos.R
- plots spectra and topographies for all conditions & labs for common pipeline analysis

13_anovaSSVEP_commPL.R
- computes frequentist & Bayesian ANOVAs (Laboratory x Driving Frequency x Modulation function) with data from common pipeline
- computes inclusion Bayes factors for ANOVA factors

14_pairwiseTestsSSVEP_commPL.R
- computes t-tests and Cohen's d for pairwise comparisons of within-subject factors (common pipeline)

15_plotGroupStats_diffPL
- plots mean ssVEP amplitudes for each participant and group means for each condition & lab (common pipeline)

16_plotJointSpectraAndGroupStats_commPL.R
- plots spectra for all conditions aggregated across labs for common pipeline analysis
- plots mean ssVEP amplitudes for each participant and group means for each condition aggregated across labs (common pipeline)

17_plotTopographies_commPL.R
- plots topographies of mean ssVEP amplitudes for all conditions & labs as well as difference topographies [square-sine] for common pipeline analysis
