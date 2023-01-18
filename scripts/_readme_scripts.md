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

12b_plotSpectra_commPL_withSNRTopos.R
- plots spectra and SNR topographies for all conditions & labs for common pipeline analysis

13_anovaSSVEP_commPL.R
- computes frequentist & Bayesian ANOVAs (Laboratory x Driving Frequency x Modulation function) with data from common pipeline
- computes inclusion Bayes factors for ANOVA factors
- DV: spectral amplitude

13b_anovaSSVEP_commPL_SNR.R
- computes frequentist & Bayesian ANOVAs (Laboratory x Driving Frequency x Modulation function) with data from common pipeline
- computes inclusion Bayes factors for ANOVA factors
- DV: SNR

14_pairwiseTestsSSVEP_commPL.R
- computes t-tests and Cohen's d for pairwise comparisons of within-subject factors (common pipeline)
- DV: spectral amplitude

14b_pairwiseTestsSSVEP_commPL_SNR.R
- computes t-tests and Cohen's d for pairwise comparisons of within-subject factors (common pipeline)
- DV: SNR

15_plotGroupStats_commPL.R
- plots mean ssVEP amplitudes for each participant and group means for each condition & lab (common pipeline)
- DV: spectral amplitude

15b_plotGroupStats_commPL_SNR.R
- plots mean ssVEP SNRs for each participant and group means for each condition & lab (common pipeline)
- DV: SNR

16_plotJointSpectraAndGroupStats_commPL_AmpAndSNR.R
- plots spectra for all conditions aggregated across labs for common pipeline analysis
- plots mean ssVEP spectral amplitudes for each participant and group means for each condition aggregated across labs (common pipeline)
- plots mean ssVEP SNRs for each participant and group means for each condition aggregated across labs (common pipeline)

17_plotTopographies_commPL.R
- plots topographies of mean ssVEP amplitudes for all conditions & labs as well as difference topographies [square-sine] for common pipeline analysis

17b_plotTopographies_commPL_SNR.R
- plots topographies of mean ssVEP SNRs for all conditions & labs as well as difference topographies [square-sine] for common pipeline analysis

18_supp_plotHilbert_commPL_SNR.R
- plots time courses of ssVEP amplitude obtained via  Hilbert transforms of ERPs from common pipeline analysis for all conditions & labs 

19_supp_anovaSSVEP_commPL_withGender.R
- computes frequentist & Bayesian ANOVAs (Gender x Driving Frequency x Modulation function) with data from common pipeline
- computes inclusion Bayes factors for ANOVA factors
- DVs: spectral amplitude and SNR

21_supp_createDataframes_commPL_singleTrialFFT.R
- reads spectral data from "individualSpectra" folder for common pipeline with FFT on single-trial level
- creates dataframes for statistical analyses and plotting data, saves them in "dataframes" folder

21c_supp_createDataframes_commPL_singleTrialFFT_timeCourse.R
- reads single-trial data from "supplementaryData/singleTrialCourse" folder
- creates dataframes for statistical analyses and plotting data for the "FFT & SNR on single-trial level" pipeline, saves them in "dataframes" folder
- creates dataframe for plotting single-trial time courses (amplitude & SNR)

22_supp_plotSpectra_commPL_withTopos.R
- plots spectra and topographies for all conditions & labs for FFT on single-trial pipeline analysis

22b_supp_plotSpectra_commPL_withSNRTopos.R
- plots spectra and SNR topographies for all conditions & labs for FFT on single-trial pipeline analysis

23_supp_anovaSSVEP_commPL.R
- computes frequentist & Bayesian ANOVAs (Laboratory x Driving Frequency x Modulation function) with data from commonFFT on single-trial pipeline
- computes inclusion Bayes factors for ANOVA factors
- DV: spectral amplitude

23b_supp_anovaSSVEP_commPL_avgSNR.R
- computes frequentist & Bayesian ANOVAs (Laboratory x Driving Frequency x Modulation function) with data from FFT on single-trial pipeline
- computes inclusion Bayes factors for ANOVA factors
- DV: SNR from ERP

23c_supp_anovaSSVEP_commPL_singleTrialSNR.R
- computes frequentist & Bayesian ANOVAs (Laboratory x Driving Frequency x Modulation function) with data from FFT on single-trial pipeline
- computes inclusion Bayes factors for ANOVA factors
- DV: SNR from single-trial data

24_supp_pairwiseTestsSSVEP_commPL_singleTrialFFT.R
- computes t-tests and Cohen's d for pairwise comparisons of within-subject factors (FFT on single-trial pipeline)
- DV: spectral amplitude

24b_supp_pairwiseTestsSSVEP_commPL_singleTrialFFT_avgSNR.R
- computes t-tests and Cohen's d for pairwise comparisons of within-subject factors (FFT on single-trial pipeline)
- DV: SNR from ERP

24c_supp_pairwiseTestsSSVEP_commPL_singleTrialFFT_singleTrialSNR.R
- computes t-tests and Cohen's d for pairwise comparisons of within-subject factors (FFT on single-trial pipeline)
- DV: SNR from single-trial data

25_supp_plotGroupStats_commPL_singleTrialFFT.R
- plots mean ssVEP amplitudes for each participant and group means for each condition & lab (FFT on single-trial pipeline)
- DV: spectral amplitude

25b_supp_plotGroupStats_commPL_singleTrialFFT_avgSNR.R
- plots mean ssVEP SNRs for each participant and group means for each condition & lab (FFT on single-trial pipeline)
- DV: SNR from ERP

25c_supp_plotGroupStats_commPL_singleTrialFFT_singleTrialSNR.R
- plots mean ssVEP SNRs for each participant and group means for each condition & lab (FFT on single-trial pipeline)
- DV: SNR from single-trial data

26_supp_plotJointSpectraAndGroupStats_commPL_singleTrial_AmpAndAvgSNR.R
- plots spectra for all conditions aggregated across labs for FFT on single-trial pipeline analysis
- plots mean ssVEP spectral amplitudes for each participant and group means for each condition aggregated across labs (FFT on single-trial pipeline)
- plots mean ssVEP SNRs (from ERP) for each participant and group means for each condition aggregated across labs (FFT on single-trial pipeline)

26c_supp_plotJointGroupStats_commPL_singleTrial_singleTrialSNR.R
- plots mean ssVEP SNRs (from single-trial data) for each participant and group means for each condition aggregated across labs (FFT on single-trial pipeline)

27_supp_plotTopographies_commPL_singleTrialFFT.R
- plots topographies of mean ssVEP amplitudes for all conditions & labs as well as difference topographies [square-sine] for FFT on single-trial pipeline analysis

27b_supp_plotTopographies_commPL_singleTrialFFT_avgSNR.R
- plots topographies of mean ssVEP SNRs (from ERP) for all conditions & labs as well as difference topographies [square-sine] for FFT on single-trial pipeline analysis

27c_supp_plotTopographies_commPL_singleTrialFFT_singleTrialSNR.R
- plots topographies of mean ssVEP SNRs (from single-trial data) for all conditions & labs as well as difference topographies [square-sine] for FFT on single-trial pipeline analysis

28_supp_plotAmpAcrossTrials.R
- plots time courses of single-trial ssVEP amplitudes across trials (FFT on single-trial pipeline) for all conditions & labs

28b_supp_plotSNRAcrossTrials.R
- plots time courses of single-trial ssVEP SNRs across trials (FFT on single-trial pipeline) for all conditions & labs

31_supp_createDataframes_PLI.R
- reads phase-locking data from "supplementaryData/phaseStability" folder (common pipeline)
- creates dataframes for statistical analyses and plotting data, saves them in "dataframes" folder

32_supp_plotTopos_PLI.R
- plots phase-locking topographies for all conditions & labs (common pipeline)

33_supp_anovaSSVEP_PLI.R
- computes frequentist & Bayesian ANOVAs (Laboratory x Driving Frequency x Modulation function) with phase-locking data from common pipeline
- computes inclusion Bayes factors for ANOVA factors
- DV: PLI

34_supp_pairwiseTestsSSVEP_commPL.R
- computes t-tests and Cohen's d for pairwise comparisons of within-subject factors (common pipeline)
- DV: PLI

35_supp_plotGroupStats_commPL.R
- plots mean ssVEP amplitudes for each participant and group means for each condition & lab (common pipeline)
- DV: PLI

36_supp_plotJointGroupStats_PLI.R
- plots mean PLIs for each participant and group means for each condition aggregated across labs (common pipeline)