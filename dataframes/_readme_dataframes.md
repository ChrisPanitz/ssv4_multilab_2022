### readme dataframes
csv files with data in long format
separator: ,
string indicator: ""


## dfSpectra*.csv dataframes contain spectra averaged across participants for plotting
dfSpectra_diffPL.csv: for samples analyzed with independent pipelines
dfSpectra_commPL.csv: for samples analyzed with common pipeline
dfSpectra_commPL_jointLabs.csv: aggregated samples analyzed with common pipeline
supp_dfSpectra_commPL_singleTrialFFT.csv: for samples analyzed with FFT on single-trial pipeline
supp_dfSpectra_commPL_singleTrialFFT_jointLabs.csv: aggregated samples analyzed with FFT on single-trial pipeline

# Variables
lab
- content: lab where data was collected
- type: between-subject factor
- levels: Florida, Leipzig
- note: not included in lab-aggregated data (dfSpectra_commPL_jointSites.csv)

mod
- content: shape of modulation function
- type: within-subject factor
- levels: square, sine

freqBin
- content: frequency band (x axis of spectrum)
- type: continuous

amp
- content: spectral amplitude
- type: continuous



## dfSSVEP*.csv dataframes with mean ssVEP amplitudes for all participants
dfSSVEP_diffPL.csv: for samples analyzed with independent pipelines
dfSSVEP_commPL.csv: for samples analyzed with common pipeline
supp_dfSSVEP_commPL_singleTrialFFT.csv: for samples analyzed with independent pipelines, contains both amplitudes and SNR from ERP
supp_dfSSVEP_commPL_singleTrialFFT_singleTrialSNR.csv: for samples analyzed with common pipeline, contains SNR from single-trial data

# Variables
part
- participant identifier
- "FL" for Florida/"LE" for Leipzig + running index 

lab
- content: lab where data was collected
- type: between-subject factor
- levels: Florida, Leipzig

freq
- content: driving frequency
- type: within-subject factor
- levels: 6Hz, 8.57Hz, 15Hz

mod
- content: shape of modulation function
- type: within-subject factor
- levels: square, sine

ssvep
- content: spectral amplitude at driving frequency
- type: continuous
- note: for independent pipelines: Leipzig = amplitude at driving frequency +/- 0.1 Hz

ssvepZ
- content: z-standardized spectral amplitude at driving frequency (across conditions, separate for labs)
- type: continuous
- note: for independent pipelines: Leipzig = amplitude at driving frequency +/- 0.1 Hz

ssvepSNR
- content: SNR at driving frequency
- type: continuous

ssvepSNR_Z
- content: z-standardized SNR at driving frequency (across conditions, separate for labs)
- type: continuous



## dfTopos*.csv dataframes with topographies for each lab & condition (sample means)
dfTopos_diffPL.csv: for samples analyzed with independent pipelines
dfTopos_commPL.csv: for samples analyzed with common pipeline
supp_dfTopos_commPL_singleTrialFFT.csv: for samples analyzed with FFT on single-trial pipeline, contains amplitudes and SNR from ERP
supp_dfTopos_commPL_singleTrialFFT_singleTrialSNR.csv: for samples analyzed with FFT on single-trial pipeline, contains SNR from single-trial data
supp_PLI_dfTopos.csv: phase-locking topographies (FFT on single-trial pipeline)

# Variables
lab
- content: lab where data was collected
- type: between-subject factor
- levels: Florida, Leipzig

freq
- content: driving frequency
- type: within-subject factor
- levels: 6Hz, 8.57Hz, 15Hz

mod
- content: shape of modulation function
- type: within-subject factor
- levels: square, sine

electrode
- content: electrode labels

x
- electrode x coordinates in 2D representation for plotting using eegUtils package
- type: continuous

y
- electrode y coordinates in 2D representation for plotting using eegUtils package
- type: continuous

amplitude
- content: mean ssVEP amplitude cross participants
- type: continuous
- note: for independent pipelines: Leipzig = amplitude at driving frequency +/- 0.1 Hz

amplitudeSNR
- content: mean ssVEP SNR cross participants
- type: continuous



## supp_dfPLI: dataframe with mean phase-locking indices for all participants

# Variables
part
- participant identifier
- "FL" for Florida/"LE" for Leipzig + running index 

lab
- content: lab where data was collected
- type: between-subject factor
- levels: Florida, Leipzig

freq
- content: driving frequency
- type: within-subject factor
- levels: 6Hz, 8.57Hz, 15Hz

mod
- content: shape of modulation function
- type: within-subject factor
- levels: square, sine

pli
- content: phase-locking index at driving frequency
- type: continuous

pliZ
- content: z-standardized phase-locking index at driving frequency (separate standardization for labs)
- type: continuous