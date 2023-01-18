## dfSSVEP_commPL_withGender
contains data for control ANOVA with factor Gender

# Variables
part
- participant identifier
- "FL" for Florida/"LE" for Leipzig + running index 

lab
- content: lab where data was collected
- type: between-subject factor
- levels: Florida, Leipzig

gender
- content: gender
- type: within-subject factor
- levels: female, male

freq
- content: driving frequency
- type: within-subject factor
- levels: 6Hz, 8.57Hz, 15Hz

mod
- content: shape of modulation function
- type: within-subject factor
- levels: square, sine

ssvepZ
- content: z-standardized ssVEP amplitude at driving frequency (separate standardization for labs)
- type: continuous

ssvepSNR_Z
- content: z-standardized ssVEP SNR at driving frequency (separate standardization for labs)
- type: continuous


##
hilbert/
contains Hilbert transform of individual ERPs

each file name contains
- the laboratory where data was collected (FL = Florida; LE = Leipzig)
- a running number within the lab sample (01 to 15)
- the driving frequency (6 vs 8.57 vs 15 Hz)
- the modulation function (square vs sine)

data structure in file:
rows = channels
columns = data point in time domain

separator: ','



##
phaseStability/
contains PLI values for each participant and condition

each file name contains
- the laboratory where data was collected (FL = Florida; LE = Leipzig)
- a running number within the lab sample (01 to 15)
- the driving frequency (6 vs 8.57 vs 15 Hz)
- the modulation function (square vs sine)

data structure in file:
rows = channels
columns = frequency bin

separator: ','



##
singleTrialCourse/
contains single-trial amplitudes and SNR (only data for respective driving frequency)

each file name contains
- the laboratory where data was collected (FL = Florida; LE = Leipzig)
- a running number within the lab sample (01 to 15)
- the driving frequency (6 vs 8.57 vs 15 Hz)
- the modulation function (square vs sine)
- the measure (amp vs SNR)

data structure in file:
rows = channels
columns = trials

separator: ','
trials with bad data: NA