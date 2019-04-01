# chum-et-al
Mission downstream juveniles - sockeye
key data: 2017 environmental, tally, biosample. focus on sockeye

KEY SCRIPTS:
> 2017data_cleaning.R: cleaning up excel sheets, renaming columns, exporting into more manageable format and .csv
> 2017data_explr_TAB.R: subsetting the env, tally and bio data. merging databases based on relational keys. preliminary data exploration and subsetting
> 2017CPUE_calibrations.R: exploratory calibration of catch by discharge, bay flow, vertical flow, etc. 
> 2017CPUE_calibrations_AppendixI.R & 2017CPUE_calibrations_AppendixII.R: code that was used to generate figures/analysis included in the appendices of the 'living word doc' 
> 2017_sampling_subset_CU.R: side analysis to see which stocks were lost/how proportions changed if 2017 sampling had occurred every 2nd day
> catch_table_matrix.R: creating the expanded catch table by 1-min intervals as in Walters (2003)
> glmm.R: preliminary look into using GLMM to predict missing CPUE values from the catch_table_matrix
> interpolation.R: using imputeTS and other methods to impute missing CPUE and current velocity values 
> mission-chilko_travel_time.R: side analysis looking at the travel time between chilko fence and mission


IGNORE: 
> appendix_fish_density.R: old analysis, not sure what is even in here
> cloning_DFO.R: couldn't get github to clone and work on DFO computer so this was what worked to 'fix' it
> decay_curve.R: playing around trying to fit a decay curve to vertical catch 
> quadratic_plateau_model.R: playing around to see how this worked with cumulative run curves


k. davidson
