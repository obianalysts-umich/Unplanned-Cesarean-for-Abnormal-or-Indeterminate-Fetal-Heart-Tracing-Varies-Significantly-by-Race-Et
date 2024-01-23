# Unplanned Cesarean for non-reassuring fetal status by race-ethnicity manuscript

## Background

This is the repository housing all analytics for the manuscript currently titled "Association Between Race-Ethnicity and Unplanned Cesarean for Non-Reassuring Fetal Status." Authors are Elizabeth Langen, Althea Bourdeau, Jessi Ems, and Lisa Kane Low.

There is little history for this repository as the code was copied from a private parent repo in order to be made publicly available.

## Data

This code uses data clinically abstracted from the electronic health record by trained clinical data abstractors at 68 hospitals participating in the [Obstetrics Initiative](https://www.obstetricsinitiative.org/), a collaborative quality improvement project funded by Blue Cross Blue Shield of Michigan and Blue Care Network.

These data are not publicly available.

## Code

The code in this repo is organized in numbered files that should be run in order. Code files do the following:

* [00.data_processing](/code/00.data_processing.R) - loads and cleans OBI data, creates derived variables
* [01.fig_1](/code/01.fig_1.R) - Creates figure 1 (flowchart of study cohort)
* [02.table_1](/code/02.table_1.R) - Creates table 1 (descriptive characteristics of study population)
* [03.table_2](/code/03.table_2.R) - Creates table 2 (characteristics of study population by race-ethnicity)
* [04.chisq](/code/04.chisq.R) - Runs ChiSquare tests for sensitivity analysis: 
  * Prosperity region & Cesarean for FHT 
  * Prosperity region & race-ethnicity 
  * Race-ethnicity & Cesarean for FHT stratified by prosperity region
* [05.regression](/code/05.regression.R) - Runs regression models
* [06.sensitivity](/code/06.sensitivity.R) - Runs sensitivity analyses:
  * Race-ethnicity and risk of unplanned Cesarean for any indication
  * Race-ethnicity and risk of unplanned Cesarean for FHT within Detroit Metro region
  * Race-ethnicity and risk of unplanned Cesarean for FHT, limiting entire cohort to unplanned Cesareans
* [07.figures](/code/07.figures.R) - create figures 2 (bar graph of NTSV Cesareans for FHT) and 3 (OR plot)
* [x.master](/code/x.master.R) - Runs all numbered code files

## Output

Output from this code (figures and tables) is saved to a private Google Drive.
