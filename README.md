# Ross Group EDIB Inclusion Index
Analysis code for processing submissions to the Ross Group EDIB Inclusion Index.

### Contact
Dr Arish Mudra Rakshasa-Loots ([email](mailto:arish.mrl@ed.ac.uk))

## Background
At its June 2024 meeting in London, the Ross EDIB Working Group defined a set of priorities to further the integration of equity, diversity, inclusion, and belonging (EDIB) in Ross Group member institutions. One of the key areas of need identified during this meeting was the lack of any formal benchmarking tools for EDIB progress within UK Advancement. The Advancement Inclusion Index established by CASE, while comprehensive, has been designed for global applicability, and thus may not be entirely fit-for-purpose for UK institutions. In response to this priority, the Ross EDIB Inclusion Index was developed as a benchmarking tool specific to the UK and Ireland for measuring EDIB progress within Advancement.

Round 1 of the Index was implemented in 2025, and Round 2 in 2026. A key change from Round 1 was that the Index is now completed electronically via Online Surveys (JISC). This ensures that responses are recorded consistently and more granular data analysis of trends is possible. The analysis code included in this repository can be used to process submissions to the Inclusion Index and generate results efficiently.

## Instructions for Use
All code should be run in R. Within the working directory, the following folder structure should be set up to run the code:
1. Data (subfolders: years, e.g. 2025, 2026)
2. Results (subfolders: years, e.g. 2025, 2026)
3. Figures (subfolder: Anonymised)
Run the `00-packages.R` script first to install and load the required packages.

For 2025, submissions were not electronic, so a cleaned dataset was compiled manually. The `2025-results.R` script analyses this data and produces visualisations of the results.

For 2026 and future iterations, analysis code cleans and processes the raw (uncoded) CSV output from Online Surveys.  
Run scripts in the following order:
1. `2026-1-datacleaning.R` to import the dataset, clean coding, calculate total and theme-wise scores, and export a cleaned summary of results. *This script will need to be updated each year* for future iterations if any changes are made to the Inclusion Index items, and requires manual input of the dataframe and recoding of names for the participating institutions.
2. `2026-2-results.R` to use the cleaned summary of results to visualise scores across institution, stratified by theme, and change from previous year.
3. `2026-3-sens-partialyes.R` to carry out a sensitivity analysis, exploring changes in scores if 'partial yes' responses are coded as `1` instead of `0`.
4. `2026-4-anonymised.R` to produce anonymised visualisations of the results for each participating institution.


## Index Development Code
Analysis code used during the development process of the Inclusion Index has also been archived in this repository. This code should not be used for analysis of results from the Index.

