Replication Data and Code for "Hate Crimes and Gender Imbalances: Fears over Mate Competition and Violence against Refugees"
American Journal of Political Science
Rafaela Dancygier, Naoki Egami, Amaney Jamal, Ramona Rischke

Operating system used for this analysis. 
- macOS version 10.15.6.  
- MacBook Pro. 
- Processor: 2.4 GHz 8-core Intel Core i9.
- Memory: 32 GB 2667 MHz DDR4

The analyses were carried out using R version 4.0.2. 

The following R packages were used:
readstata13  #  version 0.9.2
MASS # version 7.3-51.6  
sandwich  # version 2.5-1 
lmtest # version 0.9-37
stargazer # version 5.2.2
list # version 9.2
pBrackets # version 1.0

# ##################
# Main Analysis
# ##################

Please run "master.R."  Then, every table and figure will be produced.
To reproduce all numbers we computed from our datasets, please run "number_in_texts.R".
"Help.R" contains several custom functions we use for analyses. 

Below, we explain individual codes called in "master.R" But, to replicate results, analysts just need to run "master.R" 


# ##############################
# Reproduce Analysis Data Sets
# ##############################
"context.dta" and "context_placebo.dta" are produced by merging source data. All source data are in folder "source_data". 
To reproduce the two analysis data sets, please follow the following steps. 
(1) Run "produce_context_data.do" in STATA (version 16) to produce intermediate datasets "merged_context_1.dta" and "merged_context_2.dta"
(2) Run "merge_context.R" in R to produce "context.dta"
(3) Run "merge_context_placebo.R" in R to produce "context_placebo.dta"


# #####################################
# Files and Data in Replication Files
# #####################################

Files and Code to reproduce results on context analysis in the paper.
      code: ContextAnalysis_Main.R
      data: context.dta
      results: Figure 1 and Figure C8

Files and Code to reproduce results on survey analysis in the paper.
      code: SurveyAnalysis_Main.R
      data: survey.dta
      results: Figure 2, Figure 3, Table 1, Figure 4

Files and Code to reproduce results on context analysis in the Appendix.
      code: ContextAnalysis_Appendix.R
      data: context.dta and context_placebo.dta
      results: Table C1, C2, C3, C4, C5, C6, C7, C9, and Figure C10
      Note: we stored bootstrap results as "out_count_table.rdata", which is used to produce Table C5, because it is computationally expensive.        The exact code to reproduce this rdata is given in line 400-417 of "ContextAnalysis_Appendix.R". 

Files and Code to reproduce results on survey analysis in the Appendix.
      code: SurveyAnalysis_Appendix.R
      data: survey.dta and YouGov.dta
      results: Figure D2, D3, D4_1, D4_2, Table D.5.1, D.5.2, D.6.1,
      Figure D.6.2, Table D.6.3, D.6.4, D.8.1, D.8.2

# ##########################
# Description of all files
# ##########################
R code
	- ContextAnalysis_Main.R: R code for the main context analysis
	- ContextAnalysis_Appendix.R: R code for the context analysis in Appendix 
	- Help.R: R code that contains several custom functions we use for analyses
	- master.R: the master R code, which runs all the analysis R codes
	- number_in_texts.R: R code to reproduce all numbers we computed from our datasets
	- SurveyAnalysis_Main.R: R code for the main survey analysis
	- SurveyAnalysis_Appendix.R: R code for the survey analysis in Appendix 
	
	
Data
	- context.dta: the main analysis data for context analysis
	- context_placebo.dta: the data used for a placebo test in context analysis
	- out_count_table.rdata: Bootstrap results used to produce Table C5, because it is computationally expensive. 
	- survey.dta: the main analysis data for survey analysis 
	- YouGov.dta: the YouGov data we used in the survey analysis in the Appendix 
	

Folder "source_data" 
	This folder contains contains all source data necessary to produce "context.dta" and "context_placebo.dta" 
	- area_mun.dta: Source data on area size
	- base_pl.dta: The base data that contains the year and AGS information for "context_placebo.dta"
	- base.dta: The base data that contains the year and AGS information for "context.dta"
	- crime.dta: Source data on general crime
	- education.dta: Source data on education
	- hate.dta: Source data on hate crime 
	- merged_context_1.dta: Intermediate data we created using "produce_context_data.do"
	- merged_context_2.dta: Intermediate data we created using "produce_context_data.do"
	- pop_gemeinde_2008_2018.dta: Source data on population at the municipality level
	- pop_kreise_2015_2017.dta: Source data on population at the district level
	- population.dta: Source data on population
	- refugee_gender.dta: Source data on refugees by gender 
	- refugees_2008_2017.dta: Source data on refugees
	- sectors.dta: Source data on sectors
	- unempl_gemeinde_2008_2017.dta: Source data on unemployment at the municipality level
	- unemployment.dta: Source data on unemployment 
	- voting.dta: Source data on voting 

Codebooks
	- codebook_context.pdf: Codebook for "context.dta"
	- codebook_context_placebo.pdf: Codebook for "context_placebo.dta"
	- codebook_survey.pdf: Codebook for "survey.dta"
	- codebook_YouGov.pdf: Codebook for "YouGov.dta"

Additional Files related Data Sources
	- source_context.pdf: Additional details on data sources for "context.dta"
	- source_context_placebo.pdf: Additional details on data sources for "context_placebo.dta"
	- merge_context.R: R file used to merge original data sources to create "context.dta" 
	(We provide this code for the exact documentation of our merging processes. Each original data source in this file was documented in "source_context.pdf" and stored in folder "source_data".)
	- merge_context_placebo.R: R file used to merge original data sources to create "context_placebo.dta" 
	(We provide this code for the exact documentation of our merging processes. Each original data source in this file was documented in "source_context_placebo.pdf" and stored in folder "source_data".)
        - produce_context_data.do: STATA do file used to merge original data sources to create intermediate datasets used in "merge_context.R" and "merge_context_placebo.R"
	(We provide this code for the exact documentation of our merging processes. Each original data source in this file was documented in 
         "source_context.pdf" and "source_context_placebo.pdf" and stored in folder "source_data".)

Outputs 
	- table1.tex: TeX file for Table 1
	- table_C1.tex: TeX file for Table C1
	- table_C2.tex: TeX file for Table C2
	- table_C3.tex: TeX file for Table C3
	- table_C4.tex: TeX file for Table C4	
	- table_C5.tex: TeX file for Table C5	
	- table_C6.tex: TeX file for Table C6
	- table_C7.tex: TeX file for Table C7
	- table_C9.tex: TeX file for Table C9
	- table_D5_1.tex: TeX file for Table D5.1
	- table_D5_2.tex: TeX file for Table D5.2
	- table_D6_1.tex: TeX file for Table D6.1
	- table_D6_3.tex: TeX file for Table D6.3
	- table_D8_1.tex: TeX file for Table D8.1
	- table_D8_2.tex: TeX file for Table D8.2
