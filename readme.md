

# About

This folder contains scripts to produce pallid sturgeon
reference populations used in subsequent analyses of potential monitoring
programs.

## Files and structure

Files and folder within this directory.

* index.Rmd
* src - folder containing R scripts for analysis
    * 1_global.R
    * 2_functions.R
    * 3_load-and-clean.R
    * 4_figures.R
    * 5_tables.R - a function that returns publication and report table
    * 6_analysis.R - a file where analyses are completed
* dat-contains any input files required for analysis
* output-contains any output files generated during analysis

# Organization and workflow

Index.Rmd runs the show. It sources child documents that correspond to 
major sections. The the other Rmd files in this folder source child 
documents that link to outputs from the anlayses folder. 


I terms of work flow, compile index.Rmd to get the whole document. 
Compile each section to get a section view. 



		
