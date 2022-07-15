# lapop-viz
 These are the templates for creating LAPOP's visualizations of AmericasBarometer data.

The workflow for visualization projects is as follows:

1) Create project folder
2) Create or save .do file into project folder.  Run analyses in this .do file using LAPOP .ado files, with output as .csvs saved to your project folder (see help file on .ados)
3) Load lapop package in R
4) Type lapop_fonts() into console
5) Follow instructions within R help files to load in .csvs and create visualizations in R using pre-defined functions.  Use lapop_save() to save .svg files to project folder. 

<!-- badges: start -->
[![R-CMD-check](https://github.com/lapop-central/lapop-viz/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/lapop-central/lapop-viz/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->
