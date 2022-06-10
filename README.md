# lapop-viz
 These are the templates for creating LAPOP's visualizations of AmericasBarometer data.

The workflow for visualization projects is as follows:

1) Create project folder
2) Create or save .do file into project folder.  Run analyses in this .do file using LAPOP .ado files, with output as .csvs saved to your project folder (see help file on .ados)
3) Download or copy-paste templates folder to local machine
4) Copy "meta-template.R" into project folder and rename.  This is the R script you will use to create your visualizations. 
5) Follow instructions within R meta-template to load in .csvs and create visualizations in R using pre-defined functions.  Use lapop_save() to save .svg files to project folder. 
