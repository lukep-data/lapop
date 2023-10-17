# lapop 1.3.2 

* make text editable when using lapop_save()

# lapop 1.3.1 

* add x-axis label to ccm()

# lapop 1.3.0

* fixing spacing issue in subtitle for lapop_mover() and lapop_ccm()

# lapop 1.2.8 

* add fixed_aspect_ratio = TRUE option to stack()

# lapop 1.2.7

Changes to lapop_ccm including: 
* text placed directly above error bars (no offset).  Offset customizable with text_position argument.  
* fixed automatic alphabetical sorting of bars (now order of dataframe)
* new sorting options (by var2 and var3 possible)

# lapop 1.2.6

* Update lapop_ccm() to allow 3 bars.  Fixed spacing issue with subtitle.  

# lapop 1.2.5

* Update lapop_ccm() with fix for subtitle issue.  Add y-axis label. 
 
# lapop 1.2.4

* Add option to remove percentages in ts and mline

# lapop 1.2.3

* remove "AmericasBarometer" mentions from help files

# lapop 1.2.2

* remove "AmericasBarometer" from automatically appearing in source

# lapop 1.2.1

* Automatically convert varlabel to character in mline() and stack()

# lapop 1.2.0

* Added lapop_ccm() 

# lapop 1.1.2

* Fixed interpolation on beginning of series in lapop_mline()

# lapop 1.1.1

* Change all y-axis defaults to 0-100

# lapop 1.1.0

* Deprecate old function names to match Stata ado file names

# lapop 1.0.4

* Changed lapop_ts() to work with year instead of wave
* Fixed bug with labels in lapop_tsmulti()
* Reduced size of gap between bars in stacked bar
* Added repelling of labels that overlapping in lapop_sb()
* Made subtitle movable in lapop_tsmulti()
* lapop_coef() description header was wrong
* Time series help page: example at the bottom wasincorrectly formatted
* Interpolation by group for lapop_tsmulti() now prevents interpolation of final data point
* Made sorting possible in lapop_sb()

# lapop 1.0.3

* updated interpolation method in lapop_ts()
* add multivariable time series lapop_tsmulti()

# lapop 1.0.2

* Explained acronyms in package description
* Updated return values in function descriptions
* Removed \dontrun{} from lapop_save example and changed file directory to temp

# lapop 1.0.1

* updated errors in description that were rejected by CRAN

# lapop 1.0.0

* prepared lapop package for release to CRAN

# lapop 0.1.0

* Added a `NEWS.md` file to track changes to the package.
