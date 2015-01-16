Rutils
==========

A collection of R scripts

Currently contains:
* dbGaP_xml2table.R: convert an NIH dbGaP variable report (XML format) into a tab-delimited format written to a file.
* hsv2rgb.R: a utility to convert colours from HSV space to RGB. A basic conversion missing from built-in R packages.
* makeColorLighter.R: a utility to return a lighter version of a colour. Converts from RGB to RGB with intermediate HSV conversion. Relies on hsv2rgb.R, also in this project.
