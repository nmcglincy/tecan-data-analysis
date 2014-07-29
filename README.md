tecan-data-analysis
===================

analysis of yeast growth data from the tecan plate reader

tecan-analysis2.R
An archetypal analysis of different yeast strains under different media conditions.

generate-sample-info-template.R
Does exactly what it says on the tin.

read-tecan-data2.R 
Reads in a csv formatted tecan results spreadsheet, and a sample.info csv, then performs statistical analysis of the
run temperature.

growth-curve-analysis-Alg2.R
Fits a smooth spline to growth curve data, and derives interesting descriptive measures from the fit.

---

establishing-the-proper-time-base.R
Work to establish the time delay between wells and between cycles etc. Largely implemented in read-tecan-data2.R

pca-and-manova-1stgo.R
Working out a suitable statistical analysis for the growth curve characteristics.
