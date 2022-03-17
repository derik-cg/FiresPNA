# FiresPNA
Scripts for analysis of fires in PNA

Created by: Derik Castillo Guajardo
Universidad Autónoma Metropolitana
Unidad Lerma
Departamento de Ciencias Ambientales
d.castillo@correo.ler.uam.mx

These scripts helped to organize all data manipulations and analysis for the paper Simple and Complex Fire Regimes in 44 Protected Natural Areas y Mexico, to be published in the International Journal of Wildfires.
The organization of the scripts is as follows:
The GeoTifs are stored by folders labeled with the year of observations. There is a "root" directory for the GeoTifs. This file structure is important for the script, since it will go thorugh all files. The stages of data manipulation and analyis correspond to different files named "masterX" where X is a number

## Master1
This script needs all the Geotifs and one Shapefile with the official boundaries of all 182 Natural Protected Areas in Mexico.
Each Geotif was loaded in R, then clipped and masked. The resulting "small" GeoTifs were saved in the same file structure.

## Master2
This scripts essentially generates a table for all GeoTifs and if fires were detected.
This script uses all the "small" GeoTiffs. This file is just for checking that there were indeed fires in there. It does not produces data used in the paper. If fires were not detected, then one has to go back to double check the "big" GeoTifs and the Shepefile. For reference the first fire was detected in Laguna de Términos in November 2000. 

## Master3
This file is obsolete.

## Master 4
This script vectorizes all the fires. This script uses all the "small" GeoTifs and draws the outline of all fire. A fire was defined as a cluster of burnt pixels. The output file is an .RData file for each PNA and for each month. The .RData file contains two objects: First the vectorized raster (vct.rst), second the list of the corrsponding raster files. Within the vectorized raster, a tibble with all the fires, the coordinates of each corner in the fire, and other geographical attributes.

## Master 5
This script is for a preliminary check that a Time Series can be generated, so the TS is very simple. For the date component, the year and julian date, for the fire component, the number of burned pixels, together with the names of PNAs. This script does not produce data for the paper.

## Master 6
This script produces the time series used in the paper. This script needs the .RData files generated in master 4, together with the "small" rasters generated in master 1. Then, using the vectorized outlines of fires, it recovers all the dates from the Geotifs. This script also computes the area of the outline. The output is a data.frame with name, date and area.

## stats.R
This script produces the main statistics: The power spectra and the Weibull median. The autocorrelation function is also there for comparison with the power spectra. In order to obtain the power spectra, the file pwrspc.R must be run first. In the same way, file tpeaks.R must be run in order to compute the largest peaks in the periodogram.
