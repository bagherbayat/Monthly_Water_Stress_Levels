### EDI_Values_Workflow:
#This workflow quantifies one decade of (agricultural) water stress levels across Europe using satellite-derived Evapotranspiration (ET) data sets and Evaporative Drought Index (EDI) values

##Authors: Bagher Bayat (b.bayat@fz-juelich.de and bagher.bayat@gmail.com), Carsten Montzka (c.montzka@fz-juelich.de), Harry Vereecken (h.vereecken@fz-juelich.de)
#Institute of Bio- and Geosciences: Agrosphere (IBG-3), Forschungszentrum Jülich GmbH, 52425 Jülich, Germany
#Date:  20 October 2020

## Main inputs:
#1. Time series of actual evapotranspiration (ETa) data set at daily step [mm] derived from the Spinning Enhanced Visible and Infrared Imager (SEVIRI) sensor onboard the Meteosat Second Generation (MSG) satellites
#2. Time series of reference evapotranspiration (ET0) data set at daily step [mm] derived from the Spinning Enhanced Visible and Infrared Imager (SEVIRI) sensor onboard the Meteosat Second Generation (MSG) satellites
#3. Study area border as a (polygon) shapefile

## Main outputs:
#1. Maps of water stress (in jpg format) archived in a zip file
#2. Maps of water stress levels (in GTiff format) archived in a zip file
#3. Text reports (tables) containing water stress levels (in CSV format) based on the percentage of the total land area archived in a zip file

## Extent:
#European Union (with the potential to expand to other regions across the globe)
#Spatial resolution: 4km
#Temporal resolution: daily (with the potential to be adopted for weekly, monthly and yearly)

## Targeted Policy and indicator:
#SDG 6.4 (indicator 6.4.2: Levels of water stress)
#This workflow is developed within the European Commission HORIZON 2020 Program ERA-PLANET/GEOEssential project [grant number: 689443].

## Main reference:
#(Yao et al., 2010)
