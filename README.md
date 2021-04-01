  ### Monthly_Water_Stress_Levels:
  #This workflow quantifies one decade of (agricultural) water stress levels across Europe using satellite-derived Evapotraspiration (ET) data sets and Evaporative Stress Index (ESI) anomalies.
  
  ##Authors: Bagher Bayat (b.bayat@fz-juelich.de and bagher.bayat@gmail.com), Carsten Montzka (c.montzka@fz-juelich.de), Harry Vereecken (h.vereecken@fz-juelich.de) 
  #Institute of Bio- and Geosciences: Agrosphere (IBG-3), Forschungszentrum Jülich GmbH, 52425 Jülich, Germany
  #Date:  10 March 2020, Updated: 1 April 2021
  
  ## Main inputs:
  #1. Time series of actual evapotranspiration (ETa) data set at daily step [mm] derived from the Spinning Enhanced Visible and Infrared Imager (SEVIRI) sensor onboard the Meteosat Second Generation (MSG) satellites
  #2. Time series of reference evapotranspiration (ET0) data set at daily step [mm] derived from the Spinning Enhanced Visible and Infrared Imager (SEVIRI) sensor onboard the Meteosat Second Generation (MSG) satellites
  #3. Study area border as a (polygon) shapefile
  
  ## Main outputs:
  #1. One-decade (2011-2020) maps of monthly water stress levels (in jpg format) archived in a zip file
  #2. One-decade (2011-2020) maps of monthly water stress levels (in GTiff format) archived in a zip file
  #3. Text reports (tables) containing water stress levels (in CSV format) based on the percentage of the total land area archived in a zip file
  
  ## Extent:
  #European Union (include 34 countries)
  #Spatial resolution: 4 km
  #Temporal resolution: Monthly 
  
  ## Targeted Policy and indicator:
  #A contribution to SDG 6.4.2 (levels of water stress) with a focus on agricultural domain
  #This workflow is developed within the European Commission HORIZON 2020 Program ERA-PLANET/GEOEssential project [grant number: 689443].
  
  # Main references:
  #(Anderson et al., 2016, 2010)
