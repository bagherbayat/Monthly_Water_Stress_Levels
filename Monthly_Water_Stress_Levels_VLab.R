  ### Monthly Water Stress Levels:
  #This workflow quantifies one decade of (agricultural) water stress levels across Europe using satellite-derived Evapotranspiration (ET) data sets and Evaporative Stress Index (ESI) anomalies.
  
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
  #A contribution to SDG 6.4.2 (levels of water stress) with a focus on the agricultural domain
  #This workflow is developed within the European Commission HORIZON 2020 Program ERA-PLANET/GEOEssential project [grant number: 689443].
  
  # Main references:
  #(Anderson et al., 2016, 2010)

  ###################################################################################################
  ## 1. Load required packages
  library(sp)
  library(raster)
  library(prettymapr)
  library(zip)
  
  ## 2. Set Working directory and unzipping the input data
  dir <- "./"
  setwd(dir)
  
  unzip("EU_Border.zip", exdir = dir)
  unzip("ETa.zip", exdir = dir)
  unzip("ET0.zip", exdir = dir)

  ### Processing ET0
  ## 3. Read daily ET0 data, define geostatinary project, reproject to GCS, scale and save them as daily tif files 

  sdir1 <- "./ET0_HDF/"
  list.filenames_ET0 <- list.files(path = sdir1, pattern = "METREF")
  list.data_ET0 <- list()
  
  for (i in 1:length(list.filenames_ET0))
  {
  print(paste(
    "Step 1: Processing ET0 data set",
    i,
    "of",
    length(list.filenames_ET0)
  ))
  
  
  # # Reprojecting the ET0 data element (METREF) of HDF files
  # system(
  #   paste(
  #     'gdal_translate -a_srs "+proj=geos +h=35785831 +a=6378169 +b=6356583.8 +no_defs" -a_ullr -5568000 5568000 5568000 -5568000 HDF5:',
  #     sdir1,
  #     list.filenames_ET0[[i]],
  #     '://METREF temp_METREF.tif',
  #     sep = ""
  #   )
  # )
  
  
  # Reprojecting the ET0 data element (METREF) of HDF files
  system(
    paste(
      'gdal_translate -co COMPRESS=LZW -co BIGTIFF=YES -a_srs "+proj=geos +h=35785831 +a=6378169 +b=6356583.8 +no_defs" -a_ullr -5568000 5568000 5568000 -5568000 HDF5:',
      sdir1,
      list.filenames_ET0[[i]],
      '://METREF temp_METREF.tif',
      sep = ""
    )
  )
  
  system(
    paste(
      'gdalwarp -t_srs EPSG:4326 -te -10 33 34 73 -tr 0.04 0.04 -r bilinear -wo SOURCE_EXTRA=100 -overwrite temp_METREF.tif METREF.tif',
      sep = ""
    )
  )
  
  # Read Reprojected file and apply the scaling
  setwd(dir)
  list.data_ET0[[i]] <- raster(paste(dir, "/METREF.tif", sep = ""))
  list.data_ET0[[i]] <- list.data_ET0[[i]] / 100           #scaling
  list.data_ET0[[i]][list.data_ET0[[i]] < 0] <- NA
  names(list.data_ET0[[i]]) <-
    list.filenames_ET0[[i]] #add the names of the data to the list 
  
   # Make a new subdir and save reprojected and scalled ET0 data in tif format
  dir.create(file.path("DailyET0_tif"), showWarnings = FALSE,recursive = TRUE) #Creat a folder to save the tif files
  sdir2 <- "./DailyET0_tif/"
  file_tif <-  paste(sdir2, list.filenames_ET0[i], '.tif', sep = "")
  writeRaster(list.data_ET0[[i]], file = file_tif,format = "GTiff", overwrite = TRUE)
  
  # Removing previous temp_METREF and METREF
  setwd(dir)
  files <- list.files(path = dir, pattern = "METREF")
  unlink(paste(dir, files, sep = ""))
  
  #Back to data subdir
  sdir1 <- "./ET0_HDF/"
  }
  
  ## 4. Calculate monthly (mean) ET0
  sdir2 <- "./DailyET0_tif/"
  
  list.filenames_ET0 <- list.files(path = sdir2, pattern = "Disk")
  
  dataFrame <- data.frame(FileName = list.filenames_ET0 , year = NA, month = NA, day = NA) ##data frame to store file names and correspnding year, month, and day
  for(i in 1:length(list.filenames_ET0)) ### start of files for loop in each directory 
  {
  # Read date from the file name 
  sp1 <- strsplit(list.filenames_ET0[i],"_")[[1]] ### splilt file name like  "HDF5"             "LSASAF"           "MSG"              "METREF"           "MSG-Disk"         "201101200000.tif" from the file name "HDF5_LSASAF_MSG_METREF_MSG-Disk_201101200000.tif"
  
  sp2 <-  sp1[length(sp1)] #### save "200401200000.tif" to another variable
  
  # Read year, month, and day from sp2, and save as numeric 
  yr <- as.numeric(substr(x=sp2,start=1,stop=4))  ### output 2011
  mm <- as.numeric(substr(x=sp2,start=5,stop=6))  ### output 01
  dd <- as.numeric(substr(x=sp2,start=7,stop=8))### output 20
  
  dataFrame$year[i] <- yr
  dataFrame$month[i] <- mm
  dataFrame$day[i] <- dd
  
  }
  
  
  #### Read avilable years of the data files
  yearAvl <- unique(dataFrame$year)  #### output "2011 2012 ... 2020"
  
  #### check avilable months in each year and assign to another variable created in loop, e.g., mm_2011, mm_2012,.....
  for(j in 1:length(yearAvl))
  {
  temp_file <- dataFrame[which(dataFrame$year == yearAvl[j]),]
  assign(paste0("mm_", yearAvl[j]), unique(temp_file$month) )
  }  
  
  ########## Read data files year wise and mean calculation on the data files of each month
  for(k in 1:length(yearAvl))
  {
  temp_file <- dataFrame[which(dataFrame$year == yearAvl[k]),]
  temp_mm <-  eval(parse(text = paste0("mm_", yearAvl[k])))
  for(m in temp_mm)
  {
    temp_file_mm <- temp_file[which(temp_file$month == m),]
   
     ## stacking all data of a month together
    FileName <- paste(sdir2,temp_file_mm$FileName, sep ="")
    temp_file_together_mm <- lapply(FileName, raster)   
    
    temp_file_stack_mm <- stack(temp_file_together_mm)
    
    ### monthly mean
    monthly_mean <- calc(temp_file_stack_mm, fun = mean,na.rm=T)
    
  
    # Make a new subdir and save mean ET0 data in tif format
    dir.create(file.path("MonthlyET0_tif"), showWarnings = FALSE,recursive = TRUE) #Creat a folder to save the tif files
    sdir3 <- "./MonthlyET0_tif/"
    
    file_tif <-  paste(sdir3, "Mean_month_",m,"_","year","_",yearAvl[k], '.tif', sep = "")
    writeRaster(monthly_mean, file = file_tif,format = "GTiff", overwrite = TRUE)
    
    
  }
  
  }      
  
  rm(list = ls())    #clear all objects in the environment.
  
  
  ### Processing ETa
  ## 5. Read daily ETa data, define geostatinary project, reproject to GCS, scale and save them as daily tif files
  #unzip("ETa.zip", exdir = dir)
  sdir4 <- "./ETa_HDF/"
  
  ## Read  ETa data part 1 (to read from Euro region)
  list.filenames_ETa_Euro <- list.files(path = sdir4, pattern = "Euro")  
  list.data_ETa_Euro <- list()
  
  for (i in 1:length(list.filenames_ETa_Euro))
  
  {
  print(paste(
    "Step 2: Processing Euro region ETa data set",
    i,
    "of",
    length(list.filenames_ETa_Euro)
  ))
  
  # Reprojecting the ETa data element (ET) of HDF files
  system(
    paste(
      'gdal_translate -a_srs "+proj=geos +h=35785831 +a=6378169 +b=6356583.8 +no_defs" -a_ullr -922623.5 5417891 4178899 3469966 HDF5:',
      sdir4,
      list.filenames_ETa_Euro[[i]],
      '://ET temp_DMET.tif',
      sep = ""
    )
  )
  
   system(
    paste(
      'gdalwarp -t_srs EPSG:4326 -te -10 33 34 73 -tr 0.04 0.04 -r bilinear -wo SOURCE_EXTRA=100 -overwrite temp_DMET.tif DMET.tif',
      sep = ""
    )
  )
  
  
  # Read Reprojected file and apply the scaling
  setwd(dir)
  list.data_ETa_Euro[[i]] <- raster(paste(dir, "/DMET.tif", sep = ""))
  list.data_ETa_Euro[[i]] <-
    list.data_ETa_Euro[[i]] / 1000           #scaling
  list.data_ETa_Euro[[i]][list.data_ETa_Euro[[i]] < 0] <- NA
  names(list.data_ETa_Euro[[i]]) <-
    list.filenames_ETa_Euro[[i]] #add the names of data to the list
  
  # Make a new subdir and save reprojected and scalled ETa data in tif format
  dir.create(file.path("DailyETa_tif"), showWarnings = FALSE,recursive = TRUE) #Creat a folder to save the tif files
  sdir5 <- "./DailyETa_tif/"
  file_tif <-  paste(sdir5, list.filenames_ETa_Euro[i], '.tif', sep = "")
  writeRaster(list.data_ETa_Euro[[i]], file = file_tif,format = "GTiff", overwrite = TRUE)
  
  # Removing previous temp_DMET and DMET
  dir <- "./"
  setwd(dir)
  files <- list.files(path = dir, pattern = "DMET")
  unlink(paste(dir, files, sep = ""))
  
  #Back to data subdir
  sdir4 <- "./ETa_HDF/"
  
  }
  rm(list = ls())    #clear all objects in the environment.
  
  ## Read  ETa data part 2 (to read from Disk region)
  sdir4 <- "./ETa_HDF/"
  list.filenames_ETa_Disk <- list.files(path = sdir4, pattern = "Disk")
  list.data_ETa_Disk <- list()
  
  for (i in 1:length(list.filenames_ETa_Disk))
  
  {
  print(paste(
    "Step 2: Processing Disk region ETa data set",
    i,
    "of",
    length(list.filenames_ETa_Disk)
  ))
  # Reprojecting the ETa data element (ET) of HDF files
  system(
    paste(
      'gdal_translate -a_srs "+proj=geos +h=35785831 +a=6378169 +b=6356583.8 +no_defs" -a_ullr -5568000 5568000 5568000 -5568000 HDF5:',
      sdir4,
      list.filenames_ETa_Disk[[i]],
      '://ET temp_DMET.tif',
      sep = ""
    )
  )
  
  # This gdalwarp system command should work for VLab
  system(
    paste(
      'gdalwarp -t_srs EPSG:4326 -te -10 33 34 73 -tr 0.04 0.04 -r bilinear -wo SOURCE_EXTRA=100 -overwrite temp_DMET.tif DMET.tif',
      sep = ""
    )
  )
  
  
  # Read Reprojected file and apply the scaling
  setwd(dir)
  list.data_ETa_Disk[[i]] <- raster(paste(dir, "/DMET.tif", sep = ""))
  list.data_ETa_Disk[[i]] <-
    list.data_ETa_Disk[[i]] / 1000           #scaling
  list.data_ETa_Disk[[i]][list.data_ETa_Disk[[i]] < 0] <- NA
  names(list.data_ETa_Disk[[i]]) <-
    list.filenames_ETa_Disk[[i]] #add the names of data to the list
  
   # save reprojected and scalled ETa data in tif format in sdir5 folder created before
  sdir5 <- "./DailyETa_tif/"
  file_tif <-  paste(sdir5, list.filenames_ETa_Disk[i], '.tif', sep = "")
  writeRaster(list.data_ETa_Disk[[i]], file = file_tif,format = "GTiff", overwrite = TRUE)
  
  # Removing previous temp_DMET and DMET
  dir <- "./"
  setwd(dir)
  files <- list.files(path = dir, pattern = "DMET")
  unlink(paste(dir, files, sep = ""))
  
  #Back to data subdir
  sdir4 <- "./ETa_HDF/"
  
  }
  rm(list = ls())    #clear all objects in the environment.
  
  
  ## 6. Calculate monthly (mean) ETa
  sdir5 <- "./DailyETa_tif/"
  
  list.filenames_ETa <- list.files(path = sdir5, pattern = "DMET") #Reading both Euro and Disk area tif files
  
  dataFrame <- data.frame(FileName = list.filenames_ETa , year = NA, month = NA, day = NA) ##data frame to store file names and correspnding year, month, and day
  for(i in 1:length(list.filenames_ETa)) ### start of files for loop in each directory 
  {
  # Read date from the file name 
  sp1 <- strsplit(list.filenames_ETa[i],"_")[[1]] ### splilt file name like  "HDF5"             "LSASAF"           "MSG"              "METREF"           "MSG-Disk"         "200401200000.tif" from the file name "HDF5_LSASAF_MSG_METREF_MSG-Disk_200401200000.tif"
  
  sp2 <-  sp1[length(sp1)] #### save "200401200000.tif" to another variable
  
  # Read year, month, and day from sp2, and save as numeric 
  
  yr <- as.numeric(substr(x=sp2,start=1,stop=4))  ### output 2004
  mm <- as.numeric(substr(x=sp2,start=5,stop=6))  ### output 01
  dd <- as.numeric(substr(x=sp2,start=7,stop=8))### output 20
  
  dataFrame$year[i] <- yr
  dataFrame$month[i] <- mm
  dataFrame$day[i] <- dd
  
  }
  
  
  #### Read avilable years of the data files
  yearAvl <- unique(dataFrame$year)  #### output "2011 2012 ... 2020"
  
  #### check avilable months in each year and assign to another variable created in loop, e.g., mm_2011, mm_2012,.....
  for(j in 1:length(yearAvl))
  {
  temp_file <- dataFrame[which(dataFrame$year == yearAvl[j]),]
  assign(paste0("mm_", yearAvl[j]), unique(temp_file$month) )
  }  
  
  ########## Read data files year wise and mean calculation on the data files of each month
  for(k in 1:length(yearAvl))
  {
  temp_file <- dataFrame[which(dataFrame$year == yearAvl[k]),]
  temp_mm <-  eval(parse(text = paste0("mm_", yearAvl[k])))
  for(m in temp_mm)
  {
    temp_file_mm <- temp_file[which(temp_file$month == m),]
    
    ## stacking all data of a month together
    FileName <- paste(sdir5,temp_file_mm$FileName, sep ="")
    temp_file_together_mm <- lapply(FileName, raster)   
    
    temp_file_stack_mm <- stack(temp_file_together_mm)
    
    ### monthly mean
    monthly_mean <- calc(temp_file_stack_mm, fun = mean,na.rm=T)
    
    
    # Make a new subdir and save mean ET0 data in tif format
    dir.create(file.path("MonthlyETa_tif"), showWarnings = FALSE,recursive = TRUE) #Creat a folder to save the tif files
    sdir6 <- "./MonthlyETa_tif/"
    
    file_tif <-  paste(sdir6, "Mean_month_",m,"_","year","_",yearAvl[k], '.tif', sep = "")
    writeRaster(monthly_mean, file = file_tif,format = "GTiff", overwrite = TRUE)
    
    
  }
  
  }      
  
  rm(list = ls())    #clear all objects in the environment.
  
  ## 7. Compute monthly Evaporative Stress Index (ESI)
  sdir6 <- "./MonthlyETa_tif/"
  list.filenames_ETa <- list.files(path = sdir6, pattern = "Mean_month") 
  FileName <- paste(sdir6,list.filenames_ETa, sep ="")
  list.data_ETa <- lapply(FileName, raster)
  
  sdir3 <- "./MonthlyET0_tif/"
  list.filenames_ET0 <- list.files(path = sdir3, pattern = "Mean_month") 
  FileName <- paste(sdir3,list.filenames_ET0, sep ="")
  list.data_ET0 <- lapply(FileName, raster)
  
  
  list.data_ESI <-list()
  
  for (i in 1:length(list.filenames_ET0))
  {
  print(paste(
    "Step 3: Computing ESI values",
    i,
    "of",
    length(list.filenames_ET0)
  ))
  
  list.data_ESI[[i]] <-
    (list.data_ETa[[i]] / list.data_ET0[[i]])   # computing monthly ESI
  
  # Set LB for ESI
  list.data_ESI[[i]][list.data_ESI[[i]] < 0] <-
    0    #Minimum ESI is 0 (LB)
  # Set UB for ESI
  list.data_ESI[[i]][list.data_ESI[[i]] > 1.5] <-
    1.5    #Maximum ESI is normally 1 since ET0 is usually higher than ETa. However, we accept values slightly higher than 1 (upto 1.5) for situations when ETa can slightly be hiher than ET0 (for instance, in rain events!)
  
  names(list.data_ESI[[i]]) <-
    list.filenames_ET0[[i]] #add the names of data to the list
  
  
  # Make a new subdir and save mean ESI data in tif format
  dir.create(file.path("MonthlyESI_tif"), showWarnings = FALSE,recursive = TRUE) #Creat a folder to save the tif files
  sdir7 <- "./MonthlyESI_tif/"
  
  file_tif <- paste(sdir7, list.filenames_ET0[i], sep = "")
  writeRaster(list.data_ESI[[i]], file = file_tif,format = "GTiff", overwrite = TRUE)
  
  }
  
  # 8. Calculate long-term mean ("normal") and std for each month of ESI (from time series)
  
  #getting the names from ET0 files
  sdir7 <- "./MonthlyESI_tif/"
  list.filenames_ESI <- list.files(path = sdir7, pattern = "Mean_month") 
  dataFrame <- data.frame(FileName = list.filenames_ESI , year = NA, month = NA) ##data frame to store file names and correspnding year and month
  
  for(i in 1:length(list.filenames_ESI)) ### start of files for loop in each directory 
  {
  #### Read date from the file name 
  sp1 <- strsplit(list.filenames_ESI[i],"_")[[1]] ### splilt file name like  "mean"             "month"           "1"              "year"           "2011" from the file name "Mean_month_1_year_2011.tif"
  
  sp2 <-  sp1[length(sp1)]
  sp3 <-  sp1[3]
  
  ### read year and month from sp2 and sp3, and save as numeric 
  
  yr <- as.numeric(substr(x=sp2,start=1,stop=4))  ### output 2011
  mm <- as.numeric(substr(x=sp3,start=1,stop=2))  ### output 01
  
  dataFrame$year[i] <- yr
  dataFrame$month[i] <- mm
  
  }
  
  # Extracting all data of a specific month in all available years and take a mean and SD for that month; here I assume data of a month are available for atleast one year.
  for(l in 1:12)
  {
  temp_file_all <- dataFrame[which(dataFrame$month == l),]
  
  FileName <- paste(sdir7,temp_file_all$FileName, sep ="")
  temp_file_together_all <- lapply(FileName, raster)
  
  temp_file_stack_all <- stack(temp_file_together_all)
  All_years_mean <- calc(temp_file_stack_all, fun = mean,na.rm =T)
  All_years_sd <- calc(temp_file_stack_all, fun = sd,na.rm =T)
  
  
  # Make a new subdir and save monthly normal ESI in tif format
  dir.create(file.path("MonthlyNormal_ESI_tif"), showWarnings = FALSE,recursive = TRUE) #Creat a folder to save the tif files
  sdir8 <- "./MonthlyNormal_ESI_tif/"
  file_tif <-  paste(sdir8, "Mean_month_",l, '.tif', sep = "")
  writeRaster(All_years_mean, file = file_tif,format = "GTiff", overwrite = TRUE)
  
  file_tif2 <-  paste(sdir8, "SD_month_",l, '.tif', sep = "")
  writeRaster(All_years_sd, file = file_tif2,format = "GTiff", overwrite = TRUE)
  
  }
  
  # 9. Compute Anomalies in monthly ESI for the time series
  
  #### Read avilable years of the data files
  yearAvl <- unique(dataFrame$year)  #### output "2011 2012 2013 ... 2020"
  
  #### check avilable months in each year and assign to another variable created in loop, e.g., mm_2004, mm_2005,.....
  for(j in 1:length(yearAvl))
  {
  temp_file <- dataFrame[which(dataFrame$year == yearAvl[j]),]
  assign(paste0("mm_", yearAvl[j]), unique(temp_file$month) )
  }  
  
  ########## Read data files year wise and mean calculation on the data files of each month
  for(k in 1:length(yearAvl))
  {
  temp_file <- dataFrame[which(dataFrame$year == yearAvl[k]),]
  temp_mm <-  eval(parse(text = paste0("mm_", yearAvl[k])))
  for(m in temp_mm)
  {
    temp_file_mm <- temp_file[which(temp_file$month == m),]
  
    monthly_mean <- raster(paste(sdir7,temp_file_mm$FileName, sep =""))  # collect monthly ESI values
    
  
    All_years_mean <- raster(paste(sdir8,"Mean_month_",m,".tif", sep = '')) # collect monthly normal ESI values
    All_years_sd <- raster(paste(sdir8,"SD_month_",m,".tif", sep = '')) # collect monthly SD ESI values
    
    #### calculate monthly anomalies
  
    ESI_Anomaly_mm <-  (monthly_mean - All_years_mean)/ All_years_sd

    # Make a new subdir and save monthly ESI anomaly in tif format
    dir.create(file.path("MonthlyESI_Anomaly_tif"), showWarnings = FALSE,recursive = TRUE) #Creat a folder to save the tif files
    sdir9 <- "./MonthlyESI_Anomaly_tif/"
    file_tif <-  paste(sdir9, "ESI_Anomaly_","month_",m,"_","year","_",yearAvl[k], '.tif', sep = "")
    writeRaster(ESI_Anomaly_mm, file = file_tif,format = "GTiff", overwrite = TRUE)
   
  }
  }
  
  # 10. plotting monthly ESI Anomalies
  
  sdir9 <- "./MonthlyESI_Anomaly_tif/"
  list.filenames_ESI_Anomaly <- list.files(path = sdir9, pattern = "ESI_Anomaly") 
  FileName <- paste(sdir9,list.filenames_ESI_Anomaly, sep ="")
  list.data_ESI_Anomaly <- lapply(FileName, raster)
  
  #Reading EU shapefile for Masking the HDF data
  #unzip("EU_Border.zip", exdir = dir)
  sdir0 <- "./EU_Border/" #set working directory
  unzip(zipfile = "./EU_Border/data.zip", exdir = "./EU_Border/data")#unzipping the data folder
  file <- paste(sdir0, "/data/NUTS_RG_01M_2013_Update.shp", sep = "")
  europe.map <- shapefile(file) #reading unzipped shapefile
  
  europe.map <- europe.map[europe.map$STAT_LEVL_ == 0,] #reading country (state) level data
  
  project <-  "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  europe.map <- spTransform(europe.map, project)
  e <- extent(-10, 34, 33, 73) #This is EU border extent
  europe.map <- crop(europe.map, e)
  
  list.filenames_ESI_Anomaly_short<-list()
  for(i in 1:length(list.filenames_ESI_Anomaly)) ### start of files for loop in each directory
  {
  #### Read date from the file name
  sp1 <- strsplit(list.filenames_ESI_Anomaly[i],"_")[[1]] ### splilt file name like  "mean"             "month"           "1"              "year"           "2004" from the file name "Mean_month_1_year_2004.tif"
  
  sp2 <-  sp1[1]
  sp3 <-  sp1[2]
  sp4 <- as.numeric(substr(x=sp1[6],start=1,stop=4))
  sp5 <-  as.numeric(sp1[4])
  sp5 <- sprintf("%02d",sp5) # Change 1 character to 2 characters for the numbers 01 to 09 (instead of 1 to 9) 
  
  list.filenames_ESI_Anomaly_short[i] <-paste("WaterStress","_",sp4,sp5,sep="")
  
  }
  
  
  ## 11. Classifying the ESIA map
  #Create classification matrix
  reclass_df <- c(2,Inf,1,
                  1.5,2,2,
                  1,1.5, 3,
                  0,1,4,
                  -1,0,5,
                  -1.5,-1,6,
                  -2,-1.5,7,
                  -Inf,-2,8)
  
  r_colors <-
    c("darkgreen",
      "green",
      "greenyellow",
      "yellow",
      "darkgoldenrod1",
      "hotpink3",
      "red",
      "darkred")
  
  #Reshape the object into a matrix with columns and rows
  reclass_m <- matrix(reclass_df,
                      ncol = 3,
                      byrow = TRUE)
  
  
  list.data_ESI_Anomaly_mask<-list()
  list.data_ESI_Anomaly_reclass<-list()
  for (i in 1:length(list.data_ESI_Anomaly))
  {
    
    #masking based on EU border before reclassify
    list.data_ESI_Anomaly_mask[[i]] <- mask(x = list.data_ESI_Anomaly[[i]], mask = europe.map) 
  
  
   #Reclassify the raster using the reclass object - reclass_m
    list.data_ESI_Anomaly_reclass[[i]] <-
    reclassify(list.data_ESI_Anomaly_mask[[i]], reclass_m)
  
  #Plot reclassified data
  # Make a new subdir and save classified monthly ESI anomaly in jpg format
  dir.create(file.path("EU_Monthly_WaterStress_Maps"), showWarnings = FALSE,recursive = TRUE) 
  sdir10 <- "./EU_Monthly_WaterStress_Maps/"
  
  
   dpi <- 500
  jpeg(paste(sdir10, list.filenames_ESI_Anomaly_short[i], '.jpg', sep = ""),
    width = 10 * dpi,
    height = 6 * dpi,
    res = dpi)  
  
  
  #Margins for our plot
  par(mar = c(4, 4, 1.2, 0))
  
  plot(
    list.data_ESI_Anomaly_reclass[[i]],
    breaks = 0:8,
    xlim = c(-20, 40),
    ylim = c(30, 75),
    legend = FALSE,
    col = r_colors,
    xlab = "Longitude [deg]",
    ylab = "Latitude [deg]"
  ) #breaks are needed for assigning colors to classes correctly
  
  plot(
    europe.map,
    add = TRUE,
    lwd = 1,
    xlim = c(-20, 40),
    ylim = c(30, 75)
  ) 
  
  addnortharrow(
    pos = "topright",
    padin = c(0.15, 0.15),
    scale = 0.65,
    lwd = 0.65,
    border = "black",
    cols = c("white", "black"),
    text.col = "black"
  )
  
  raster::scalebar(
    d = 1000,
    # distance in km
    xy = c(-40, 34),
    type = "bar",
    divs = 4,
    below = "km",
    lonlat = TRUE,
    label = c(0, 500, 1000),
    adj = c(0, -0.75),
    lwd = 1
  )
  
  mtext(
    "GCS WGS 1984",
    side = 1,
    line = -1.43,
    cex = 0.9,
    at = 50
  )
  
  legend(
    "topleft",
    legend = c(
      "Extreme Wet",
      "Severe Wet",
      "Moderate Wet",
      "Mild Wet",
      "Mild Drought",
      "Moderate Drought",
      "Severe Drought",
      "Extreme Drought"
    ),
    
    fill = r_colors,
    border = "black",
    bty = "n",
    title = "Water stress levels"
  )
  
  title(list.filenames_ESI_Anomaly_short[i])
  dev.off()
  
  
  ## 12. Lets generate some statistics per country as a text report
  
  #Extract raster values to polygons
  (v <- extract(list.data_ESI_Anomaly_reclass[[i]], europe.map[1]))
  
  #Get class counts for each polygon
  v.counts <- lapply(v, table)
  
  #Calculate class percentages for each polygon
  (v.pct <- lapply(
    v.counts,
    FUN = function(x) {
      (x / sum(x)) * 100
    }
  ))
  
  #Seperate columns of v.pct
  out1 <- lapply(v.pct , '[', '1')
  out2 <- lapply(v.pct , '[', '2')
  out3 <- lapply(v.pct , '[', '3')
  out4 <- lapply(v.pct , '[', '4')
  out5 <- lapply(v.pct , '[', '5')
  out6 <- lapply(v.pct , '[', '6')
  out7 <- lapply(v.pct , '[', '7')
  out8 <- lapply(v.pct , '[', '8')
  
  
  #Replace missing values with NA in different columns
  l1 <-
    sapply(out1 , function(x)
      c(x , rep(NA , 1 - length(x))))
  l2 <-
    sapply(out2 , function(x)
      c(x , rep(NA , 1 - length(x))))
  l3 <-
    sapply(out3 , function(x)
      c(x , rep(NA , 1 - length(x))))
  l4 <-
    sapply(out4 , function(x)
      c(x , rep(NA , 1 - length(x))))
  l5 <-
    sapply(out5 , function(x)
      c(x , rep(NA , 1 - length(x))))
  l6 <-
    sapply(out6 , function(x)
      c(x , rep(NA , 1 - length(x))))
  l7 <-
    sapply(out7 , function(x)
      c(x , rep(NA , 1 - length(x))))
  l8 <-
    sapply(out8 , function(x)
      c(x , rep(NA , 1 - length(x))))
  
  Class_1 <- lapply(l1, unlist)
  Class_2 <- lapply(l2, unlist)
  Class_3 <- lapply(l3, unlist)
  Class_4 <- lapply(l4, unlist)
  Class_5 <- lapply(l5, unlist)
  Class_6 <- lapply(l6, unlist)
  Class_7 <- lapply(l7, unlist)
  Class_8 <- lapply(l8, unlist)
  
  #Creating an empty datafram
  df <- as.data.frame(matrix(ncol = 8, nrow = 34))
  names(df) <- c(1, 2, 3, 4, 5, 6,7,8)
  
  #Filling the empty datafram with columns
  df[1] <- do.call(rbind, Class_1)
  df[2] <- do.call(rbind, Class_2)
  df[3] <- do.call(rbind, Class_3)
  df[4] <- do.call(rbind, Class_4)
  df[5] <- do.call(rbind, Class_5)
  df[6] <- do.call(rbind, Class_6)
  df[7] <- do.call(rbind, Class_7)
  df[8] <- do.call(rbind, Class_8)
  
  
  class.df <- df
  
  #Replace NA's with 0 and add names and naming the columns
  class.df[is.na(class.df)] <-
    0    #This changes NA values to zero in the report for certain classes that do not have any value (for instance, CZ do not have any near normal and moderate classes, so they are considered as NA in the report so we need to change this to zero)
  colnames(class.df) <-
  c("Extreme Wet",
  "Severe Wet",
  "Moderate Wet",
  "Mild Wet",
  "Mild Drought",
  "Moderate Drought",
  "Severe Drought",
  "Extreme Drought")
  
  #Adding a new columns for EU countries names
  class.df$Country = europe.map[[1]]
  class.df[c(
    "Country",
    "Extreme Wet",
    "Severe Wet",
    "Moderate Wet",
    "Mild Wet",
    "Mild Drought",
    "Moderate Drought",
    "Severe Drought",
    "Extreme Drought")]
  
      #Here is the individual daily CSV reports
      dir.create(file.path("EU_Monthly_WaterStress_Reports"), showWarnings = FALSE,recursive = TRUE) #Creat a folder to save the tif files
      sdir11 <- "./EU_Monthly_WaterStress_Reports/"
      
      csvfile <-
        paste(sdir11, list.filenames_ESI_Anomaly_short[i], '.csv', sep = "")
      write.table(
        class.df[c(
          "Country",
          "Extreme Wet",
          "Severe Wet",
          "Moderate Wet",
          "Mild Wet",
          "Mild Drought",
          "Moderate Drought",
          "Severe Drought",
          "Extreme Drought")],
        file =  csvfile,
        sep = ",",
        row.names = F,
        col.names = T,
        quote = F
      )
    
  
  #Here is the individual monthly tif maps masked for EU 
  dir.create(file.path("EU_Monthly_WaterStress_Rasters"), showWarnings = FALSE,recursive = TRUE) #Creat a folder to save the tif files
  sdir12 <- "./EU_Monthly_WaterStress_Rasters/"
  
  file_tif <-
    paste(sdir12, list.filenames_ESI_Anomaly_short[i], '.tif', sep = "")
  writeRaster(
    list.data_ESI_Anomaly_mask[[i]],
    file = file_tif,
    format = "GTiff",
    overwrite = TRUE
  )
  }
  
  #13. Zipping all generated output files
  
  files_jpg <- list.files(path = sdir10, pattern = "jpg")
  zipfile_jpg <- "EU_Monthly_WaterStress_Maps.zip"
  zip(
  zipfile_jpg,
  paste(sdir10, files_jpg, sep = ""),
  recurse = T,
  compression_level = 9,
  include_directories = F,
  root = ".",
  mode = c("cherry-pick")
  )
  
  zipfile_csv <- "EU_Monthly_WaterStress_Reports.zip"
  files_csv <- list.files(path = sdir11, pattern = "csv")
  zip(
  zipfile_csv,
  paste(sdir11, files_csv, sep = ""),
  recurse = T,
  compression_level = 9,
  include_directories = F,
  root = ".",
  mode = c("cherry-pick")
  )
  
  zipfile_tif <- "EU_Monthly_WaterStress_Rasters.zip"
  files_tif <- list.files(path = sdir12, pattern = "tif")
  zip(
  zipfile_tif,
  paste(sdir12, files_tif, sep = ""),
  recurse = T,
  compression_level = 9,
  include_directories = F,
  root = ".",
  mode = c("cherry-pick")
  )
  

  
