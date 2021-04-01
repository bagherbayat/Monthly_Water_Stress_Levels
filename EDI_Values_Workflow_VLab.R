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

###################################################################################################
## 1. Load required packages
library(sp)
library(raster)
library(prettymapr)
library(zip)

## 2. Set Working directory
dir <- "./"
setwd(dir)

## 3. Read  ET0 data
sdir <- "./ET0/"
list.filenames_ET0 <- list.files(path = sdir, pattern = "Disk")
list.data_ET0 <- list()

for (i in 1:length(list.filenames_ET0))
{
  print(paste(
    "Step 1: Processing ET0 data set",
    i,
    "of",
    length(list.filenames_ET0)
  ))
  
  # Reprojecting the ET0 data element (METREF) of HDF files
  system(
    paste(
      'gdal_translate -a_srs "+proj=geos +h=35785831 +a=6378169 +b=6356583.8 +no_defs" -a_ullr -5568000 5568000 5568000 -5568000 HDF5:',
      sdir,
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
  
  # Read Reprojected file and apply the scalling
  setwd(dir)
  list.data_ET0[[i]] <- raster(paste(dir, "/METREF.tif", sep = ""))
  list.data_ET0[[i]] <- list.data_ET0[[i]] / 100           #scaling
  list.data_ET0[[i]][list.data_ET0[[i]] < 0] <- NA
  names(list.data_ET0[[i]]) <-
    list.filenames_ET0[[i]] #add the names of the data to the list
}

## 4. Read  ETa data
## 4.1 Read  ETa data part 1 (to read from Euro region)
sdir <- "./ETa/"
list.filenames_ETa_Euro <- list.files(path = sdir, pattern = "Euro")
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
      sdir,
      list.filenames_ETa_Euro[[i]],
      '://ET temp_DMET.tif',
      sep = ""
    )
  )
  
  system(
    paste(
      'gdalwarp -t_srs EPSG:4326 -te -10 33 34 73 -tr 0.04 0.04 -r bilinear -wo SOURCE_EXTRA=100 -overwrite temp_DMET.tif ET.tif',
      sep = ""
    )
  )
  
  # Read Reprojected file and apply the scalling
  setwd(dir)
  list.data_ETa_Euro[[i]] <- raster(paste(dir, "/ET.tif", sep = ""))
  list.data_ETa_Euro[[i]] <-
    list.data_ETa_Euro[[i]] / 1000           #scaling
  list.data_ETa_Euro[[i]][list.data_ETa_Euro[[i]] < 0] <- NA
  names(list.data_ETa_Euro[[i]]) <-
    list.filenames_ETa_Euro[[i]] #add the names of data to the list
}

## 4.2 Read  ETa data part 2 (to read from Disk region)
sdir <- "./ETa/"
list.filenames_ETa_Disk <- list.files(path = sdir, pattern = "Disk")
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
      sdir,
      list.filenames_ETa_Disk[[i]],
      '://ET temp_DMET.tif',
      sep = ""
    )
  )
  
  system(
    paste(
      'gdalwarp -t_srs EPSG:4326 -te -10 33 34 73 -tr 0.04 0.04 -r bilinear -wo SOURCE_EXTRA=100 -overwrite temp_DMET.tif ET.tif',
      sep = ""
    )
  )
  
  # Read Reprojected file and apply the scalling
  setwd(dir)
  list.data_ETa_Disk[[i]] <- raster(paste(dir, "/ET.tif", sep = ""))
  list.data_ETa_Disk[[i]] <-
    list.data_ETa_Disk[[i]] / 1000           #scaling
  list.data_ETa_Disk[[i]][list.data_ETa_Disk[[i]] < 0] <- NA
  names(list.data_ETa_Disk[[i]]) <-
    list.filenames_ETa_Disk[[i]] #add the names of data to the list
}

# Combine two lists (list.data_ETa_Euro and list.data_ETa_Disk)
list.data_ETa <-
  do.call(c, list(list.data_ETa_Euro, list.data_ETa_Disk))

## 5. Compute Evaporative Drought Index (EDI)
list.data_EDI <- list()

for (i in 1:length(list.filenames_ET0))
{
  print(paste(
    "Step 3: Computing EDI values",
    i,
    "of",
    length(list.filenames_ET0)
  ))
  
  list.data_EDI[[i]] <-
    1 - (list.data_ETa[[i]] / list.data_ET0[[i]])   # computing EDI
  
  # Set LB for EDI
  list.data_EDI[[i]][list.data_EDI[[i]] < 0] <-
    0    #Minimum EDI is 0 (LB)
  
  
  # Getting the names from input files. ET0 is used since these data sets have always fixed file naming system
  list.filenames_ET0[i] <-
    strsplit(list.filenames_ET0[i], split = 'Disk_')[[1]][2]  #spliting the title to two parts and taking the date
  list.filenames_ET0[i] <- gsub('.{4}$', '', list.filenames_ET0[i])
  list.filenames_ET0[i] <-
    paste("Water_Stress_Levels_", list.filenames_ET0[i], sep = " ")#(underlines [_] in file names are important for VLab)
  
  # 6. Masking the map based on European countries border
  ### This runs locally
  # sdir <- "./EU_Border/" #set working directory
  # unzip(zipfile = "./EU_Border/data.zip", exdir = "./EU_Border/data")#unzipping the data folder
  # file <- paste(sdir, "/data/NUTS_RG_01M_2013_Update.shp", sep = "")
  # europe.map <- shapefile(file) #reading unzipped shapefile
  
  # ## This runs on VLab
  sdir <- "./EU_Border/" #set working directory
  system("unzip ./SHP/data.zip -d ./SHP/")
  file <- paste(dir, "/SHP/NUTS_RG_01M_2013_Update.shp", sep = "")
  europe.map <- shapefile(file)
  
  europe.map <-
    europe.map[europe.map$STAT_LEVL_ == 0, ] #reading country (state) level data
  
  project <-
    "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  europe.map <- spTransform(europe.map, project)
  e <- extent(-10, 34, 33, 73) #This is EU border extent
  europe.map <- crop(europe.map, e)
  
  list.data_EDI[[i]] <-
    mask(x = list.data_EDI[[i]], mask = europe.map) #masking EDI products
  
  ## 7. Classifying the EDI map
  #Create classification matrix
  reclass_df <-  c(-Inf, 0.2, 1,
                   0.2, 0.4, 2,
                   0.4, 0.6, 3,
                   0.6, 0.8, 4,
                   0.8, 1, 5,
                   1, Inf, 6)
  
  #Reshape the object into a matrix with columns and rows
  reclass_m <- matrix(reclass_df,
                      ncol = 3,
                      byrow = TRUE)
  
  #Reclassify the raster using the reclass object(reclass_m)
  list.data_EDI[[i]] <-
    reclassify(list.data_EDI[[i]], reclass_m)
  
  #Plot reclassified data
  r_colors <-
    c("greenyellow",
      "yellow",
      "burlywood1",
      "darkgoldenrod1",
      "red",
      "darkred")
  
  #Margins for our plot
  par(mar = c(4, 4, 1.2, 0)) # Set the margin on all sides
  
  plot(
    list.data_EDI[[i]],
    breaks = 0:6,
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
    adj = c(0,-0.75),
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
    legend = c("<0.2",
               "0.2 - 0.4",
               "0.4 - 0.6",
               "0.6 - 0.8",
               "0.8 - 1",
               "> 1"),
    
    fill = r_colors,
    border = "black",
    bty = "n",
    # turn off legend border
    title = "EDI values [-]"
  )
  
  
  title(list.filenames_ET0[i])
  
  
  ## 8. Saving daily time series as jpg files
  dir.create(file.path("Water_Stress_Maps_jpg"), recursive = TRUE) #Creat a folder to save the jpg files
  sdir <- "./Water_Stress_Maps_jpg/"
  
  dpi <- 500
  jpeg(
    paste(sdir, list.filenames_ET0[i], '.jpg', sep = ""),
    width = 10 * dpi,
    height = 6 * dpi,
    res = dpi
  )
  
  #Margins for our plot
  par(mar = c(4, 4, 1.2, 0))
  
  plot(
    list.data_EDI[[i]],
    breaks = 0:6,
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
  
  # Using raster library for scalebar
  raster::scalebar(
    d = 1000,
    # distance in km
    xy = c(-40, 34),
    type = "bar",
    divs = 4,
    below = "km",
    lonlat = TRUE,
    label = c(0, 500, 1000),
    adj = c(0,-0.75),
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
    legend = c("<0.2",
               "0.2 - 0.4",
               "0.4 - 0.6",
               "0.6 - 0.8",
               "0.8 - 1",
               "> 1"),
    
    fill = r_colors,
    border = "black",
    bty = "n",
    title = "EDI values [-]"
  )
  
  title(list.filenames_ET0[i])
  dev.off()
  
  ## 9. Lets generate some statistics per country as a text report
  
  #Extract raster values to polygons
  (v <- extract(list.data_EDI[[i]], europe.map[1]))
  
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
  
  Class_1 <- lapply(l1, unlist)
  Class_2 <- lapply(l2, unlist)
  Class_3 <- lapply(l3, unlist)
  Class_4 <- lapply(l4, unlist)
  Class_5 <- lapply(l5, unlist)
  Class_6 <- lapply(l6, unlist)
  
  #Creating an empty datafram
  df <- as.data.frame(matrix(ncol = 6, nrow = 34))
  names(df) <- c(1, 2, 3, 4, 5, 6)
  
  #Filling the empty datafram with columns
  df[1] <- do.call(rbind, Class_1)
  df[2] <- do.call(rbind, Class_2)
  df[3] <- do.call(rbind, Class_3)
  df[4] <- do.call(rbind, Class_4)
  df[5] <- do.call(rbind, Class_5)
  df[6] <- do.call(rbind, Class_6)
  
  class.df <- df
  
  #Replace NA's with 0 and add names and naming the columns
  class.df[is.na(class.df)] <-
    0    #This changes NA values to zero in the report for certain classes that do not have any percentage (for instance, CZ do not have any near normal, moderate classes, so they are considered as NA in the report so we need to change this to zero)
  colnames(class.df) <-
    c("[<0.2]",
      "[0.2 - 0.4]",
      "[0.4 - 0.6]",
      "[0.6 - 0.8]",
      "[0.8 - 1]",
      "[>1]")
  
  #Adding a new columns for EU countries names
  class.df$Country = europe.map[[1]]
  class.df[c("Country",
             "[<0.2]",
             "[0.2 - 0.4]",
             "[0.4 - 0.6]",
             "[0.6 - 0.8]",
             "[0.8 - 1]",
             "[>1]")]
  
  #Here is the individual daily CSV reports
  dir.create(file.path("Water_Stress_Maps_CSV"), recursive = TRUE) #Creat a folder to save the CSV files
  sdir <- "./Water_Stress_Maps_CSV/"
  csvfile <-
    paste(sdir, list.filenames_ET0[i], '.csv', sep = "")
  write.table(
    class.df[c("Country",
               "[<0.2]",
               "[0.2 - 0.4]",
               "[0.4 - 0.6]",
               "[0.6 - 0.8]",
               "[0.8 - 1]",
               "[>1]")],
    file =  csvfile,
    sep = ",",
    row.names = F,
    col.names = T,
    quote = F
  )
  
  #Here is the individual daily tif maps
  dir.create(file.path("Water_Stress_Maps_GTiff"), recursive = TRUE) #Creat a folder to save the GTiff files
  sdir <- "./Water_Stress_Maps_GTiff/"
  
  file_tif <-
    paste(sdir, list.filenames_ET0[i], '.tif', sep = "")
  writeRaster(
    list.data_EDI[[i]],
    file = file_tif,
    format = "GTiff",
    overwrite = TRUE
  )
}

#10. Removing intermediate products before collecting the main outputs
setwd(dir)
files <- list.files(path = dir, pattern = "ET")
unlink(paste(dir, files, sep = ""))

#11. Zipping all generated output files

files_jpg <- list.files(path = dir, pattern = "jpg")
zipfile_jpg <- "Water_Stress_Maps_jpg.zip"
zip(
  zipfile_jpg,
  paste(dir, files_jpg, sep = ""),
  recurse = T,
  compression_level = 9,
  include_directories = F,
  root = ".",
  mode = c("cherry-pick")
)

files_tif <- list.files(path = dir, pattern = "GTiff")
zipfile_tif <- "Water_Stress_Maps_GTiff.zip"
zip(
  zipfile_tif,
  paste(dir, files_tif, sep = ""),
  recurse = T,
  compression_level = 9,
  include_directories = F,
  root = ".",
  mode = c("cherry-pick")
)

zipfile_csv <- "Water_Stress_Reports_CSV.zip"
files_csv <- list.files(path = dir, pattern = "CSV")
zip(
  zipfile_csv,
  paste(dir, files_csv, sep = ""),
  recurse = T,
  compression_level = 9,
  include_directories = F,
  root = ".",
  mode = c("cherry-pick")
)
