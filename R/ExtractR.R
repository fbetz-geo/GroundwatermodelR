#' Handles the resulting raster time series and extracts time series for specific locations
#' @description This function is designed to work with the resulting raster time series and extract single time series as
#' table. It implements the idea of 'virtual gauges' described in Betz and Fischer (2023).
#' @details The functions assumes to take the output from the predictoR() function from the GroundwatermodelR package
#' where file names include the timestamp: gwl_YYYYmmdd (gwd refers to groundwater level). For instance gwl_20201025.tif refers
#' to the interpolation raster on 2020-10-25. An option to include custom filename formats is not included yet.\cr
#' The function can take points or polygons as input. In case of points, simply the values at the point location are returned.
#' In case of polygons, the function returns basic statistics for each time step of the values within the polygon. The
#' GroundwatermodelR package offers the function plotfindR to convert polygons to points.\cr
#' The function has two output options. Using out_type="df", it returns a data.frame containing all values in long format. If
#' out_type="export", the functions returns one .csv file for each location and stores it in the directory specified with out_dir.
#' @param raster_dir Directory, where the rasters are stored. Filenames are assumed to include the
#' timestamp (see details)
#' @param loc path to any sf-compatible file containing either points or polyons for which to extract and summarize raster time series
#' @param out_dir The directory where to write the tables resulting from each location
#' @param out_type Create data.frame as output or write it to files. If "df", the function returns a data.frame in long format,
#' if out_type="export", the function exports one single csv-file per location to the directory specified in out_dir.
#' @param raster_type File type of the input rasters stored in raster_dir. Default format is tif
#' @param name_col Name of the attribute column in loc where the name or id of the location is stored. This can refer to a full
#' name or be simply an id. During the extraction, it is used to link the raster data clearly to the location.
#' @param dec the string to use for decimal points when writing the output to file. Default is "," as expected by MS Excel.
#' @return A data.frame or csv files written to out_dir
#' @author Florian Betz
#' @references Betz, F; Fischer, P. (in prep.): Assessing Spatial-Temporal Dynamics of Groundwater in a Restored Floodplain at the German Danube using Machine Learning. In Preparation.

extractR<-function(raster_dir=NULL,
                  loc=NULL,
                  out_dir=NULL,
                  out_type="df",
                  raster_type="tif",
                  name_col=Name,
                  dec=","){

  #Load rasters
  files<-list.files(raster_dir,pattern=paste0("*.",raster_type),full.names = TRUE)
  gwl<-rast(files)

  #Load vector
  loc<-st_read(loc)

  #Get the dates from the file names. This depends on proper file naming and might be a potential source of errors
  dates<-paste(substr(files,nchar_dir-11,nchar_dir-8),
         substr(files,nchar_dir-7,nchar_dir-6),
         substr(files,nchar_dir-5,nchar_dir-4),
         sep="-")

  #Simple case: loc are points, then extraction is straightforward
  if (all(sf::st_is(loc,"POINT"))) {
    #Extract the values from the rasters
    df_list<-list()

    for (i in 1:nrow(loc)) {

      #Get data.frame for single location
      df<-terra::extract(gwl,loc[i]) %>% t() %>% as.data.frame() %>%
        mutate(Name=loc$name_col[i])
      df_list[[i]]<-df

    }

    #Data.frame mode
    if (out_type=="df") {
      df<-do.call(rbind,df_list)
      return(df)
    }

    #Export mode
    if (out_type=="export") {
      filenames<-paste0(out_dir,"/",as.character(loc$name_col),".csv")

      for (i in 1:length(df_list)) {
        write.csv(file_list[[i]],file = filenames[i],quote=FALSE,sep=";",dec=dec,row.names = FALSE)

      }}
  }


  if (all(sf::st_is(loc,"POLYGON"))) {

    #Create empty list to be filled while looping over the locations
    df_list<-list()

    #For loop over the locations to extract the data and compute the statistics
    for (i in 1:nrow(loc)) {

      #Extract the data and get summary statistics
      df_min<-terra::extract(gwl,loc[i],fun=min,na.rm=TRUE) %>% t() %>% as.data.frame() %>%
        tibble::add_column(Date=dates,.before = 1)
      df_mean<-terra::extract(gwl,loc[i],fun=mean,na.rm=TRUE) %>% t() %>% as.data.frame() %>%
        tibble::add_column(Date=dates,.before = 1)
      df_med<-terra::extract(gwl,loc[i],fun=median,na.rm=TRUE) %>% t() %>% as.data.frame() %>%
        tibble::add_column(Date=dates,.before = 1)
      df_max<-terra::extract(gwl,loc[i],fun=max,na.rm=TRUE) %>% t() %>% as.data.frame() %>%
        tibble::add_column(Date=dates,.before = 1)

      #Merge the different tables to one table
      df<-dplyr::left_join(df_min,df_mean) %>% dplyr::left_join(df_med) %>% dplyr::left_join(df_max)

      df_list[[i]]<-df

    }

    #Output in data.frame mode
    if (out_type=="df") {
      df<-do.call(rbind,df_list)
      return(df)

    }

    #Output in export mode
    if (out_type=="export") {

      #Define output file names
      filenames<-paste0(out_dir,"/",as.character(loc$name_col),".csv")

      #Loop over the list entrances to write the data.frames to files
      for (i in 1:length(df_list)) {
        write.csv(file_list[[i]],file = filenames[i],quote=FALSE,sep=";",dec=dec,row.names = FALSE)

    }}

    }
}
