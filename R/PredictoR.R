#' Make predictions for new timesteps using existing model
#' @description This function uses an existing interpolation model
#' (e.g. trained using the TraineR function) and makes predictions for new timesteps
#' @param model Path to the existing model stored as .rds file
#' @param spat_predictors Raster file containing the spatial predictors of the model; has to be the same as used for model training
#' @param temp_predictors Data.frame containing the time series input for prediction where each row refers to one timestep
#' @param date_column Name of the column where the time information is stored in the table of temporal predictors; default is Date
#' @param date_format How the time information is formatted in date_column; please refer to as.Date() for
#' @param out_basename Basename of the output filenames. Default is "gwl" as this basename is used in the other functions of GroundwatermodelR
#' @param out_dir Directory where to store the raster files created during prediction
#' @param ncores number of cores to use during multicore prediction; default is one (i.e. no parallel processing), the number of
#' available on your machine can be estimated using parellel::detectCores()
#' @param ... Additional parameter of terra::predict
#' @author Florian Betz
#' @references Betz, F; Fischer, P. (in prep.): Assessing Spatial-Temporal Dynamics of Groundwater in a Restored Floodplain at the German Danube using Machine Learning. In Preparation.
#' @return One raster file for each timestep in the temporal predictor stored in out_dir. The filename includes a timestamp.

predictoR<-function(model=NULL,
                    spat_predictors=NULL,
                    temp_predictors=NULL,
                    date_column=Date,
                    date_format="%Y-%m-%d",
                    out_basename="gwl",
                    out_format="tif",
                    out_dir=NULL,
                    ncores=detectCores(),
                    ...){

  #Read model from file
  model<-readRDS(model)

  #Get stack of predictor rasters
  predictors<-rast(spat_predictors)

  #Get dates from the input table with the constant values
  date_vector<-as.Date(temp_predictors$date_column,format=date_format)

  #Throw error in case the date handling is not correct.
  if (!is.Date(date_vector)) {
  stop("Please check if date_column and date_format are specified correctly")
  }

  #Get names for writing the output
  names<-paste0(out_basename,"_",stringr::str_remove_all(as.character(date_vector),"[-]"),".",out_format)

  #Get current working directory (to restore later) and set it to out_dir
  temp<-getwd()
  setwd(out_dir)

  #Make prediction
  for (i in 1:nrow(const_predictors)){
    message(paste("Prediction", as.character(i),"of",as.character(nrow(const_predictors),sep=" ")))
    terra::predict(object=spat_predictors,model=model,const=temp_predictors[i,],filename=names[i],cores=ncores)
    }

  #Restore working directory
  setwd(temp)
  rm(temp)
}
