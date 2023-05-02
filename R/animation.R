#'Create Animated GIF from the Raster Time Series
#' @description This function takes the entire raster time series and makes an animated GIF.
#' @details The functions assumes to take the output from the predict_rts() function from the GroundwatermodelR package
#' where file names include the timestamp: gwl_YYYYmmdd (gwd refers to groundwater level). For instance gwl_20201025.tif refers
#' to the interpolation raster on 2020-10-25. An option to include custom filename formats is not included yet.\cr
#' The function assumes 'magick.exe' to be present. See help("ani.options") for further details.
#' @param raster_dir Directory, where the rasters for the animation are stored. Filenames are assumed to include the
#' timestamp (see details)
#' @param raster_type File type of the input rasters stored in raster_dir. Default format is tif
#' @param out_file Path to the resulting GIF file
#' @param color_palette Color palette to use for plotting the single raster files. Any palette from the colorspace-package
#' is supported.
#' @param magick_path Parent directory of ImageMagick. Can be downloaded from https://imagemagick.org/
#' @param ... Further parameters for ani.options and saveGIF. See help("ani.options") and help("saveGIF") for further information.
#' @author Florian Betz
#' @references Betz, F; Fischer, P. (in prep.): Assessing Spatial-Temporal Dynamics of Groundwater in a Restored Floodplain at the German Danube using Machine Learning. In Preparation.
#' @return A GIF file with the animation of the raster time series as specified with 'out_file'

Animation<-function(raster_dir=NULL,
                    raster_type="tif",
                    color_palette='BluYl',
                    range=c(370,380),
                    magick_path="C:/Program Files/ImageMagick-7.0.10-Q16-HDRI",
                    out_file='animation.gif',
                    ...){

  #Load raster files from directory and make raster stack
  files<-list.files(raster_dir,pattern=paste0("*.",raster_type),full.names = TRUE)
  gwd<-terra::rast(files)

  #Get dates from the file names
  #get number of characters of the directory
  nchar_dir<-nchar(raster_dir)

  #Puzzling the dates together from the file names. This might be a potential soure of errors and should be solved differently in future
  dates<-paste(substr(files,nchar_dir-11,nchar_dir-8),
               substr(files,nchar_dir-7,nchar_dir-6),
               substr(files,nchar_dir-5,nchar_dir-4),
               sep="-")

  #Set options for the animated GIF
  oopt<-animation::ani.options(convert=paste0(magick_path,"convert.exe"),ani.height=200)

  #Create animated GIF
  animation::saveGIF(for(i in 1:nlyr(gwd)){
    plot(gwd[[i]],range=range,col=rev(colorspace::sequential_hcl(n=25,palette=color_palette)),main=dates[i])},
    nmax=nlyr(gwd),ani.pause(),movie.name = out_file)

}
