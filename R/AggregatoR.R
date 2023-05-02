#' Aggregation and summary statistics of time series
#' @description Helper function to bulk process of time series files; can be used to aggregate the series
#' specific season and to compute summary statistics.
#' @param x data.frame containing the time series to analyze; must inlcude a column with the timestamp
#' @param time_col Name of the column in which the information about time is stored
#' @param time_format specifies in which format the time is stored. See ?lubridate::parse_date_time() for further information
#' @param stat_col Name of the column in which the data to be aggregated or summarized is stored
#' @param statistic Statistic to use for aggregation, typically mean, median, min or max. Default is to use na.rm=TRUE when computing the statistics
#' @param summary Boolean; if TRUE, summary statistics are computed during the aggregation
#' @param season Boolean; if FALSE (the default), simple yearly aggregation is performed; if TRUE, aggregation is performed for the
#' period of the year specified by the range of days of the year given by doy
#' @param doy period defined by days of the year. Boundaries are included in the computation.
#' @param export boolean, if TRUE, the function returns a csv file containing the output.
#' @param filename filename (including path) of the output file if export=TRUE.
#' @param ... Additional parameter for the statistic, e.g. a vector of probailities when computing quantiles as statistics
#' @author Florian Betz
#' @references Betz, F; Fischer, P. (in prep.): Assessing Spatial-Temporal Dynamics of Groundwater in a Restored Floodplain at the German Danube using Machine Learning. In Preparation.
#' @return A data.frame containing the aggregated or summarized time series

aggregatoR<-function(x,
                     time_col="Date",
                     time_format="%Y-%m-%d",
                     stat_col="Time_Series",
                     statistic=mean,
                     Summary=TRUE,
                     season=FALSE,
                     doy=c(74:180),
                     export=FALSE,
                     filename=NULL,
                     ...){

  #Read time series input ot xts
  t<-xts(x[,stat_col],order.by=lubridate::parse_date_time(x[,time_col],orders=time_format))

  #Multiple Statistics/Summary Statistics
  if(Summary){
    if(season){
      t<-t[.indexyday(t) %in% doy]
      t.stat1<-xts::apply.yearly(t,mean,na.rm=TRUE)
      t.stat2<-xts::apply.yearly(t,median,na.rm=TRUE)
      t.stat3<-xts::apply.yearly(t,min,na.rm=TRUE)
      t.stat4<-xts::apply.yearly(t,max,na.rm=TRUE)
    }
    if(!season){
      t.stat1<-xts::apply.yearly(t,mean,na.rm=TRUE)
      t.stat2<-xts::apply.yearly(t,median,na.rm=TRUE)
      t.stat3<-xts::apply.yearly(t,min,na.rm=TRUE)
      t.stat4<-xts::apply.yearly(t,max,na.rm=TRUE)}

    #Write Result to data.frame
    df<-data.frame(Year=year(t.stat1),Stat1=coredata(t.stat1),Stat2=coredata(t.stat2),
                   Stat3=coredata(t.stat3),Stat4=coredata(t.stat4))
    colnames(df)<-c("Year","mean","median","min","max")
    return(df)}

  if(!Summary){
    if(season){
      t<-t[.indexyday(t) %in% doy]
      t.stat<-xts::apply.yearly(t,statistic,na.rm=TRUE)
    }
    if(!season){
      t.stat<-xts::apply.yearly(t,statistic,na.rm=TRUE)
    }
    df<-data.frame(Year=year(t.stat),Stat=coredata(t.stat))
    colnames(df)<-c("Year",as.character(substitute(statistic)))
    return(df)
  }

  #Handle export argument
  if (export) {
    write.csv(df,file = filname)

  }

  }
