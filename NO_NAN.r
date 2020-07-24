# Eliminate NaN values in observed discharge

#Example values for Newcstle 2012 event, calibration
anfangszeit <- c(2008,2,28,1)
endzeit <- c(2012,5,31,24)
minSPUR<- c(16213,23917)
maxSPUR<- c(16524,24494)

#Initial date for hourly observed discharge. Format is "01 01 2008 01:00:00"
a_dd<-anfangszeit[3] 
a_mm<-anfangszeit[2] 
a_yy<-anfangszeit[1]
a_init_yr <- paste0(a_yy<-anfangszeit[1],"-01-01") 
a_dayofyear <- dayYear(a_dd,a_mm,a_yy)
a_reversed_date<-format(as.Date(as.Date.numeric(a_dayofyear-1, origin=a_init_yr)), "%d %m %Y")
a_plot<-format(as.Date(as.Date.numeric(a_dayofyear-1, origin=a_init_yr)), "%d %m %Y")
anfangszeit_disH<-paste0(a_reversed_date," 01:00:00")

#End date for hourly observed discharge. Format is "01 01 2008 00:00:00" (following day)
#Note: endzeit_disH will be one day after sim finishes. This will be corrected later.
e_dd<-endzeit[3] 
e_mm<-endzeit[2] 
e_yy<-endzeit[1]
e_init_yr <- paste0(e_yy<-endzeit[1],"-01-01") 
e_dayofyear <- dayYear(e_dd,e_mm,e_yy)
e_reversed_date<-format(as.Date(as.Date.numeric(e_dayofyear, origin=e_init_yr)), "%d %m %Y")
endzeit_disH<-paste0(e_reversed_date," 00:00:00")

#Initial date for daily observed discharge. Format is "01/01/2008"
aa_dd<-anfangszeit[3] 
aa_mm<-anfangszeit[2] 
aa_yy<-anfangszeit[1]
aa_init_yr <- paste0(aa_yy<-anfangszeit[1],"-01-01") 
aa_dayofyear <- dayYear(aa_dd,aa_mm,aa_yy)
anfangszeit_disD<-format(as.Date(as.Date.numeric(aa_dayofyear-1, origin=aa_init_yr)), "%d/%m/%Y")

#End date for daily obs discharge. Format is "02/01/2008"
ee_dd<-endzeit[3] 
ee_mm<-endzeit[2] 
ee_yy<-endzeit[1]
ee_init_yr <- paste0(ee_yy<-endzeit[1],"-01-01") 
ee_dayofyear <- dayYear(ee_dd,ee_mm,ee_yy)
endzeit_disD<-format(as.Date(as.Date.numeric(ee_dayofyear-1, origin=ee_init_yr)), "%d/%m/%Y")

#---Hourly observed discharge without NaNs
colnames(disch_raw) <- c("Date", "DischCumecs")
disch_raw <- disch_raw[c(which(grepl(anfangszeit_disH, disch_raw$Date)):(which(grepl(endzeit_disH, disch_raw$Date)))),]
row.names(disch_raw) <- c(1:nrow(disch_raw))

if (any(is.na(disch_raw$DischCumecs))==TRUE){ #if there are any NaN values in raw file of hourly discharge
  
  print("NaN values found in hourly discharge time series")
  #  at: as.vector(disch_obs_nan$Date)
  #Extract dates with NaNs from observed discharge and add a column with row numbers
  disch_obs_nan <- disch_raw[which(is.na(disch_raw$DischCumecs)),]
  disch_obs_nan$ZeilName <- rownames(disch_obs_nan)
  eins <- strsplit(as.character(disch_obs_nan$Date), ' ')
  nan_datum <- do.call(rbind, eins)
  nan_datum <- as.data.frame.matrix(nan_datum[,c(1,2,3)]) # column 4 is hours hh:mm:ss
  nan_datum <- paste(nan_datum$V3, nan_datum$V2, nan_datum$V1, sep="-")                    #Careful here with sep=nrfa file
  #V1 is day, V2 is month, V3 year                                           
  #Create to data frame with dates and discharge that will replace NaNs
  nandf <- data.frame(Date=nan_datum, DischToFill=c(1:length(nan_datum))) #fills with dummy variable
  
  for (y in 1:nrow(nandf)){
    roww <- which(nan_datum[y]==werte$data) # dates of missing data
    nandf$DischToFill[y] <-werte$last[roww] # filled missing data
  }
  
  #Transfer values from dataframe showing rows that have NaNs into those rows of the Observed Discharge dataframe
  for (w in 1:nrow(werte)){
    rowww <- as.numeric(disch_obs_nan$ZeilName[w])
    valor <- nandf$DischToFill[w]
    disch_raw$DischCumecs[rowww] <- valor
    disch_obs_cumecs<-disch_raw$DischCumecs
  }
  rm(nandf,werte,eins, roww, rowww, valor, w, y)
  print("NaN values filled with daily discharge data")
} else{
  disch_obs_cumecs <- disch_raw$DischCumecs
}

#Is there spurious data that have to be removed?
if (esgibt_sp== "YES") {
  #Delete spurious data
  dummy_spurious<-list()
  for (gg in 1:length(minSPUR)){
    dummy_spurious[[gg]] <-minSPUR[gg]:maxSPUR[gg]
  }
  
  SPURIOUS<-unlist(dummy_spurious,use.names = FALSE)
  disch_obs_cumecs<-disch_obs_cumecs[-SPURIOUS] #------------------------------------SPURIOUS DATA
}

#Consider spin-up time, "tail" keeps the last n values. In this case, the total - the spin-up time
disch_obs_cumecs_spinup <- tail(disch_obs_cumecs,length(disch_obs_cumecs)-(spinup_days*24+spinup_hours)) 



