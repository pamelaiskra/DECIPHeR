#Determine members of the behavioural ensemble

#----Matrix of DynaTOP output files
dyna_files <- list.files(pattern="\\.flow$") 

#Spin-up time 
#180 for calibration, #365 for validation
spinup_days <- 180
spinup_hours<-0

#Consider spin-up time, "tail" keeps the last n values. In this case, the total - the spin-up time
disch_obs_cumecs_spinup <- tail(disch_obs_cumecs,length(disch_obs_cumecs)-(spinup_days*24+spinup_hours)) 

#----Some values to calculate NSE
durchschnitt <- mean(disch_obs_cumecs_spinup) 
Denominador <- sum((disch_obs_cumecs_spinup-durchschnitt)^2)
gauge_col<- which(grepl(gauge,(flow_conn_file$Node_ID/1000)))

#----Function that creates a dataframe of sim disch and calculates NSE
# Read simulated runoff and create dataframe of such, and calculates NSE [all disch in cumecs]

read_dyna_files <- function(matriz) { 
  DynaTOPoutputFile <- read.table(matriz)
  FlussDynaTOP <- (DynaTOPoutputFile[,gauge_col])*(area)/3600 # [m/h] to cumecs
  
  #Consider spin-up time, "tail" keeps the last n values. In this case, the total - the spin-up time
  FlussDynaTOP_spinup <- tail(FlussDynaTOP,length(FlussDynaTOP)-(spinup_days*24+spinup_hours))
  
  Numerador <- sum((disch_obs_cumecs_spinup-FlussDynaTOP_spinup)^2)
  NSE <- 1- (Numerador/Denominador)
  NSEdataframe <- rbind(data.frame(Archivo=matriz,NSEvalue=NSE))
  
  return(list(FlussDynaTOP, NSEdataframe))
}

#-------------1a) CALCULATE NSE
sss<-data.frame(lapply(dyna_files,read_dyna_files))
fin_runoff <- length(dyna_files)*3-2
subset_runoff <- seq(1, fin_runoff, by=3)
disch_sim_cumecs <- sss[,subset_runoff] #[m3/s]
disch_sim <- disch_sim_cumecs*(3600000/area) #cumecs to [mm/h]
disch_obs <- disch_obs_cumecs*(3600000/area) #cumecs to [mm/h]

colnames(disch_sim) <- dyna_files
fin_nse <- length(dyna_files)*3
subset_nse <- seq(3, fin_nse, by=3)
nse_zoo <- as.numeric(sss[1,subset_nse])
nse <- data.frame(SimName=dyna_files,NSEvalue=nse_zoo)

#-----------1b) CALCULATE RMSE
sss2<-as.numeric(lapply(dyna_files,iskra_rmse))
rmse <- data.frame(SimName=dyna_files,RMSEvalue=sss2)

#-----------2a) Extract top5 Quantile performing NSE
Quantil <- quantile(nse$NSEvalue, c(.95)) 
roro<- which(nse$NSEvalue>Quantil) 

#------------2b) Extract top 5% performing RMSE. Order RMSE low to high, takes first 5%, max value of that 5% is Max allowed RMSE
#Quantil2 <- quantile(sss2, c(.05)) 
Quantil2 <- max(head(sort(sss2),length(sss2)*0.05))
roro2<- which(sss2<=Quantil2) 

#-----------3) Final TOP 5:
rororo <- intersect(roro,roro2)
top5<-disch_sim[,rororo]

Max_rmse <- max(sss2)
Min_rmse <- min(sss2)
Percentile_05 <- Quantil2
Mean_rmse <- mean(sss2)

een <- c("Max RMSE",Max_rmse)
twee <- c("Mean RMSE",Mean_rmse)
drie <- c("Min RMSE",Min_rmse)
vier <- c("Max of 20%", Percentile_05)
vijf <- cbind(dyna_files,sss2)

rmse_array <- rbind(een,twee,drie,vier,vijf)

#----Export results (NSE values and Q95) to a .txt file
nse_file <- file.path(getwd(), sprintf("NSE_values_%s.txt",Projekttitel))
Max_value <- max(nse$NSEvalue)
nse_array<-rbind(data.frame(SimName="Percentile 95",NSEvalue=as.numeric(Quantil)),data.frame(SimName="Max value",NSEvalue=Max_value),nse)
write.table(nse_array,nse_file, sep="\t",eol = "\n", quote = FALSE, row.names=FALSE)

#------Export RMSE values
rmse_file <- file.path(getwd(), sprintf("RMSE_values_%s.txt",Projekttitel))
write.table(rmse_array,rmse_file, sep="\t",eol = "\n", quote = FALSE, row.names=FALSE)