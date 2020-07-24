#The 5th–95th percentile uncertainty bounds of the behavioural simulations

#behavioural: NSE values of the behavioural members of the ensemble
#(to determine behavioural ensemble, run code NSE_RMSE)

threshold <- 0.55 #If too high for actual NSE values, this is corrected later.
perci<-c(0.5,0.05,0.95) # Write the median first, then the rest of the percentiles

if (quantile(behavioural,0.2) < threshold) threshold <- quantile(behavioural,0.2)
w3 <- (behavioural >threshold) / sum((behavioural>threshold)) # normalised weights

GLUE_percentiles<- function (matrix_glue,tstep) {
  
  x3 <- as.vector(matrix_glue[tstep,]) # Values for all simulations for ONE timestep
  x3 <- x3[w3>0]     #Values>0, mean that they are assigned a probability because they have a NSE that is > threshold
  ecdf <- cumsum(rep(1/length(x3),length(x3))) # Cumulative distribution function
  sort.x3 <- sort(x3,index=TRUE)
  
  # Another way of calculating CDF:
  # ecdf <- cumsum(w3[sort.x3$ix])
  
  out_iskra <- rep(NA,length(perci)) # This is the vector with the GLUE percentiles
  
  for(perci_number in 1:length(perci)) {
    jj_alternativo <- which.min(abs(ecdf-perci[perci_number]))
    
    # jj_alternativo da la ubicación del valor más cercano al percientil. Dicho valor es ecdf[jj_alternativo]
    
    # Si el percentil es MENOR a 0.5, el percentil tiene que ser MAYOR al valor
    # Por ende, si el valor es mayor al percentil, el apuntador debe disminuir.
    if ((perci[perci_number]<=0.5) && (ecdf[jj_alternativo] > perci[perci_number])) {
      jj_alternativo <- jj_alternativo-1 
      if (jj_alternativo == 0) jj_alternativo <- 1
      
      # Si el percentil es MAYOR a 0.5, el percentil debe ser MENOR al valor
      # Por ende si el valor es menor del percentil, el apuntador debe aumentar
    } else if ((perci[perci_number]>0.5) && (ecdf[jj_alternativo] < perci[perci_number])) {
      jj_alternativo <- jj_alternativo+1 
    }
    
    out_iskra[perci_number] <- sort.x3$x[jj_alternativo]
  }
  return(out_iskra)
}

disch_sim_ptile <- matrix(NA,nrow(disch_sim),length(perci)-1) # A matrix of nans with no. timesteps x percentiles for each one
# One of them is the median which is stored separatedly...
disch_sim_median <- rep(NA,nrow(disch_sim))                   # ... here.

a_disch_sim<-as.matrix(top5) # For the sake of format

#For each time step, stores the percentiles, and then the median
for(timestep in 1:nrow(disch_sim)){
  ausser <- GLUE_percentiles(a_disch_sim ,timestep)
  disch_sim_ptile[timestep,] <- ausser[2:length(perci)]
  disch_sim_median[timestep] <- ausser[1]
}

#  So the upper and lower bounds from GLUE are given by:
top5min <- disch_sim_ptile[,1]
top5max <- disch_sim_ptile[,2]


