#Scatter plots of NSE vs parameter value

nse <- data.frame(SimName=dyna_files,NSEvalue=nse_zoo) # See code NSE_RMSE
gauge <- 23016
Projekttitel <- "GEAR_calib_Newc"
no_paramclass <- 2 # One for urban, one for non-urban

#Function to do scatter plots
NSE_scatter_plot <- function (init_col,end_col,gaugee,Projekttitell,matrix_to_be_plotted,param_class)
  for (m in init_col:end_col) {
    kth_parameter <- colz[m]
    PlotKopft4 <- sprintf("NSE vs %s, gauge %s %s, param class %s",kth_parameter, gaugee, Projekttitell,param_class)
    DateiKopft4 <- sprintf("%s.png",PlotKopft4)
    coso <- as.vector(t(subset(matrix_to_be_plotted, select=c(kth_parameter))))
    coso2 <- as.vector(t(subset(matrix_to_be_plotted, select=NSE_values)))
    png(filename = paste0(plots_folder,DateiKopft4), width = 600, height=450, units="px")
    plot(coso, coso2, type = "p", col = "black", main=PlotKopft4, pch=".", cex=3, xlab=kth_parameter, ylab="NSE values",
         cex.lab=1.5, cex.axis=1.5, cex.main=1.5) 
    dev.off()
  }

ResMatrix <- list.files(pattern=".res")
#Matrix of model parameter values VS NSE for ONE parameter class
Params_one_class <- list()
#Matrices for ALL parameter classes
Params_all_class <- list()

#Create such matrix for all parameter classes...
for (j in 1:no_paramclass) {
  #... for all files in simulation
  for (k in 1:length(ResMatrix)) {
    temp_matrix <- create_param_matrix(ResMatrix[k])
    temp_values<- temp_matrix[j,]
    Params_one_class[[k]] <- c(ResMatrix[k],temp_values)
  }
  Params_all_class[[j]] <-Params_one_class
  
  #And make scatter plots using the calculated NSE values
  Params_forplot <- data.frame(do.call(rbind,Params_one_class) , nse$NSEvalue)
  colnames(Params_forplot) <- colz
  NSE_scatter_plot(3,9,gauge,Projekttitel,Params_forplot,j)
}
