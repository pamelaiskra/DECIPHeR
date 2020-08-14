#install.packages("reshape")
#install.packages("hydroGOF")
#library(reshape)
#install.packages("hydroGOF")

rm(list=ls())
sims<-10
pasos<-20

crear <- function(mylist,cuantos){
  mylist[[cuantos]] <- runif(n = sims, min = 15, max = 55)
}

lala <- list()
for (j in 1:pasos) lala[[j]] <- crear(lala,j)

#---------------------

aa<-data.frame(do.call(rbind,lala))
aamean<-apply(aa,1,mean)
aamin<-apply(aa,1,min)
aamax<-apply(aa,1,max)
vec<-runif(n = sims, min = 0.9, max = 1.1)
vec1<-runif(n = sims, min = 0.9, max = 1.1)
vec2<-runif(n = sims, min = 0.9, max = 1.1)
vec3<-runif(n = sims, min = 0.9, max = 1.1)
vec4<-runif(n = sims, min = 0.9, max = 1.1)
vec5<-runif(n = sims, min = 0.9, max = 1.1)
vec6<-runif(n = sims, min = 0.9, max = 1.1)
vec7<-runif(n = sims, min = 0.9, max = 1.1)
vec8<-runif(n = sims, min = 0.9, max = 1.1)
vec9<-runif(n = sims, min = 0.9, max = 1.1)
vec10<-runif(n = sims, min = 0.9, max = 1.3)

aa1<-aamean*vec1
aa2<-aamean*vec2
aa3<-aamean*vec3
aa4<-aamean*vec4
aa5<-aamean*vec5
aa6<-aamean*vec6
aa7<-aamean*vec7
aa8<-aamean*vec8
aa9<-aamean*vec9
aa10<-aamean*vec10

aasim<-data.frame(aa1,aa2,aa3,aa4,aa5,aa6,aa7,aa8,aa9,aa10)

aaobs<-aamean*vec

plot(aasim[,1],type="l")
for (k in 1:sims){ 
  lines(aasim[,k])
}
lines(aaobs,col="red")

#---- calculate NSE
calc_nse <- function(matriz,m,obs=aaobs) { 
  simm <- matriz[,m]
  nse_index<-NSE(simm,obs)
  return(nse_index)
}

nse_array<-list()
for (i in 1:10) nse_array[[i]]<- calc_nse(aasim,i)
nse_array <- do.call(rbind,nse_array)

#Select those with NSE>value
valuee <- 0.35
indexx <- which(nse_array>=valuee)
aasim_nse <- aasim[,indexx]

aamin_nse<-apply(aasim_nse,1,min)
aamax_nse<-apply(aasim_nse,1,max)

aatodo<-data.frame(cbind(aamean,aamin,aamax,aamin_nse,aamax_nse))
xx<-c(1:nrow(aatodo))

ggplot(data=aatodo, aes()) +
  geom_ribbon(aes(ymin=aamin,
                ymax=aamax, 
                x=xx,
                fill = "Sim min/max"), alpha = 0.6) + #MinMax
  geom_ribbon(aes(ymin=aamin_nse,
                  ymax=aamax_nse, 
                  x=xx,
                  fill = "Sim behavioural"), alpha = 0.6) + #MinMax
  geom_line(aes(x=xx,y=aamean, colour = "Sim mean"),
            linetype="dashed", size=1) +
  geom_line(aes(x=xx,y=aaobs, colour = "Obs"),
            size=1) +
  scale_color_manual(values=c("blue", "black")) +
  scale_fill_manual(values=c("yellow","green"))


#-------------FDC
#Ahora sim = year
# timestep = month
# FDC per year
dummy<-apply(aasim,2,sort)

# Y
sorted<-dummy[c(20:1),,drop = FALSE]

totals<-nrow(aasim)
rank<-c(1:totals)

# X
prob_exceed<-rank/(totals-1)

fdc_median <- apply(sorted,1,median)

plot(x=prob_exceed,y=sorted[,1],type="l")
for (k in 2:sims){ 
  lines(x=prob_exceed,y=sorted[,k],)
}
lines(x=prob_exceed,y=fdc_median,col="red",lwd=3)

#High flows
Q5 <- quantile(fdc_median,0.95)

#Low flows
Q95 <- quantile(fdc_median,0.05)



