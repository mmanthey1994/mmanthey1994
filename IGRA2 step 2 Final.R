
#Need to restrict domain of data matrix since there are no obs in first few years for the stations selected...
#in moving average filtering need to leave known data points as they are...

options(digits = 3)
####################################################################################################
####################################################################################################
#Loading data and installing packages
####################################################################################################
####################################################################################################
#install.packages("urca")
library("urca")

#install.packages("tseries")
library(cleanIGRA)
library(magrittr)
library(dplyr)

library(tseries)
#install.packages("forecast")
library("forecast")
#CHECK THE DIMENSIONS OF OUR DATA FROM MARCH 22 UP TO DATE DATA COERSION FILE...

dim(All_data_matrix)
par(mfrow=c(3,5))
for(i in 1:210){plot(All_data_matrix[i,],ylim=c(-90,10),main=i)}

period=365*2


longest_num_obs= dim(All_data_matrix)[2]

longest_num_obs
#this needs to be reset manually based on how many stations we consider...
NUM_STATIONS=4
#reset manually 

AllTimes=seq(1,dim(All_data_matrix)[2]/2,by=0.5)
AllTimes
length(AllTimes)

data_matrix=All_data_matrix[c(136:150,151:165,166:180,196:210),]
data_matrix[data_matrix==0]=NA

par(mfrow=c(3,5))


#removing sequences of only NAs...
#also making a vector to keep track of which were removed
removed_station_vector_initial=rowSums(is.na(data_matrix)) != ncol(data_matrix)
removed_station_vector_initial

before=dim(data_matrix)[1]
data_matrix=data_matrix[rowSums(is.na(data_matrix)) != ncol(data_matrix), ]
after=dim(data_matrix)[1]

num_removals=before-after
num_removals


for(i in 1:(NUM_STATIONS*15-num_removals)){
  plot(data_matrix[i,])
}
 
##############################################################################
######################################
#Allocating to a test set here
######################################
######################################
known_data_matrix=data_matrix
######################################
#artificially removed data  
######################################

#dim(data_matrix)
#sample(1:dim(data_matrix)[2],4500)
#data_matrix[1,sample(1:dim(data_matrix)[2],9000*.2)]=NA
#data_matrix[2,sample(1:dim(data_matrix)[2],9000*.2)]=NA
#data_matrix[3,sample(1:dim(data_matrix)[2],9000*.2)]=NA
#data_matrix[4,sample(1:dim(data_matrix)[2],9000*.2)]=NA

#We take one years worth of samples (possibly NA) across the data matrix at random locations and store them in a testing set, then delete this observation
Test_SET_locations_matrix=matrix(NA,((NUM_STATIONS*15)-num_removals),period)



for(i in 1:((NUM_STATIONS*15)-num_removals)){
  Test_SET_locations_matrix[i,]=sample(1:dim(data_matrix)[2],period)
}

Test_SET_matrix_1_Period=matrix(NA,((NUM_STATIONS*15)-num_removals),period)

for(i in 1:((NUM_STATIONS*15)-num_removals)){
  Test_SET_matrix_1_Period[i,]=known_data_matrix[i,Test_SET_locations_matrix[i,]]
}


for(i in 1:((NUM_STATIONS*15)-num_removals)){
  data_matrix[i,Test_SET_locations_matrix[i,]]=NA
}

#######################################



####################################################################################################
#################################################################################################### 
####################################################################################################




#We coduct some tests TO SEE IF ORIGINAL NON-DETRENDED DATA IS (trend) STATIONARY (null is stationary and nonstationary respectively) 
#NOTE, ADF and KPSS are based on RW+linear deterministic trends, so will not pick up seasonal nonstationarity... 
#In reality, it is obvious there is seasonal non-stationarity, so we will always have to detrend in this respect...
#after this detrending we can then check for residual trend non-stationarity 
#test#1
?ur.kpss


summary(ur.kpss(data_matrix[1,],type="mu",use.lag=0))
summary(ur.kpss(data_matrix[1,],type="tau",use.lag=0))

summary(ur.kpss(data_matrix[1,],type="mu",use.lag=1))
summary(ur.kpss(data_matrix[1,],type="tau",use.lag=1))

summary(ur.kpss(na.omit(data_matrix[1,]),type="mu",use.lag=5))
summary(ur.kpss(na.omit(data_matrix[1,]),type="tau",use.lag=5))

kpss.test(na.omit(data_matrix[1,]),null="Trend",lshort=TRUE)


#generally REJECT null (that series is stationary) SO WE DO NEED TO DETREND/DIFFERENCE...
#test#2 
adf.test(na.omit(data_matrix[4,]),k=1)
?adf.test
#reject null (that series is nonstationary) CONTRADICTORY...HOWEVER, NOTE WE HAD TO REMOVE NA VALUES AND HENCE NULL HYPOTHESIS OF NON-STATIONARITY IS INHERENTLY VIOLATED...

#Use a kpss test assuming stationary around a trend, generally we accept the hypothesis that there is trend-stationarity!!
kpss_p_values_raw_data=rep(NA,(NUM_STATIONS*15-num_removals))
for(i in 1:(NUM_STATIONS*15-num_removals)){kpss_p_values_raw_data[i]=kpss.test(na.omit(data_matrix[i,]),null="Trend",lshort=TRUE)[[3]]
}
kpss_p_values_raw_data


####################################################################################################
####################################################################################################
#Removing linear and periodic deterministic trend with LM
####################################################################################################
####################################################################################################
library("urca")

#install.packages("tseries")
library(cleanIGRA)
library(magrittr)
library(dplyr)

library(tseries)
library("optimbase")
library("wordspace")
data_matrix[data_matrix==0]=NA
trend_removed_data_matrix_COMPLICATED_LM=data_matrix


phase_vectors=rep(NA,NUM_STATIONS*15-num_removals)

for(i in 1:(NUM_STATIONS*15-num_removals)){
  t=1:longest_num_obs
  w=2*pi/365.25/2 #we divide by two again because day and night data included!!
  
  cosine_series=cos(w*t)
  sine_series=sin(w*t)
  cosine_series2=cos(2*w*t)
  sine_series2=sin(2*w*t)
  cosine_series3=cos(3*w*t)
  sine_series3=sin(3*w*t)
  cosine_series4=cos(4*w*t)
  sine_series4=sin(4*w*t)
  #cosine_series5=cos(5*w*t)
  #sine_series5=sin(5*w*t)
  #cosine_series6=cos(6*w*t)
  #sine_series6=sin(6*w*t)
  #cosine_series7=cos(7*w*t)
  #sine_series7=sin(7*w*t)
  #cosine_series8=cos(8*w*t)
  #sine_series8=sin(8*w*t)
  #cosine_series9=cos(9*w*t)
  #sine_series9=sin(9*w*t)
  #cosine_series10=cos(10*w*t)
  #sine_series10=sin(10*w*t)
  
  
  model1=lm(data_matrix[i,]~t+cosine_series+sine_series+cosine_series2+sine_series2+cosine_series3+sine_series3+cosine_series4+sine_series4)
  
  
  trend_removed_data_matrix_COMPLICATED_LM[i,!is.na(data_matrix[i,])]=trend_removed_data_matrix_COMPLICATED_LM[i,!is.na(data_matrix[i,])]-(model1$coefficients[1]+model1$coefficients[2]*t+model1$coefficients[3]*cosine_series+model1$coefficients[4]*sine_series+model1$coefficients[5]*cosine_series2+model1$coefficients[6]*sine_series2+model1$coefficients[7]*cosine_series3+model1$coefficients[8]*sine_series3+model1$coefficients[9]*cosine_series4+model1$coefficients[10]*sine_series4)[!is.na(trend_removed_data_matrix_COMPLICATED_LM[i,])]
  
  phase_vectors[i]=atan(model1$coefficients[4]/model1$coefficients[3])
  
  plot((model1$coefficients[1]+model1$coefficients[2]*t+model1$coefficients[3]*cosine_series+model1$coefficients[4]*sine_series+model1$coefficients[5]*cosine_series2+model1$coefficients[6]*sine_series2+model1$coefficients[7]*cosine_series3+model1$coefficients[8]*sine_series3+model1$coefficients[9]*cosine_series4+model1$coefficients[10]*sine_series4),col="red",ylim=c(-100,0))
  
  par(new=TRUE)
  plot(data_matrix[i,],ylim=c(-100,0))
  
  
  #model1=lm(data_matrix[i,]~t+cosine_series+sine_series+cosine_series2+sine_series2+cosine_series3+sine_series3+cosine_series4+sine_series4+cosine_series5+sine_series5+cosine_series6+sine_series6+cosine_series7+sine_series7+cosine_series8+sine_series8+cosine_series9+sine_series9+cosine_series10+sine_series10)
  
  
  #trend_removed_data_matrix_COMPLICATED_LM[i,!is.na(data_matrix[i,])]=trend_removed_data_matrix_COMPLICATED_LM[i,!is.na(data_matrix[i,])]-(model1$coefficients[1]+model1$coefficients[2]*t+model1$coefficients[3]*cosine_series+model1$coefficients[4]*sine_series+model1$coefficients[5]*cosine_series2+model1$coefficients[6]*sine_series2+model1$coefficients[7]*cosine_series3+model1$coefficients[8]*sine_series3+model1$coefficients[9]*cosine_series4+model1$coefficients[10]*sine_series4+model1$coefficients[11]*cosine_series5+model1$coefficients[12]*sine_series5+model1$coefficients[13]*cosine_series6+model1$coefficients[14]*sine_series6+model1$coefficients[15]*cosine_series7+model1$coefficients[16]*sine_series7+model1$coefficients[17]*cosine_series8+model1$coefficients[18]*sine_series8+model1$coefficients[19]*cosine_series9+model1$coefficients[20]*sine_series9+model1$coefficients[21]*cosine_series10+model1$coefficients[22]*sine_series10)[!is.na(trend_removed_data_matrix_COMPLICATED_LM[i,])]
  
  #  plot((model1$coefficients[1]+model1$coefficients[2]*t+model1$coefficients[3]*cosine_series+model1$coefficients[4]*sine_series+model1$coefficients[5]*cosine_series2+model1$coefficients[6]*sine_series2+model1$coefficients[7]*cosine_series3+model1$coefficients[8]*sine_series3+model1$coefficients[9]*cosine_series4+model1$coefficients[10]*sine_series4+model1$coefficients[11]*cosine_series5+model1$coefficients[12]*sine_series5+model1$coefficients[13]*cosine_series6+model1$coefficients[14]*sine_series6+model1$coefficients[15]*cosine_series7+model1$coefficients[16]*sine_series7+model1$coefficients[17]*cosine_series8+model1$coefficients[18]*sine_series8+model1$coefficients[19]*cosine_series9+model1$coefficients[20]*sine_series9+model1$coefficients[21]*cosine_series10+model1$coefficients[22]*sine_series10),col="red")
  
  # par(new=TRUE)
  #plot(data_matrix[i,])
}
#We see that our timeseries are visually out of phase??! at least in first component of fourier decomposition...
phase_vectors=phase_vectors*180/pi 
phase_vectors
atan(1)*180/pi
atan(180)
plot(model1$coefficients[1]+model1$coefficients[2]*t+model1$coefficients[3]*cosine_series+model1$coefficients[4]*sine_series+model1$coefficients[5]*cosine_series2+model1$coefficients[6]*sine_series2+model1$coefficients[7]*cosine_series3+model1$coefficients[8]*sine_series3+model1$coefficients[9]*cosine_series4+model1$coefficients[10]*sine_series4,xlim=c(1,4000))

#plot(model1$coefficients[1]+model1$coefficients[2]*t+model1$coefficients[3]*cosine_series+model1$coefficients[4]*sine_series+model1$coefficients[5]*cosine_series2+model1$coefficients[6]*sine_series2+model1$coefficients[7]*cosine_series3+model1$coefficients[8]*sine_series3+model1$coefficients[9]*cosine_series4+model1$coefficients[10]*sine_series4+model1$coefficients[11]*cosine_series5+model1$coefficients[12]*sine_series5+model1$coefficients[13]*cosine_series6+model1$coefficients[14]*sine_series6+model1$coefficients[15]*cosine_series7+model1$coefficients[16]*sine_series7+model1$coefficients[17]*cosine_series8+model1$coefficients[18]*sine_series8+model1$coefficients[19]*cosine_series9+model1$coefficients[20]*sine_series9+model1$coefficients[21]*cosine_series10+model1$coefficients[22]*sine_series10,xlim=c(1,4000))
#We see that complicated lm someties seems to have too large an amplitude??


#checking fit....
par(mfrow=c(2,2))

for(i in 1:(NUM_STATIONS*15-num_removals)){
  t=1:longest_num_obs
  w=2*pi/365.25/2 #we divide by two again because day and night data included!!
  
  cosine_series=cos(w*t)
  sine_series=sin(w*t)
  cosine_series2=cos(2*w*t)
  sine_series2=sin(2*w*t)
  cosine_series3=cos(3*w*t)
  sine_series3=sin(3*w*t)
  cosine_series4=cos(4*w*t)
  sine_series4=sin(4*w*t)
  #cosine_series5=cos(5*w*t)
  #sine_series5=sin(5*w*t)
  #cosine_series6=cos(6*w*t)
  #sine_series6=sin(6*w*t)
  #cosine_series7=cos(7*w*t)
  #sine_series7=sin(7*w*t)
  #cosine_series8=cos(8*w*t)
  #sine_series8=sin(8*w*t)
  #cosine_series9=cos(9*w*t)
  #sine_series9=sin(9*w*t)
  #cosine_series10=cos(10*w*t)
  #sine_series10=sin(10*w*t)
  
  
  model1=lm(data_matrix[i,]~t+cosine_series+sine_series+cosine_series2+sine_series2+cosine_series3+sine_series3+cosine_series4+sine_series4)
  
  plot(model1,main=i) 
  
  #model1=lm(data_matrix[i,]~t+cosine_series+sine_series+cosine_series2+sine_series2+cosine_series3+sine_series3+cosine_series4+sine_series4+cosine_series5+sine_series5+cosine_series6+sine_series6+cosine_series7+sine_series7+cosine_series8+sine_series8+cosine_series9+sine_series9+cosine_series10+sine_series10)
  
  #plot(model1) 
  
}

############################################################################################################################
############################################################################################################################
#visual inspection for our detrendig and for stationarity
#Below we plot original (relatively dense dataset) vs. detrended data
par(new=FALSE)
par(mfrow=c(3,2))

for(i in 1:(NUM_STATIONS*15-num_removals)){
  plot(data_matrix[i,],type="l",ylim=c(-150,10),main=i)
  
  plot(trend_removed_data_matrix_COMPLICATED_LM[i,],type="l",ylim=c(-150,10),main=i)
  
}


for(i in 1:(NUM_STATIONS*15-num_removals)){
  plot(data_matrix[i,],type="l",main=i,xlim=c(38000,38100))
  
  plot(trend_removed_data_matrix_COMPLICATED_LM[i,],type="l",main=i,xlim=c(38000,38100))
  
}


#overalapping plots to check for phase
for(i in 1:(NUM_STATIONS*15-num_removals-1)){
  plot(data_matrix[i,],type="l",ylim=c(-150,10),main=i,xlim=c(35000,38000))
  par(new=TRUE)
  plot(data_matrix[i+1,],type="l",ylim=c(-150,10),main=i,xlim=c(35000,38000),col="red")
  
}

for(i in 1:(NUM_STATIONS*15-num_removals-1)){
  plot(data_matrix[i,],type="l",ylim=c(-150,10),main=i)
  par(new=TRUE)
  plot(data_matrix[i+1,],type="l",ylim=c(-150,10),main=i,col="red")
  
}


############################################################################################################################
############################################################################################################################
#Univariate stationarity tests
############################################################################################################################
############################################################################################################################



#performing two popular tests... (null is stationary and nonstationary respectively)  
#test#1
test_stats_trend_removed_complicated_LM=rep(NA,(NUM_STATIONS*15-num_removals))
for(i in 1:(NUM_STATIONS*15-num_removals)){test_stats_trend_removed_complicated_LM[i]=summary(ur.kpss(trend_removed_data_matrix_COMPLICATED_LM[i,],lags = "nil",type = "mu"))@teststat}
test_stats_trend_removed_complicated_LM
summary(ur.kpss(trend_removed_data_matrix_COMPLICATED_LM[i,],lags = "nil",type = "mu"))
?ur.kpss()
#Note 0.463 is the 5% Confidence level cutoff
#THis seems to do a better job than STL??!
#NOTE-we use a mu test as we're assuming we removed linear term with lm...

#IF we use more lags we achieve:
test_stats_trend_removed_complicated_LM_WITH_LAGS=rep(NA,(NUM_STATIONS*15-num_removals))
for(i in 1:(NUM_STATIONS*15-num_removals)){test_stats_trend_removed_complicated_LM_WITH_LAGS[i]=summary(ur.kpss(trend_removed_data_matrix_COMPLICATED_LM[i,],use.lag=1, type = "mu"))@teststat}
test_stats_trend_removed_complicated_LM_WITH_LAGS
#many are stationary with just 1 lag :) :) :) 



test_stats_trend_removed_complicated_LM_WITH_MANY_LAGS=rep(NA,(NUM_STATIONS*15-num_removals))
for(i in 1:(NUM_STATIONS*15-num_removals)){test_stats_trend_removed_complicated_LM_WITH_MANY_LAGS[i]=summary(ur.kpss(trend_removed_data_matrix_COMPLICATED_LM[i,], type = "mu"))@teststat}
test_stats_trend_removed_complicated_LM_WITH_MANY_LAGS
#mostly accept stationarity 

#what does ADF have to say? NOTE lag order has been set to 1
test_stats_trend_removed_complicated_LM_ADF=rep(NA,(NUM_STATIONS*15-num_removals))
for(i in 1:(NUM_STATIONS*15-num_removals)){test_stats_trend_removed_complicated_LM_ADF[i]=adf.test(na.omit(trend_removed_data_matrix_COMPLICATED_LM[i,],k=1))[[3]]}
test_stats_trend_removed_complicated_LM_ADF#THEY ALL INDICATE STATIONARY TOO!!
#hence if we difference the data an appropriate number of times we should be confident of stationarity



?ca.jo()



#but does it remove periodicity?? We can't check properly but make NAs zero and then do an acf... should still see basterdised peaks
trend_removed_data_matrix_COMPLICATED_LM[is.na(trend_removed_data_matrix_COMPLICATED_LM)]=0
for(i in 1:(NUM_STATIONS*15-num_removals)) { 
  plot(acf(trend_removed_data_matrix_COMPLICATED_LM[i,],lag.max = 1600),main=i)
}

#GENERALLY DOES A BETTER JOB AT REMOVING PERIODICITY??!  


############################################
#we use PACF to see if an AR model is appropriate for residuals...
#############################################
pacf(trend_removed_data_matrix_COMPLICATED_LM[1,],lag.max = 1000)
pacf(trend_removed_data_matrix_COMPLICATED_LM[1,],lag.max = 30)
#we see strong evidense of autoregressive behaviour!!
##################################################







############################################
############################################
#Finding and scaling by stratified variance/variance function
#############################################
#############################################
#library(fda)

trend_removed_data_matrix_COMPLICATED_LM[trend_removed_data_matrix_COMPLICATED_LM==0]=NA
period=365*2
num_times=longest_num_obs
num_basis_elts=10
NUM_STATIONS
#daybasis65 <- create.fourier.basis(rangeval=c(0, period), nbasis=num_basis_elts)
#day.5
####################################
#cutting up observations into slices
####################################
trend_removed_data_matrix_COMPLICATED_LM_LIST=list()
for(i in 1:(NUM_STATIONS*15-num_removals)){trend_removed_data_matrix_COMPLICATED_LM_LIST[[i]]=matrix(rep(NA,floor(num_times/period)*period),floor(num_times/period),period)}

for(i in 1:(NUM_STATIONS*15-num_removals)){
  
  for(j in 1:(num_times/period)){
    trend_removed_data_matrix_COMPLICATED_LM_LIST[[i]][j,]=trend_removed_data_matrix_COMPLICATED_LM[i,((period*(j-1)+1):(period*j))]
  }
  
}


####################################
#daily stratified standard errors - this works best!! For dense data... hence use fda SMOOTHING function after... 
####################################
Daily_std_error_est_list=list()
for(k in 1:(NUM_STATIONS*15-num_removals)){
  Daily_std_error_est_list[[k]]=rep(NA,period)
  
}



for(k in 1:(NUM_STATIONS*15-num_removals)){
  for(i in 1:period){
    Daily_std_error_est_list[[k]][i]=sd(trend_removed_data_matrix_COMPLICATED_LM_LIST[[k]][,i],na.rm = TRUE)
  }
}


plot(Daily_std_error_est_list[[1]])
#plot(Daily_std_error_est_list[[2]])
#plot(Daily_std_error_est_list[[13]])
#plot(Daily_std_error_est_list[[14]])

#scaling residual series... 
#1:6/1:2
par(mfrow=c(2,1))
##plot(trend_removed_data_matrix_COMPLICATED_LM[1,]/Daily_std_error_est_list[[1]])
#plot(trend_removed_data_matrix_COMPLICATED_LM[1,]/Daily_std_error_est_list[[1]],type="l")
##plot(trend_removed_data_matrix_COMPLICATED_LM[1,])
#plot(trend_removed_data_matrix_COMPLICATED_LM[1,],type='l')

##plot(trend_removed_data_matrix_COMPLICATED_LM[2,]/Daily_std_error_est_list[[2]])
#plot(trend_removed_data_matrix_COMPLICATED_LM[2,]/Daily_std_error_est_list[[2]],type="l")
##plot(trend_removed_data_matrix_COMPLICATED_LM[2,])
#plot(trend_removed_data_matrix_COMPLICATED_LM[2,],type='l')

##plot(trend_removed_data_matrix_COMPLICATED_LM[3,]/Daily_std_error_est_list[[3]])
#plot(trend_removed_data_matrix_COMPLICATED_LM[3,]/Daily_std_error_est_list[[3]],type="l")
##plot(trend_removed_data_matrix_COMPLICATED_LM[3,])
#plot(trend_removed_data_matrix_COMPLICATED_LM[3,],type='l')

##plot(trend_removed_data_matrix_COMPLICATED_LM[4,]/Daily_std_error_est_list[[4]])
#plot(trend_removed_data_matrix_COMPLICATED_LM[4,]/Daily_std_error_est_list[[4]],type="l")
##plot(trend_removed_data_matrix_COMPLICATED_LM[4,])
#plot(trend_removed_data_matrix_COMPLICATED_LM[4,],type='l')


#library("zoo")


#for(k in 1:(NUM_STATIONS*15-num_removals)){
#plot((Daily_std_error_est_list[[k]]))
#}

Smoothed_std_error_curves=list()

for(i in 1:(NUM_STATIONS*15-num_removals)){
  t=1:period
  w=2*pi/365.25/2 #we divide by two again because day and night data included!!
  
  cosine_series=cos(w*t)
  sine_series=sin(w*t)
  cosine_series2=cos(2*w*t)
  sine_series2=sin(2*w*t)
  cosine_series3=cos(3*w*t)
  sine_series3=sin(3*w*t)
  cosine_series4=cos(4*w*t)
  sine_series4=sin(4*w*t)
  
  
  model1=lm(Daily_std_error_est_list[[i]]~cosine_series+sine_series+cosine_series2+sine_series2+cosine_series3+sine_series3+cosine_series4+sine_series4)
  Smoothed_std_error_curves[[i]]=model1$coefficients[1]+model1$coefficients[2]*cosine_series+model1$coefficients[3]*sine_series+model1$coefficients[4]*cosine_series2+model1$coefficients[5]*sine_series2+model1$coefficients[6]*cosine_series3+model1$coefficients[7]*sine_series3+model1$coefficients[8]*cosine_series4+model1$coefficients[9]*sine_series4
  
  
}




for(k in 1:(NUM_STATIONS*15-num_removals)){
  plot(Smoothed_std_error_curves[[k]])
}


trend_removed_data_matrix_COMPLICATED_LM_Scaled=trend_removed_data_matrix_COMPLICATED_LM 
trend_removed_data_matrix_COMPLICATED_LM_Scaled[]=NA

for(k in 1:(NUM_STATIONS*15-num_removals)){
  trend_removed_data_matrix_COMPLICATED_LM_Scaled[k,]=trend_removed_data_matrix_COMPLICATED_LM[k,]/Smoothed_std_error_curves[[k]]
}


for(k in 1:(NUM_STATIONS*15-num_removals)){
  plot(trend_removed_data_matrix_COMPLICATED_LM[k,],main=k)
  plot(trend_removed_data_matrix_COMPLICATED_LM_Scaled[k,],main=k)
  
}

trend_removed_data_matrix_COMPLICATED_LM_Scaled[trend_removed_data_matrix_COMPLICATED_LM_Scaled==0]=NA
for(k in 1:(NUM_STATIONS*15-num_removals)){
  plot(trend_removed_data_matrix_COMPLICATED_LM[k,],type='l',main=k,xlim=c(1,10000)+25000)
  plot(trend_removed_data_matrix_COMPLICATED_LM_Scaled[k,],type='l',main=k,xlim=c(1,10000)+25000)
  
}

#trend_removed_data_matrix_COMPLICATED_LM_Scaled[trend_removed_data_matrix_COMPLICATED_LM_Scaled==0]=NA

##########################################################################################################
#Checking pacf and acf for MA va AR vs ARMA of order(?) behavior
##########################################################################################################
##########################################################################################################
trend_removed_data_matrix_COMPLICATED_LM_Scaled[is.na(trend_removed_data_matrix_COMPLICATED_LM_Scaled)]=0
plot(acf(trend_removed_data_matrix_COMPLICATED_LM_Scaled[1,],lag.max = 1600),main=1)

for(i in 1:(NUM_STATIONS*15-num_removals)) { 
  plot(acf(trend_removed_data_matrix_COMPLICATED_LM_Scaled[i,],lag.max = 1600),main=i)
}

#GENERALLY DOES A BETTER JOB AT REMOVING PERIODICITY??!  


############################################
#we use PACF to see if an AR model is appropriate for residuals...
#############################################
pacf(trend_removed_data_matrix_COMPLICATED_LM_Scaled[1,],lag.max = 1000)
pacf(trend_removed_data_matrix_COMPLICATED_LM_Scaled[1,],lag.max = 30)


#we see evidence of an ARMA(many,many) relationship but ignore this... it's at least AR(1)

trend_removed_data_matrix_COMPLICATED_LM_Scaled[trend_removed_data_matrix_COMPLICATED_LM_Scaled==0]=NA
##########################################################################################################
##########################################################################################################
##########################################################################################################
trend_removed_data_matrix_COMPLICATED_LM_Scaled[is.na(trend_removed_data_matrix_COMPLICATED_LM_Scaled)]=0
W=cor(t(trend_removed_data_matrix_COMPLICATED_LM_Scaled))

mean(W)
W[W<0]=0

W=W-diag(rep(1,dim(trend_removed_data_matrix_COMPLICATED_LM_Scaled)[1]))
W=normalize.rows(W,method="manhattan")
View(W)

W
###################################################
#parella algorithm starts running...  
##################################################
trend_removed_data_matrix_COMPLICATED_LM_Scaled[trend_removed_data_matrix_COMPLICATED_LM_Scaled==0]=NA

miss_vector_ones_and_zeros=function(data_vector_time_t){
  vector_of_onesandzeros=rep(NA,length(data_vector_time_t))
  for(i in 1:length(data_vector_time_t)){
    
    if(is.na(data_vector_time_t[i])){
      vector_of_onesandzeros[i]=0
    }
    
    if(!is.na(data_vector_time_t[i])){
      vector_of_onesandzeros[i]=1
    }
    
  }
  
  
  return(vector_of_onesandzeros)
}


delta_matrix=apply(trend_removed_data_matrix_COMPLICATED_LM_Scaled,2,miss_vector_ones_and_zeros)
#mean centreing data matrix with columns of "x" as sets of measurements from given station
#scale(x, scale = FALSE)


sum(delta_matrix[2,])


#replacing misses with zero for data matrix
trend_removed_data_matrix_COMPLICATED_LM_Scaled[is.na(trend_removed_data_matrix_COMPLICATED_LM_Scaled)]=0 
trend_removed_data_matrix_COMPLICATED_LM_Scaled[1,]


#fix up dimension...
y_bar=rowSums(trend_removed_data_matrix_COMPLICATED_LM_Scaled)/rowSums(delta_matrix)

y_bar
trend_removed_data_matrix_COMPLICATED_LM_Scaled[1,]
sweep(trend_removed_data_matrix_COMPLICATED_LM_Scaled,1,y_bar)


y_tilde_matrix=delta_matrix*sweep(trend_removed_data_matrix_COMPLICATED_LM_Scaled,1,y_bar)
y_tilde_matrix[,1] 

#install.packages("Hmisc")
library("Hmisc")


#iteration 

num_iterations=300  #change this to be large enough for convergence (or use while loop eventually)

#need to instanciate blank yhat...
yhat_matrix=matrix(rep(0,dim(y_tilde_matrix)[1]*dim(y_tilde_matrix)[2]),dim(y_tilde_matrix)[1],dim(y_tilde_matrix)[2])
for(s in 1:num_iterations){
  print("at iteration")
  print(s)
  
  #a)
  R=matrix(rep(0,dim(y_tilde_matrix)[1]^2),dim(y_tilde_matrix)[1],dim(y_tilde_matrix)[1])
  for(i in 1:(dim(y_tilde_matrix)[2]-1)){R=R+y_tilde_matrix[,i+1]%*%t(y_tilde_matrix[,i])}
  G=matrix(rep(0,dim(y_tilde_matrix)[1]^2),dim(y_tilde_matrix)[1],dim(y_tilde_matrix)[1])
  for(i in 1:(dim(y_tilde_matrix)[2])){G=G+y_tilde_matrix[,i]%*%t(y_tilde_matrix[,i])}
  
  Sigma1HAT=1/dim(y_tilde_matrix)[2]*R
  Sigma0HAT=1/dim(y_tilde_matrix)[2]*G
  Sigma1HAT
  Sigma0HAT
  
  lambda0=rep(0,dim(y_tilde_matrix)[1])
  lambda1=rep(0,dim(y_tilde_matrix)[1])
  lambda2=rep(0,dim(y_tilde_matrix)[1])
  
  for(i in 1:length(lambda0)){
    
    ei=rep(0,dim(y_tilde_matrix)[1])
    ei[i]=1
    
    wi=W[i,]
    
    Xi_HAT=cbind(t(Sigma1HAT)%*%wi,Sigma0HAT%*%ei,Sigma0HAT%*%wi)
    Yi_HAT=t(Sigma1HAT)%*%ei
    
    lam=(t(solvet(t(Xi_HAT)%*%Xi_HAT,tol = 1e-25)%*%t(Xi_HAT)%*%Yi_HAT))
    
    lambda0[i]=lam[1]
    lambda1[i]=lam[2]
    lambda2[i]=lam[3]
  }
  lambda0
  print(norm((diag(lambda0)%*%W), type = "2")+norm((diag(lambda1)+diag(lambda2)%*%W), type = "2"))
  print(solve(diag(NUM_STATIONS*15-num_removals)-diag(lambda0)%*%W))
  #b)
  #yhat_matrix two steps for t-1=0 case... I'm assuming other terms are just zero...
  yhat_matrix[,1]=y_tilde_matrix[,1]
  for(i in 2:dim(y_tilde_matrix)[2]){
    yhat_matrix[,i]=diag(lambda0)%*%W%*%y_tilde_matrix[,i]+diag(lambda1)%*%y_tilde_matrix[,i-1]+diag(lambda2)%*%W%*%y_tilde_matrix[,i-1]  
  }
  
  
  #ybar
  x=rep(0,dim(y_tilde_matrix)[1])
  for(i in 1:dim(y_tilde_matrix)[2]){
    x=x+delta_matrix[,i]*trend_removed_data_matrix_COMPLICATED_LM_Scaled[,i]+(rep(1,length(trend_removed_data_matrix_COMPLICATED_LM_Scaled[,1]))-delta_matrix[,i])*(yhat_matrix[,i]+y_bar)
  }
  y_bar=x/dim(y_tilde_matrix)[2]
  
  
  #ytilde_matrix
  for(i in 1:dim(y_tilde_matrix)[2]){
    y_tilde_matrix[,i]=delta_matrix[,i]*(trend_removed_data_matrix_COMPLICATED_LM_Scaled[,i]-y_bar)+(rep(1,dim(y_tilde_matrix)[1])-delta_matrix[,i])*yhat_matrix[,i]
  }
  
  
}


#the result
result=y_tilde_matrix+y_bar
#want this to be false...
all(result==trend_removed_data_matrix_COMPLICATED_LM_Scaled)
dim(result)
#compare with this
trend_removed_data_matrix_COMPLICATED_LM_Scaled[1,]

trend_removed_data_matrix_COMPLICATED_LM_Scaled_IMPUTED=result

par(mfrow=c(3,3))
#plotting  the imputed series...
for(i in 1:(NUM_STATIONS*15-num_removals)) {
  plot(trend_removed_data_matrix_COMPLICATED_LM_Scaled_IMPUTED[i,],type='l',ylim=c(-150,10),main=i)
  plot(trend_removed_data_matrix_COMPLICATED_LM_Scaled[i,],type='l',ylim=c(-150,10),main=i)
  plot(data_matrix[i,],type='l',ylim=c(-150,10),main=i)
  
  
}






###########################################
#Undoing variance scaling with the flavour of the day  
################################################
trend_removed_data_matrix_COMPLICATED_LM_IMPUTED=trend_removed_data_matrix_COMPLICATED_LM_Scaled_IMPUTED
trend_removed_data_matrix_COMPLICATED_LM_IMPUTED[]=NA
for(k in 1:((NUM_STATIONS*15)-num_removals)){
  trend_removed_data_matrix_COMPLICATED_LM_IMPUTED[k,]=trend_removed_data_matrix_COMPLICATED_LM_Scaled_IMPUTED[k,]*Smoothed_std_error_curves[[k]]
}








###########################################
#Undoing detrending with the flavour of the day (need to have this last)
################################################
retrended_data_matrix=matrix(rep(NA,(dim(trend_removed_data_matrix_COMPLICATED_LM_IMPUTED)[1]*(dim(trend_removed_data_matrix_COMPLICATED_LM_IMPUTED)[2]))),dim(trend_removed_data_matrix_COMPLICATED_LM_IMPUTED)[1],(dim(trend_removed_data_matrix_COMPLICATED_LM_IMPUTED)[2]))

dim(retrended_data_matrix)
dim(trend_removed_data_matrix_COMPLICATED_LM_IMPUTED)
for(i in 1:((NUM_STATIONS*15)-num_removals)){
  
  t=1:longest_num_obs
  w=2*pi/365.25/2 #we divide by two again because day and night data included!!
  
  cosine_series=cos(w*t)
  sine_series=sin(w*t)
  cosine_series2=cos(2*w*t)
  sine_series2=sin(2*w*t)
  cosine_series3=cos(3*w*t)
  sine_series3=sin(3*w*t)
  cosine_series4=cos(4*w*t)
  sine_series4=sin(4*w*t)
  #cosine_series5=cos(5*w*t)
  #sine_series5=sin(5*w*t)
  #cosine_series6=cos(6*w*t)
  #sine_series6=sin(6*w*t)
  #cosine_series7=cos(7*w*t)
  #sine_series7=sin(7*w*t)
  #cosine_series8=cos(8*w*t)
  #sine_series8=sin(8*w*t)
  #cosine_series9=cos(9*w*t)
  #sine_series9=sin(9*w*t)
  #cosine_series10=cos(10*w*t)
  #sine_series10=sin(10*w*t)
  
  
  
  model1=lm(data_matrix[i,]~t+cosine_series+sine_series+cosine_series2+sine_series2+cosine_series3+sine_series3+cosine_series4+sine_series4)
  
  retrended_data_matrix[i,]=trend_removed_data_matrix_COMPLICATED_LM_IMPUTED[i,]+(model1$coefficients[1]+model1$coefficients[2]*t+model1$coefficients[3]*cosine_series+model1$coefficients[4]*sine_series+model1$coefficients[5]*cosine_series2+model1$coefficients[6]*sine_series2+model1$coefficients[7]*cosine_series3+model1$coefficients[8]*sine_series3+model1$coefficients[9]*cosine_series4+model1$coefficients[10]*sine_series4 )
  
  
  #model1=lm(data_matrix[i,]~t+cosine_series+sine_series+cosine_series2+sine_series2+cosine_series3+sine_series3+cosine_series4+sine_series4+cosine_series5+sine_series5+cosine_series6+sine_series6+cosine_series7+sine_series7+cosine_series8+sine_series8+cosine_series9+sine_series9+cosine_series10+sine_series10)
  
  #retrended_data_matrix[i,]=trend_removed_data_matrix_COMPLICATED_LM_IMPUTED[i,]+(model1$coefficients[1]+model1$coefficients[2]*t+model1$coefficients[3]*cosine_series+model1$coefficients[4]*sine_series+model1$coefficients[5]*cosine_series2+model1$coefficients[6]*sine_series2+model1$coefficients[7]*cosine_series3+model1$coefficients[8]*sine_series3+model1$coefficients[9]*cosine_series4+model1$coefficients[10]*sine_series4+model1$coefficients[11]*cosine_series5+model1$coefficients[12]*sine_series5+model1$coefficients[13]*cosine_series6+model1$coefficients[14]*sine_series6+model1$coefficients[15]*cosine_series7+model1$coefficients[16]*sine_series7+model1$coefficients[17]*cosine_series8+model1$coefficients[18]*sine_series8+model1$coefficients[19]*cosine_series9+model1$coefficients[20]*sine_series9+model1$coefficients[21]*cosine_series10+model1$coefficients[22]*sine_series10 )
  
  
  
  
}

#plot of last lm fit...
#plot( xlim=c(1,2000),(model1$coefficients[1]+model1$coefficients[2]*t+model1$coefficients[3]*cosine_series+model1$coefficients[4]*sine_series+model1$coefficients[5]*cosine_series2+model1$coefficients[6]*sine_series2+model1$coefficients[7]*cosine_series3+model1$coefficients[8]*sine_series3+model1$coefficients[9]*cosine_series4+model1$coefficients[10]*sine_series4+model1$coefficients[11]*cosine_series5+model1$coefficients[12]*sine_series5+model1$coefficients[13]*cosine_series6+model1$coefficients[14]*sine_series6))


###########################################
#adding noise
################################################


residuals_std_estimate_vector=rep(NA,(NUM_STATIONS*15-num_removals))


COMPLICATED_LM_reconstructed_series_WITH_ADDED_NOISE=retrended_data_matrix


for(i in 1:(NUM_STATIONS*15-num_removals)){
  
  residuals_std_estimate_vector[i]=sd(trend_removed_data_matrix_COMPLICATED_LM[i,(!is.na(data_matrix[i,]))])
  #STL_reconstructed_series[i,]=result[i,]+stl(obs1,s.window = "periodic")$time.series[,"seasonal"]+stl(obs1,s.window = "periodic")$time.series[,"trend"]
  
  #STL_noiseless_reconstructed_series[i,]=result[i,]+stl(obs1,s.window = "periodic")$time.series[,"seasonal"]+stl(obs1,s.window = "periodic")$time.series[,"trend"]
  
  
  COMPLICATED_LM_reconstructed_series_WITH_ADDED_NOISE[i,(is.na(data_matrix[i,]))]=COMPLICATED_LM_reconstructed_series_WITH_ADDED_NOISE[i,(is.na(data_matrix[i,]))]+rnorm(sum(is.na(data_matrix[i,])),0,residuals_std_estimate_vector[i])
  
}
residuals_std_estimate_vector



#############################################################################################################3
#############################################################################################################

known_residuals=matrix(rep(NA,(dim(trend_removed_data_matrix_COMPLICATED_LM_IMPUTED)[1]*(dim(trend_removed_data_matrix_COMPLICATED_LM_IMPUTED)[2]))),dim(trend_removed_data_matrix_COMPLICATED_LM_IMPUTED)[1],(dim(trend_removed_data_matrix_COMPLICATED_LM_IMPUTED)[2]))

for(i in 1:((NUM_STATIONS*15)-num_removals)){
  
  t=1:longest_num_obs
  w=2*pi/365.25/2 #we divide by two again because day and night data included!!
  
  cosine_series=cos(w*t)
  sine_series=sin(w*t)
  cosine_series2=cos(2*w*t)
  sine_series2=sin(2*w*t)
  cosine_series3=cos(3*w*t)
  sine_series3=sin(3*w*t)
  cosine_series4=cos(4*w*t)
  sine_series4=sin(4*w*t)
  #cosine_series5=cos(5*w*t)
  #sine_series5=sin(5*w*t)
  #cosine_series6=cos(6*w*t)
  #sine_series6=sin(6*w*t)
  #cosine_series7=cos(7*w*t)
  #sine_series7=sin(7*w*t)
  #cosine_series8=cos(8*w*t)
  #sine_series8=sin(8*w*t)
  #cosine_series9=cos(9*w*t)
  #sine_series9=sin(9*w*t)
  #cosine_series10=cos(10*w*t)
  #sine_series10=sin(10*w*t)
  
  model1=lm(data_matrix[i,]~t+cosine_series+sine_series+cosine_series2+sine_series2+cosine_series3+sine_series3+cosine_series4+sine_series4)
  
  #####################################
  #####################################
  known_residuals[i,]=(known_data_matrix)[i,]-(model1$coefficients[1]+model1$coefficients[2]*t+model1$coefficients[3]*cosine_series+model1$coefficients[4]*sine_series+model1$coefficients[5]*cosine_series2+model1$coefficients[6]*sine_series2+model1$coefficients[7]*cosine_series3+model1$coefficients[8]*sine_series3+model1$coefficients[9]*cosine_series4+model1$coefficients[10]*sine_series4 )
  
}


#In the below code a kalman filter is used to impute values under a VAR(1) model.
#Since not all loctions can be processed at once, for each location the batch of 4 locations with highest correlation are used in individual VAR(1) models. 
#2 locations are from the same station, 2 are from other stations. This is to deal well (hopefully) with long gaps and short gaps of misses. 
#Long gaps should be dealt with by leveraging between station dynamics. Short ones by within station dynamics.


#install.packages("stats")
library(stats)
#install.packages("MARSS")
library(MARSS)


data_matrix[is.na(data_matrix)]=0
#these are the station locations
removed_station_vector_initial


correlation_matrix=(cor(t(data_matrix)))
correlation_matrix=correlation_matrix-diag(1,dim(correlation_matrix)[1])
correlation_matrix_between_stations=correlation_matrix
correlation_matrix_within_stations=correlation_matrix



correlation_matrix_between_stations[1:15,1:15]=0
correlation_matrix_between_stations[16:30,16:30]=0
correlation_matrix_between_stations[31:45,31:45]=0
correlation_matrix_between_stations[46:60,46:60]=0


correlation_matrix_within_stations=correlation_matrix-correlation_matrix_between_stations




#example of finding indices of highest values
order(c(2,7,4,9,5),decreasing = TRUE)[1:3]


order(correlation_matrix_between_stations[1,],decreasing = TRUE)[1:2]
order(correlation_matrix_within_stations[1,],decreasing = TRUE)[1:2]



#####################################################################################################
#####################################################################################################

grouped_stations_list=matrix(data=NA,(NUM_STATIONS*15-num_removals),5)
for(i in 1:(NUM_STATIONS*15-num_removals)){
  
  grouped_stations_list[i,1]=i
  grouped_stations_list[i,c(2,3)]=order(correlation_matrix_between_stations[i,],decreasing = TRUE)[1:2]
  grouped_stations_list[i,c(4,5)]=order(correlation_matrix_within_stations[i,],decreasing = TRUE)[1:2]
}

grouped_stations_list



#####################################################################################################
#####################################################################################################
Kalman_matrix_of_imputed_residuals_stationary=matrix(data=NA,(dim(trend_removed_data_matrix_COMPLICATED_LM_Scaled)[1]),(dim(trend_removed_data_matrix_COMPLICATED_LM_Scaled)[2]))


trend_removed_data_matrix_COMPLICATED_LM_Scaled[trend_removed_data_matrix_COMPLICATED_LM_Scaled==0]=NA
data_matrix[data_matrix==0]=NA

for(i in 1:(NUM_STATIONS*15-num_removals)){
  
  
  Z <- diag(1,5)
  B <- matrix(paste(rep("B",(5)^2),as.character(1:(5)^2)), 5, 5)
  U <- matrix(0, (5), 1)
  #note Q is made symmetric X%*%t(X) leveraged... 
  Q=matrix(paste(rep("Q",(5)^2),as.character(matrix((1:(5)^2), 5, 5) %*%t(matrix((1:(5)^2), 5, 5)))), 5, 5)
  A <- matrix(0, (5), 1)
  R <- matrix(0, (5), (5))
  
  #see page 252 on how to set this... 
  pinit <-as.matrix(rowMeans(trend_removed_data_matrix_COMPLICATED_LM_Scaled[grouped_stations_list[i,],],na.rm=TRUE))
  dim(pinit)
  
  V <- matrix(0, (5), (5))
  model.list.2m <- list(Z = Z, B = B, U = U, Q = Q, A = A,R = R, x0 = pinit, V0 = V, tinitx = 1)
  
  mar2 <- MARSS(trend_removed_data_matrix_COMPLICATED_LM_Scaled[grouped_stations_list[i,], (2:dim(trend_removed_data_matrix_COMPLICATED_LM_Scaled)[2])], model = model.list.2m,silent = 2)
  
  
  imputed_kalman_scaled_residuals=cbind(trend_removed_data_matrix_COMPLICATED_LM[grouped_stations_list[i,],1],mar2$ytT)
  
  #note the one on the RHS is meant to be there!! think about it... 
  Kalman_matrix_of_imputed_residuals_stationary[i,]=imputed_kalman_scaled_residuals[1,]
}

#####################################################################################################
#####################################################################################################
kalman_residuls_scaling_undone_IMPUTED=matrix(NA,dim(Kalman_matrix_of_imputed_residuals_stationary)[1],dim(Kalman_matrix_of_imputed_residuals_stationary)[2])

for(k in 1:((NUM_STATIONS*15)-num_removals)){
  kalman_residuls_scaling_undone_IMPUTED[k,]=Kalman_matrix_of_imputed_residuals_stationary[k,]*Smoothed_std_error_curves[[k]]
}

for(k in 1:((NUM_STATIONS*15)-num_removals)){
  plot(kalman_residuls_scaling_undone_IMPUTED[k,])
}

###########################################
#Undoing detrending with the flavour of the day (need to have this last)
################################################
retrended_data_matrix_kalman=matrix(NA,dim(kalman_residuls_scaling_undone_IMPUTED)[1],(dim(kalman_residuls_scaling_undone_IMPUTED)[2]))


for(i in 1:((NUM_STATIONS*15)-num_removals)){
  
  t=1:longest_num_obs
  w=2*pi/365.25/2 #we divide by two again because day and night data included!!
  
  cosine_series=cos(w*t)
  sine_series=sin(w*t)
  cosine_series2=cos(2*w*t)
  sine_series2=sin(2*w*t)
  cosine_series3=cos(3*w*t)
  sine_series3=sin(3*w*t)
  cosine_series4=cos(4*w*t)
  sine_series4=sin(4*w*t)
  
  model1=lm(data_matrix[i,]~t+cosine_series+sine_series+cosine_series2+sine_series2+cosine_series3+sine_series3+cosine_series4+sine_series4)
  
  retrended_data_matrix_kalman[i,]=kalman_residuls_scaling_undone_IMPUTED[i,]+(model1$coefficients[1]+model1$coefficients[2]*t+model1$coefficients[3]*cosine_series+model1$coefficients[4]*sine_series+model1$coefficients[5]*cosine_series2+model1$coefficients[6]*sine_series2+model1$coefficients[7]*cosine_series3+model1$coefficients[8]*sine_series3+model1$coefficients[9]*cosine_series4+model1$coefficients[10]*sine_series4 )
  
  
}




MSE_1_period_removed_vec_KALMAN=rep(NA,((NUM_STATIONS*15)-num_removals))
for(i in 1:((NUM_STATIONS*15)-num_removals)){
  MSE_1_period_removed_vec_KALMAN[i]=mean((retrended_data_matrix_kalman[i,Test_SET_locations_matrix[i,]]-known_data_matrix[i,Test_SET_locations_matrix[i,]])^2,na.rm=TRUE)
}
MSE_1_period_removed_vec_KALMAN



MSE_1_KALMAN=rep(NA,((NUM_STATIONS*15)-num_removals))
for(i in 1:((NUM_STATIONS*15)-num_removals)){
  MSE_1_KALMAN[i]=mean((kalman_residuls_scaling_undone_IMPUTED[i,Test_SET_locations_matrix[i,]]-known_residuals[i,Test_SET_locations_matrix[i,]])^2,na.rm=TRUE)
}
MSE_1_KALMAN


MSE_1_Parrella=rep(NA,((NUM_STATIONS*15)-num_removals))
for(i in 1:((NUM_STATIONS*15)-num_removals)){
  MSE_1_Parrella[i]=mean((trend_removed_data_matrix_COMPLICATED_LM_IMPUTED[i,Test_SET_locations_matrix[i,]]-known_residuals[i,Test_SET_locations_matrix[i,]])^2,na.rm=TRUE)
}
MSE_1_Parrella

#############################################################################################################
#############################################################################################################



MSE_1_period_removed_vec_LM_ONLY=rep(NA,((NUM_STATIONS*15)-num_removals))
for(i in 1:((NUM_STATIONS*15)-num_removals)){
  
  
  t=1:longest_num_obs
  w=2*pi/365.25/2 #we divide by two again because day and night data included!!
  
  cosine_series=cos(w*t)
  sine_series=sin(w*t)
  cosine_series2=cos(2*w*t)
  sine_series2=sin(2*w*t)
  cosine_series3=cos(3*w*t)
  sine_series3=sin(3*w*t)
  cosine_series4=cos(4*w*t)
  sine_series4=sin(4*w*t)
  #cosine_series5=cos(5*w*t)
  #sine_series5=sin(5*w*t)
  #cosine_series6=cos(6*w*t)
  #sine_series6=sin(6*w*t)
  #cosine_series7=cos(7*w*t)
  #sine_series7=sin(7*w*t)
  #cosine_series8=cos(8*w*t)
  #sine_series8=sin(8*w*t)
  #cosine_series9=cos(9*w*t)
  #sine_series9=sin(9*w*t)
  #cosine_series10=cos(10*w*t)
  #sine_series10=sin(10*w*t)
  
  model1=lm(data_matrix[i,]~t+cosine_series+sine_series+cosine_series2+sine_series2+cosine_series3+sine_series3+cosine_series4+sine_series4)
  
  #####################################
  #####################################
  
  MSE_1_period_removed_vec_LM_ONLY[i]=mean(((model1$coefficients[1]+model1$coefficients[2]*t+model1$coefficients[3]*cosine_series+model1$coefficients[4]*sine_series+model1$coefficients[5]*cosine_series2+model1$coefficients[6]*sine_series2+model1$coefficients[7]*cosine_series3+model1$coefficients[8]*sine_series3+model1$coefficients[9]*cosine_series4+model1$coefficients[10]*sine_series4 )[Test_SET_locations_matrix[i,]]-known_data_matrix[i,Test_SET_locations_matrix[i,]])^2,na.rm=TRUE)
}
MSE_1_period_removed_vec_LM_ONLY



#install.packages("imputeTS")
library("imputeTS")
plot(na.interpolation(trend_removed_data_matrix_COMPLICATED_LM[2,],option = "linear"))
plot(trend_removed_data_matrix_COMPLICATED_LM[2,])


retrended_data_matrix_Linear_Interpolation=matrix(rep(NA,(dim(trend_removed_data_matrix_COMPLICATED_LM_IMPUTED)[1]*(dim(trend_removed_data_matrix_COMPLICATED_LM_IMPUTED)[2]))),dim(trend_removed_data_matrix_COMPLICATED_LM_IMPUTED)[1],(dim(trend_removed_data_matrix_COMPLICATED_LM_IMPUTED)[2]))

for(i in 1:((NUM_STATIONS*15)-num_removals)){
  
  t=1:longest_num_obs
  w=2*pi/365.25/2 #we divide by two again because day and night data included!!
  
  cosine_series=cos(w*t)
  sine_series=sin(w*t)
  cosine_series2=cos(2*w*t)
  sine_series2=sin(2*w*t)
  cosine_series3=cos(3*w*t)
  sine_series3=sin(3*w*t)
  cosine_series4=cos(4*w*t)
  sine_series4=sin(4*w*t)
  #cosine_series5=cos(5*w*t)
  #sine_series5=sin(5*w*t)
  #cosine_series6=cos(6*w*t)
  #sine_series6=sin(6*w*t)
  #cosine_series7=cos(7*w*t)
  #sine_series7=sin(7*w*t)
  #cosine_series8=cos(8*w*t)
  #sine_series8=sin(8*w*t)
  #cosine_series9=cos(9*w*t)
  #sine_series9=sin(9*w*t)
  #cosine_series10=cos(10*w*t)
  #sine_series10=sin(10*w*t)
  
  model1=lm(data_matrix[i,]~t+cosine_series+sine_series+cosine_series2+sine_series2+cosine_series3+sine_series3+cosine_series4+sine_series4)
  
  #####################################
  #####################################
  retrended_data_matrix_Linear_Interpolation[i,]=na.interpolation(trend_removed_data_matrix_COMPLICATED_LM[i,],option = "linear")+(model1$coefficients[1]+model1$coefficients[2]*t+model1$coefficients[3]*cosine_series+model1$coefficients[4]*sine_series+model1$coefficients[5]*cosine_series2+model1$coefficients[6]*sine_series2+model1$coefficients[7]*cosine_series3+model1$coefficients[8]*sine_series3+model1$coefficients[9]*cosine_series4+model1$coefficients[10]*sine_series4 )
  
}


MSE_1_period_removed_Linear_Interpolation=rep(NA,((NUM_STATIONS*15)-num_removals))
for(i in 1:((NUM_STATIONS*15)-num_removals)){
  
  MSE_1_period_removed_Linear_Interpolation[i]=mean((retrended_data_matrix_Linear_Interpolation[i,Test_SET_locations_matrix[i,]]-known_data_matrix[i,Test_SET_locations_matrix[i,]])^2,na.rm=TRUE)
}
MSE_1_period_removed_Linear_Interpolation

MSE_1_period_removed_vec_NO_NOISE=rep(NA,((NUM_STATIONS*15)-num_removals))
for(i in 1:((NUM_STATIONS*15)-num_removals)){
  MSE_1_period_removed_vec_NO_NOISE[i]=mean((known_data_matrix[i,Test_SET_locations_matrix[i,]]-retrended_data_matrix[i,Test_SET_locations_matrix[i,]])^2,na.rm=TRUE)
}
MSE_1_period_removed_vec_NO_NOISE

#choosing filter order based on ACF cutoff...
par(mfrow=c(1,1))
acf(trend_removed_data_matrix_COMPLICATED_LM_IMPUTED[1,],lag.max = 200)
acf(trend_removed_data_matrix_COMPLICATED_LM_IMPUTED[16,],lag.max = 200)
acf(trend_removed_data_matrix_COMPLICATED_LM_IMPUTED[25,],lag.max = 200)
#based on these our model is pretty bad in general... We do not pursue this!!


FILTERED_retrended_data_matrix=matrix(rep(NA,(dim(trend_removed_data_matrix_COMPLICATED_LM_IMPUTED)[1]*(dim(trend_removed_data_matrix_COMPLICATED_LM_IMPUTED)[2]))),dim(trend_removed_data_matrix_COMPLICATED_LM_IMPUTED)[1],(dim(trend_removed_data_matrix_COMPLICATED_LM_IMPUTED)[2]))
filtered_residuals_without_known_ones_set=matrix(rep(NA,(dim(trend_removed_data_matrix_COMPLICATED_LM_IMPUTED)[1]*(dim(trend_removed_data_matrix_COMPLICATED_LM_IMPUTED)[2]))),dim(trend_removed_data_matrix_COMPLICATED_LM_IMPUTED)[1],(dim(trend_removed_data_matrix_COMPLICATED_LM_IMPUTED)[2]))
filtered_residuals_with_known_ones_set=matrix(rep(NA,(dim(trend_removed_data_matrix_COMPLICATED_LM_IMPUTED)[1]*(dim(trend_removed_data_matrix_COMPLICATED_LM_IMPUTED)[2]))),dim(trend_removed_data_matrix_COMPLICATED_LM_IMPUTED)[1],(dim(trend_removed_data_matrix_COMPLICATED_LM_IMPUTED)[2]))
for(i in 1:((NUM_STATIONS*15)-num_removals)){
  
  t=1:longest_num_obs
  w=2*pi/365.25/2 #we divide by two again because day and night data included!!
  
  cosine_series=cos(w*t)
  sine_series=sin(w*t)
  cosine_series2=cos(2*w*t)
  sine_series2=sin(2*w*t)
  cosine_series3=cos(3*w*t)
  sine_series3=sin(3*w*t)
  cosine_series4=cos(4*w*t)
  sine_series4=sin(4*w*t)
  
  model1=lm(data_matrix[i,]~t+cosine_series+sine_series+cosine_series2+sine_series2+cosine_series3+sine_series3+cosine_series4+sine_series4)
  
  filtered_residuals_without_known_ones_set[i,]=ma(trend_removed_data_matrix_COMPLICATED_LM_IMPUTED[i,],5)
  
  filtered_residuals_with_known_ones_set[i,]=filtered_residuals_without_known_ones_set[i,]
  filtered_residuals_with_known_ones_set[i,!is.na(trend_removed_data_matrix_COMPLICATED_LM[i,])]=trend_removed_data_matrix_COMPLICATED_LM[i,!is.na(trend_removed_data_matrix_COMPLICATED_LM[i,])]
  
  FILTERED_retrended_data_matrix[i,]=filtered_residuals_with_known_ones_set[i,]+(model1$coefficients[1]+model1$coefficients[2]*t+model1$coefficients[3]*cosine_series+model1$coefficients[4]*sine_series+model1$coefficients[5]*cosine_series2+model1$coefficients[6]*sine_series2+model1$coefficients[7]*cosine_series3+model1$coefficients[8]*sine_series3+model1$coefficients[9]*cosine_series4+model1$coefficients[10]*sine_series4 )
  
}

MSE_1_period_removed_vec_FILTERED_NO_NOISE=rep(NA,((NUM_STATIONS*15)-num_removals))
for(i in 1:((NUM_STATIONS*15)-num_removals)){
  MSE_1_period_removed_vec_FILTERED_NO_NOISE[i]=mean((known_data_matrix[i,Test_SET_locations_matrix[i,]]-FILTERED_retrended_data_matrix[i,Test_SET_locations_matrix[i,]])^2,na.rm=TRUE)
}
MSE_1_period_removed_vec_FILTERED_NO_NOISE
sum(MSE_1_period_removed_vec_FILTERED_NO_NOISE)
#############################################################
#final plots
########################################################
par(mfrow=c(2,1))
for(i in 1:((NUM_STATIONS*15)-num_removals)) { 
  
  plot(trend_removed_data_matrix_COMPLICATED_LM[i,],type='l',ylim=c(-100,10))
  plot(COMPLICATED_LM_reconstructed_series_WITH_ADDED_NOISE[i,],type='l',ylim=c(-100,10),main=i)
  
}


par(mfrow=c(2,1))
for(i in 1:(NUM_STATIONS*15-num_removals)) { 
  
  plot(data_matrix[i,],type='l',ylim=c(-100,10))
  plot(retrended_data_matrix[i,],type='l',ylim=c(-100,10),main=i)
  
}


par(mfrow=c(3,1))
for(i in 1:(NUM_STATIONS*15-num_removals)) { 
  plot(trend_removed_data_matrix_COMPLICATED_LM_IMPUTED[i,],type='l',ylim=c(-30,30))
  plot(trend_removed_data_matrix_COMPLICATED_LM_Scaled_IMPUTED[i,],ylim=c(-30,30),type='l')
  plot(trend_removed_data_matrix_COMPLICATED_LM[i,],ylim=c(-30,30),type='l')
  
}


par(mfrow=c(1,1))
for(i in 1:(NUM_STATIONS*15-num_removals)) { 
  plot(trend_removed_data_matrix_COMPLICATED_LM_Scaled_IMPUTED[i,],ylim=c(-5,5),type='l')
  
}


par(mfrow=c(2,1))
for(i in 1:(NUM_STATIONS*15-num_removals)) { 
  #plot(retrended_data_matrix[i,],type='l',ylim=c(-100,10),main=i)
  plot(data_matrix[i,],ylim=c(-100,10),type='l')
  
  plot(COMPLICATED_LM_reconstructed_series_WITH_ADDED_NOISE[i,],type='l',ylim=c(-100,10),main=i)
  
}







par(mfrow=c(5,2))
for(i in 1:(NUM_STATIONS*15-num_removals)) { 
  #plot(retrended_data_matrix[i,],type='l',ylim=c(-100,10),main=i)
  plot(data_matrix[i,],ylim=c(-100,10),type='l')
  plot(retrended_data_matrix[i,],type='l',ylim=c(-100,10),main=i)
  
}


par(mfrow=c(2,1))
for(i in 1:(NUM_STATIONS*15-num_removals)) { 
  #plot(retrended_data_matrix[i,],type='l',ylim=c(-100,10),main=i)
  plot(data_matrix[i,],ylim=c(-100,10),type='l',xlim=c(30000,32000))
  plot(retrended_data_matrix[i,],type='l',ylim=c(-100,10),xlim=c(30000,32000),main=i)
  
}

par(mfrow=c(2,1))
for(i in 1:(NUM_STATIONS*15-num_removals)) { 
  #plot(retrended_data_matrix[i,],type='l',ylim=c(-100,10),main=i)
  plot(data_matrix[i,],ylim=c(-100,10),type='l',xlim=c(1,5000))
  plot(retrended_data_matrix[i,],type='l',ylim=c(-100,10),xlim=c(1,5000),main=i)
  
}


#############################


MSE_1_period_removed_Linear_Interpolation
sum(MSE_1_period_removed_Linear_Interpolation)

MSE_1_period_removed_vec_KALMAN
sum(MSE_1_period_removed_vec_KALMAN)
sum(MSE_1_period_removed_vec_KALMAN)/60

MSE_1_period_removed_vec_LM_ONLY
sum(MSE_1_period_removed_vec_LM_ONLY)
sum(MSE_1_period_removed_vec_LM_ONLY)/60

MSE_1_period_removed_vec_NO_NOISE
sum(MSE_1_period_removed_vec_NO_NOISE)

MSE_1_period_removed_vec_FILTERED_NO_NOISE
sum(MSE_1_period_removed_vec_FILTERED_NO_NOISE)


x=MSE_1_period_removed_vec_KALMAN-MSE_1_period_removed_vec_FILTERED_NO_NOISE
sort(x)

y=MSE_1_period_removed_vec_KALMAN-MSE_1_period_removed_vec_NO_NOISE
sort(y)

par(mfrow=c(1,1))

plot(data_matrix[11,])
sum(is.na(data_matrix[11,]))
#############################################################
#############################################################
par(mfrow=c(3,1))
plot(retrended_data_matrix_Linear_Interpolation[1,])
plot(retrended_data_matrix_kalman[1,])
#plot of veclmonly would just be periodic function...
plot(retrended_data_matrix[1,])
#############################################################
#############################################################
par(mfrow=c(2,1))
plot(trend_removed_data_matrix_COMPLICATED_LM_IMPUTED[1,])
plot(kalman_residuls_scaling_undone_IMPUTED[1,])
#plot of veclmonly would just be periodic function...
#############################################################
#############################################################
par(mfrow=c(5,1))
plot(na.interpolation(trend_removed_data_matrix_COMPLICATED_LM_Scaled[1,20000:22000],option = "linear"),ylim=c(-3,3))
plot(trend_removed_data_matrix_COMPLICATED_LM_Scaled_IMPUTED[1,20000:22000],ylim=c(-3,3),type='l')
plot(ma(trend_removed_data_matrix_COMPLICATED_LM_Scaled_IMPUTED[1,20000:22000],10),ylim=c(-3,3))
plot(Kalman_matrix_of_imputed_residuals_stationary[1,20000:22000],ylim=c(-3,3),type='l')
plot(known_residuals[1,20000:22000])
############################################################
#############################################################
par(mfrow=c(5,1))
plot(na.interpolation(trend_removed_data_matrix_COMPLICATED_LM_Scaled[1,30000:32000],option = "linear"),ylim=c(-3,3))
plot(trend_removed_data_matrix_COMPLICATED_LM_Scaled_IMPUTED[1,30000:32000],ylim=c(-3,3),type='l')
plot(ma(trend_removed_data_matrix_COMPLICATED_LM_Scaled_IMPUTED[1,30000:32000],10),ylim=c(-3,3))
plot(Kalman_matrix_of_imputed_residuals_stationary[1,30000:32000],ylim=c(-3,3),type='l')
plot(known_residuals[1,30000:32000])










#########################################################################
############################################################################################################################
############################################################################################################################
############################################################################################################################
#FINAL PLOTS 
############################################################################################################################
############################################################################################################################
############################################################################################################################
#diagnostic plots
#checking fit....

##############

###############
par(mfrow=c(2,2))

t=1:longest_num_obs
w=2*pi/365.25 #we divide by two again because day and night data included!!

cosine_series=cos(w*t)
sine_series=sin(w*t)
cosine_series2=cos(2*w*t)
sine_series2=sin(2*w*t)
cosine_series3=cos(3*w*t)
sine_series3=sin(3*w*t)
cosine_series4=cos(4*w*t)
sine_series4=sin(4*w*t)

model1=lm(data_matrix[46,]~t+cosine_series+sine_series+cosine_series2+sine_series2+cosine_series3+sine_series3+cosine_series4+sine_series4)

plot(model1, which =3 ,main="Casey 1000 hPa: Standardised Residuals vs. Fitted Values ", head("")) 
plot(model1, which =2 ,main="Casey 1000 hPa: Q-Q Plot",head("")) 
model2=lm(data_matrix[60,]~t+cosine_series+sine_series+cosine_series2+sine_series2+cosine_series3+sine_series3+cosine_series4+sine_series4)

plot(model2, which =3 ,main="Casey 92500 hPa Standardised Residuals vs. Fitted Values ", head("")) 
plot(model2, which =2 ,main="Casey 92500 hPa Q-Q Plot",head("")) 

#visual inspection for our detrendig and for stationarity
#Below we plot original (relatively dense dataset) vs. detrended data
par(new=FALSE)
par(mfrow=c(2,2))

trend_removed_data_matrix_COMPLICATED_LM[trend_removed_data_matrix_COMPLICATED_LM==0]=NA

plot(data_matrix[20,],type="l",main="Marrawah: Raw Temperature Series",xlab = "Days since January 1st 2000",ylab = "Temperature (degrees Celcius)")

plot(trend_removed_data_matrix_COMPLICATED_LM[20,],type="l",ylim=c(-15,15),main="Marrawah: Detrended Residual Series",xlab = "Days since January 1st 2000",ylab = "Temperature (degrees Celcius)")


plot(data_matrix[3,],type="l",ylim=c(--10,35),main="Smithton: Raw Temperature Series",xlab = "Days since January 1st 2000",ylab = "Temperature (degrees Celcius)")

plot(trend_removed_data_matrix_COMPLICATED_LM[3,],type="l",ylim=c(-10,10),main="Smithton: Detrended Residual Series",xlab = "Days since January 1st 2000",ylab = "Temperature (degrees Celcius)")


#########################################################
plot(trend_removed_data_matrix_COMPLICATED_LM[60,],type="l",ylim=c(-10,10),xlim=c(40000,50000),main="Casey (92500 hPa): Detrended Residual Series",xlab = "Half days since February 4th 1954",ylab = "Temperature (degrees Celcius)")
plot(trend_removed_data_matrix_COMPLICATED_LM[46,],type="l",ylim=c(-10,10),xlim=c(40000,50000),main="Casey (1000 hPa): Detrended Residual Series",xlab = "Half days since February 4th 1954",ylab = "Temperature (degrees Celcius)")


#for(q in 1:NUM_STATIONS){
  
  #trend_removed_data_matrix_COMPLICATED_LM[q,]=trend_removed_data_matrix_COMPLICATED_LM[q,]/Smoothed_std_error_curves[[q]]
  
#}
plot(trend_removed_data_matrix_COMPLICATED_LM_Scaled[60,],type="l",ylim=c(-10,10),xlim=c(40000,50000),main="Casey (92500 hPa): Detrended Residual Series",xlab = "Half days since February 4th 1954",ylab = "Temperature (degrees Celcius)")
plot(trend_removed_data_matrix_COMPLICATED_LM_Scaled[46,],type="l",ylim=c(-10,10),xlim=c(40000,50000),main="Casey (1000 hPa): Detrended Residual Series",xlab = "Half days since February 4th 1954",ylab = "Temperature (degrees Celcius)")


#for(q in 1:NUM_STATIONS){
  
  #trend_removed_data_matrix_COMPLICATED_LM[q,]=trend_removed_data_matrix_COMPLICATED_LM[q,]*Smoothed_std_error_curves[[q]]
  
#}
###################################################################
#MSE stuff 

MSE_1_Parrella
MSE_1_period_removed_vec_FILTERED_NO_NOISE
mean(MSE_1_KALMAN)
MSE_1_period_removed_vec_LM_ONLY
MSE_1_period_removed_Linear_Interpolation



MSE_1_Parrella[c(1,2,14,15,16,17,29,30)+30]
MSE_1_period_removed_vec_FILTERED_NO_NOISE[c(1,2,14,15,16,17,29,30)+30]
MSE_1_KALMAN[c(1,2,14,15,16,17,29,30)+30]
MSE_1_period_removed_vec_LM_ONLY[c(1,2,14,15,16,17,29,30)+30]
MSE_1_period_removed_Linear_Interpolation[c(1,2,14,15,16,17,29,30)+30]


trend_removed_data_matrix_COMPLICATED_LM_Scaled[is.na(trend_removed_data_matrix_COMPLICATED_LM_Scaled)]=0
cor(t(trend_removed_data_matrix_COMPLICATED_LM_Scaled[c(1,2,14,15,16,17,29,30)+30,]))

 
sum(abs(cor(t(trend_removed_data_matrix_COMPLICATED_LM_Scaled[,])))-diag(1,60))/60/60


array_to_LaTeX <- function(arr){
  rows <- apply(arr, MARGIN=1, paste, collapse = " & ")
  matrix_string <- paste(rows, collapse = " \\\\ ")
  return(paste("\\begin{bmatrix}", matrix_string, "\\end{bmatrix}"))
}

trend_removed_data_matrix_COMPLICATED_LM_Scaled[is.na(trend_removed_data_matrix_COMPLICATED_LM_Scaled)]=0
cat(array_to_LaTeX(round(cor(t(trend_removed_data_matrix_COMPLICATED_LM_Scaled[c(1,2,14,15,16,17,29,30)+30,])),digits = 3
)))


par(mfrow=c(1,1))
plot(MSE_1_Parrella,type="l",col=1,ylim=c(0,25), ylab =" MSE", main="MSE Comparison of Imputation Methods P/P+/K/L",xlab = c(" Mawson                    Davis                        Mirnyj                     Casey"))
par(new=TRUE)       
plot(MSE_1_period_removed_vec_FILTERED_NO_NOISE,type="l",ylab =" MSE", col=2,ylim=c(0,25),xlab="")
par(new=TRUE)
plot(MSE_1_KALMAN,type="l",col=3,ylim=c(0,25), ylab =" MSE",xlab="")
par(new=TRUE)
plot(MSE_1_period_removed_Linear_Interpolation, ylab =" MSE",type="l",col=5,ylim=c(0,25),xlab="")


abline(v=15, col="purple",lty=1)
abline(v=30, col="purple",lty=1)
abline(v=45, col="purple",lty=1)
legend(35,25, legend=c("P-imputation", "P-imputation+Filtering", "K-imputation estimates", "Linear interpolation estimates"),
       col=c(1:4), lty=c(1,1,1,1), cex=0.8)



 
par(new=TRUE)
plot(MSE_1_period_removed_vec_LM_ONLY,type="l",col=4,ylim=c(0,25))
 


 

#Correlation matrix 


trend_removed_data_matrix_COMPLICATED_LM_Scaled[is.na(trend_removed_data_matrix_COMPLICATED_LM_Scaled)]=0
cor(t(trend_removed_data_matrix_COMPLICATED_LM))

par(mfrow=c(2,2))
acf(trend_removed_data_matrix_COMPLICATED_LM_Scaled[46,], main="Casey 1000hPA Scaled Residual Temperature Series ACF")
#acf(trend_removed_data_matrix_COMPLICATED_LM_Scaled[47,], main="Cape Grim Scaled Residual Temperature Series ACF")
#acf(trend_removed_data_matrix_COMPLICATED_LM_Scaled[59,], main="Smithton Scaled Residual Temperature Series ACF")
acf(trend_removed_data_matrix_COMPLICATED_LM_Scaled[60,], main="Casey 92500hPA Scaled Residual Temperature Series ACF")


pacf(trend_removed_data_matrix_COMPLICATED_LM_Scaled[46,] , main="Casey 1000hPA Scaled Residual Temperature Series PACF")
#pacf(trend_removed_data_matrix_COMPLICATED_LM_Scaled[47,], main="Cape Grim Scaled Residual Temperature Series PACF")
#pacf(trend_removed_data_matrix_COMPLICATED_LM_Scaled[59,], main="Smithton Scaled Residual Temperature Series PACF")
pacf(trend_removed_data_matrix_COMPLICATED_LM_Scaled[60,], main="Casey 92500hPA Scaled Residual Temperature Series PACF")




acf(trend_removed_data_matrix_COMPLICATED_LM[1,], main="Marrawah Scaled Residual Temperature Series ACF")
acf(trend_removed_data_matrix_COMPLICATED_LM[3,], main="Smithton Scaled Residual Temperature Series ACF")


pacf(trend_removed_data_matrix_COMPLICATED_LM[1,] , main="Marrawah Scaled Residual Temperature Series PACF")
pacf(trend_removed_data_matrix_COMPLICATED_LM[3,], main="Smithton Scaled Residual Temperature Series PACF")


#reconstruted series
par(mfrow=c(3,1))
plot(data_matrix[46,],ylim=c(-90,0),main="Casey 1000hPA: Raw Temperature Series",ylab = "Temperature (degrees Celcius)", xlab = "Half days since February 4th 1954")
plot(retrended_data_matrix[46,],ylim=c(-90,0),main="Casey 1000hPA: P-imputation Reconstructed Temperature Series",ylab = "Temperature (degrees Celcius)", xlab = "Half days since February 4th 1954")
plot(retrended_data_matrix_kalman[46,],ylim=c(-90,0),main="Casey 1000hPA: K-imputation Reconstructed Temperature Series",ylab = "Temperature (degrees Celcius)", xlab = "Half days since February 4th 1954")


par(mfrow=c(3,1))
plot(data_matrix[60,],ylim=c(-40,10),main="Casey 92500hPA: Raw Temperature Series",ylab = "Temperature (degrees Celcius)", xlab = "Half days since February 4th 1954")
plot(retrended_data_matrix[60,],ylim=c(-40,10),main="Casey 92500hPA: P-imputation Reconstructed Temperature Series",ylab = "Temperature (degrees Celcius)", xlab = "Half days since February 4th 1954")
plot(retrended_data_matrix_kalman[60,],ylim=c(-40,10),main="Casey 92500hPA: K-imputation Reconstructed Temperature Series",ylab = "Temperature (degrees Celcius)", xlab = "Half days since February 4th 1954")


#calculating missingness rate
p=0
for(i in 1:60){
  p=p+sum(is.na(data_matrix[i,]))/length(data_matrix[i,])
  print(sum(is.na(data_matrix[i,]))/length(data_matrix[i,]))
}
 p/60

 ###################################
 #appendix plots
 
 par(mfrow=c(4,2))
 plot(data_matrix[1,],main="Mawson 92500hPA: Raw Temperature Series",ylab = "Temperature (degrees Celcius)", xlab = "Half days since February 4th 1954")
 plot(data_matrix[15,],main="Mawson 1000hPA: Raw Temperature Series",ylab = "Temperature (degrees Celcius)", xlab = "Half days since February 4th 1954")
 plot(data_matrix[16,],main="Davis 92500hPA: Raw Temperature Series",ylab = "Temperature (degrees Celcius)", xlab = "Half days since February 4th 1954")
 plot(data_matrix[30,],main="Davis 1000hPA: Raw Temperature Series",ylab = "Temperature (degrees Celcius)", xlab = "Half days since February 4th 1954")
 plot(data_matrix[31,],main="Mirnyj 92500hPA: Raw Temperature Series",ylab = "Temperature (degrees Celcius)", xlab = "Half days since February 4th 1954")
 plot(data_matrix[45,-36762],main="Mirnyj 1000hPA: Raw Temperature Series",ylab = "Temperature (degrees Celcius)", xlab = "Half days since February 4th 1954")
 plot(data_matrix[46,],main="Casey 92500hPA: Raw Temperature Series",ylab = "Temperature (degrees Celcius)", xlab = "Half days since February 4th 1954")
 plot(data_matrix[60,],main="Casey 1000hPA: Raw Temperature Series",ylab = "Temperature (degrees Celcius)", xlab = "Half days since February 4th 1954")
 
 
 which.min(data_matrix[45,])
 
 plot(trend_removed_data_matrix_COMPLICATED_LM_Scaled[1,],xlim=c(40000,45000),ylim=c(-5,5),main="Mawson 92500hPA: Detrended & Scaled Series",ylab = "Temperature (degrees Celcius)", xlab = "Half days since February 4th 1954")
 plot(trend_removed_data_matrix_COMPLICATED_LM_Scaled[15,],xlim=c(40000,45000),main="Mawson 1000hPA: Detrended & Scaled Series",ylab = "Temperature (degrees Celcius)", xlab = "Half days since February 4th 1954")
 plot(trend_removed_data_matrix_COMPLICATED_LM_Scaled[16,],xlim=c(40000,45000),main="Davis 92500hPA: Detrended & Scaled Series",ylab = "Temperature (degrees Celcius)", xlab = "Half days since February 4th 1954")
 plot(trend_removed_data_matrix_COMPLICATED_LM_Scaled[30,],xlim=c(40000,45000),main="Davis 1000hPA: Detrended & Scaled Series",ylab = "Temperature (degrees Celcius)", xlab = "Half days since February 4th 1954")
 plot(trend_removed_data_matrix_COMPLICATED_LM_Scaled[31,],xlim=c(40000,45000),main="Mirnyj 92500hPA: Detrended & Scaled Series",ylab = "Temperature (degrees Celcius)", xlab = "Half days since February 4th 1954")
 plot(trend_removed_data_matrix_COMPLICATED_LM_Scaled[45,],xlim=c(40000,45000),main="Mirnyj 1000hPA: Detrended & Scaled Series",ylab = "Temperature (degrees Celcius)", xlab = "Half days since February 4th 1954")
 plot(trend_removed_data_matrix_COMPLICATED_LM_Scaled[46,],xlim=c(40000,45000),main="Casey 92500hPA: Detrended & Scaled Series",ylab = "Temperature (degrees Celcius)", xlab = "Half days since February 4th 1954")
 plot(trend_removed_data_matrix_COMPLICATED_LM_Scaled[60,],xlim=c(40000,45000),main="Casey 1000hPA: Detrended & Scaled Series",ylab = "Temperature (degrees Celcius)", xlab = "Half days since February 4th 1954")
 
 
 plot(retrended_data_matrix[1,],xlim=c(40000,45000),main="Mawson 92500hPA: P-imputed Temperature Series",ylab = "Temperature (degrees Celcius)", xlab = "Half days since February 4th 1954")
 plot(retrended_data_matrix[15,],xlim=c(40000,45000),main="Mawson 1000hPA: P-imputed Temperature Series",ylab = "Temperature (degrees Celcius)", xlab = "Half days since February 4th 1954")
 plot(retrended_data_matrix[16,],xlim=c(40000,45000),main="Davis 92500hPA: P-imputed Temperature Series",ylab = "Temperature (degrees Celcius)", xlab = "Half days since February 4th 1954")
 plot(retrended_data_matrix[30,],xlim=c(40000,45000),main="Davis 1000hPA: P-imputed Temperature Series",ylab = "Temperature (degrees Celcius)", xlab = "Half days since February 4th 1954")
 plot(retrended_data_matrix[31,],xlim=c(40000,45000),main="Mirnyj 92500hPA: P-imputed Temperature Series",ylab = "Temperature (degrees Celcius)", xlab = "Half days since February 4th 1954")
 plot(retrended_data_matrix[45,],xlim=c(40000,45000),main="Mirnyj 1000hPA: P-imputed Temperature Series",ylab = "Temperature (degrees Celcius)", xlab = "Half days since February 4th 1954")
 plot(retrended_data_matrix[46,],xlim=c(40000,45000),main="Casey 92500hPA: P-imputed Temperature Series",ylab = "Temperature (degrees Celcius)", xlab = "Half days since February 4th 1954")
 plot(retrended_data_matrix[60,],xlim=c(40000,45000),main="Casey 1000hPA: P-imputed Temperature Series",ylab = "Temperature (degrees Celcius)", xlab = "Half days since February 4th 1954")
  
 
 
 plot(retrended_data_matrix[1,],xlim=c(1000,5000),main="Mawson 92500hPA: P-imputed Temperature Series",ylab = "Temperature (degrees Celcius)", xlab = "Half days since February 4th 1954")
 plot(retrended_data_matrix[15,],xlim=c(1000,5000),main="Mawson 1000hPA: P-imputed Temperature Series",ylab = "Temperature (degrees Celcius)", xlab = "Half days since February 4th 1954")
 plot(retrended_data_matrix[16,],xlim=c(1000,5000),main="Davis 92500hPA: P-imputed Temperature Series",ylab = "Temperature (degrees Celcius)", xlab = "Half days since February 4th 1954")
 plot(retrended_data_matrix[30,],xlim=c(1000,5000),main="Davis 1000hPA: P-imputed Temperature Series",ylab = "Temperature (degrees Celcius)", xlab = "Half days since February 4th 1954")
 plot(retrended_data_matrix[31,],xlim=c(1000,5000),main="Mirnyj 92500hPA: P-imputed Temperature Series",ylab = "Temperature (degrees Celcius)", xlab = "Half days since February 4th 1954")
 plot(retrended_data_matrix[45,],xlim=c(1000,5000),main="Mirnyj 1000hPA: P-imputed Temperature Series",ylab = "Temperature (degrees Celcius)", xlab = "Half days since February 4th 1954")
 plot(retrended_data_matrix[46,],xlim=c(1000,5000),main="Casey 92500hPA: P-imputed Temperature Series",ylab = "Temperature (degrees Celcius)", xlab = "Half days since February 4th 1954")
 plot(retrended_data_matrix[60,],xlim=c(1000,5000),main="Casey 1000hPA: P-imputed Temperature Series",ylab = "Temperature (degrees Celcius)", xlab = "Half days since February 4th 1954")
 
 
 
 
 plot(retrended_data_matrix_kalman[1,],xlim=c(40000,45000),main="Mawson 92500hPA: K-imputed Temperature Series",ylab = "Temperature (degrees Celcius)", xlab = "Half days since February 4th 1954")
 plot(retrended_data_matrix_kalman[15,],xlim=c(40000,45000),main="Mawson 1000hPA: K-imputed Temperature Series",ylab = "Temperature (degrees Celcius)", xlab = "Half days since February 4th 1954")
 plot(retrended_data_matrix_kalman[16,],xlim=c(40000,45000),main="Davis 92500hPA: K-imputed Temperature Series",ylab = "Temperature (degrees Celcius)", xlab = "Half days since February 4th 1954")
 plot(retrended_data_matrix_kalman[30,],xlim=c(40000,45000),main="Davis 1000hPA: K-imputed Temperature Series",ylab = "Temperature (degrees Celcius)", xlab = "Half days since February 4th 1954")
 plot(retrended_data_matrix_kalman[31,],xlim=c(40000,45000),main="Mirnyj 92500hPA: K-imputed Temperature Series",ylab = "Temperature (degrees Celcius)", xlab = "Half days since February 4th 1954")
 plot(retrended_data_matrix_kalman[45,],xlim=c(40000,45000),main="Mirnyj 1000hPA: K-imputed Temperature Series",ylab = "Temperature (degrees Celcius)", xlab = "Half days since February 4th 1954")
 plot(retrended_data_matrix_kalman[46,],xlim=c(40000,45000),main="Casey 92500hPA: K-imputed Temperature Series",ylab = "Temperature (degrees Celcius)", xlab = "Half days since February 4th 1954")
 plot(retrended_data_matrix_kalman[60,],xlim=c(40000,45000),main="Casey 1000hPA: K-imputed Temperature Series",ylab = "Temperature (degrees Celcius)", xlab = "Half days since February 4th 1954")
 
 
 
 plot(retrended_data_matrix_kalman[1,],xlim=c(1000,5000),main="Mawson 92500hPA: K-imputed Temperature Series",ylab = "Temperature (degrees Celcius)", xlab = "Half days since February 4th 1954")
 plot(retrended_data_matrix_kalman[15,],xlim=c(1000,5000),main="Mawson 1000hPA: K-imputed Temperature Series",ylab = "Temperature (degrees Celcius)", xlab = "Half days since February 4th 1954")
 plot(retrended_data_matrix_kalman[16,],xlim=c(1000,5000),main="Davis 92500hPA: K-imputed Temperature Series",ylab = "Temperature (degrees Celcius)", xlab = "Half days since February 4th 1954")
 plot(retrended_data_matrix_kalman[30,],xlim=c(1000,5000),main="Davis 1000hPA: K-imputed Temperature Series",ylab = "Temperature (degrees Celcius)", xlab = "Half days since February 4th 1954")
 plot(retrended_data_matrix_kalman[31,],xlim=c(1000,5000),main="Mirnyj 92500hPA: K-imputed Temperature Series",ylab = "Temperature (degrees Celcius)", xlab = "Half days since February 4th 1954")
 plot(retrended_data_matrix_kalman[45,],xlim=c(1000,5000),main="Mirnyj 1000hPA: K-imputed Temperature Series",ylab = "Temperature (degrees Celcius)", xlab = "Half days since February 4th 1954")
 plot(retrended_data_matrix_kalman[46,],xlim=c(1000,5000),main="Casey 92500hPA: K-imputed Temperature Series",ylab = "Temperature (degrees Celcius)", xlab = "Half days since February 4th 1954")
 plot(retrended_data_matrix_kalman[60,],xlim=c(1000,5000),main="Casey 1000hPA: K-imputed Temperature Series",ylab = "Temperature (degrees Celcius)", xlab = "Half days since February 4th 1954")
 
 
 plot(retrended_data_matrix_Linear_Interpolation[1,],xlim=c(40000,45000),main="Mawson 92500hPA: L-imputed Temperature Series",ylab = "Temperature (degrees Celcius)", xlab = "Half days since February 4th 1954")
 plot(retrended_data_matrix_Linear_Interpolation[15,],xlim=c(40000,45000),main="Mawson 1000hPA: L-imputed Temperature Series",ylab = "Temperature (degrees Celcius)", xlab = "Half days since February 4th 1954")
 plot(retrended_data_matrix_Linear_Interpolation[16,],xlim=c(40000,45000),main="Davis 92500hPA: L-imputed Temperature Series",ylab = "Temperature (degrees Celcius)", xlab = "Half days since February 4th 1954")
 plot(retrended_data_matrix_Linear_Interpolation[30,],xlim=c(40000,45000),main="Davis 1000hPA: L-imputed Temperature Series",ylab = "Temperature (degrees Celcius)", xlab = "Half days since February 4th 1954")
 plot(retrended_data_matrix_Linear_Interpolation[31,],xlim=c(40000,45000),main="Mirnyj 92500hPA: L-imputed Temperature Series",ylab = "Temperature (degrees Celcius)", xlab = "Half days since February 4th 1954")
 plot(retrended_data_matrix_Linear_Interpolation[45,],xlim=c(40000,45000),main="Mirnyj 1000hPA: L-imputed Temperature Series",ylab = "Temperature (degrees Celcius)", xlab = "Half days since February 4th 1954")
 plot(retrended_data_matrix_Linear_Interpolation[46,],xlim=c(40000,45000),main="Casey 92500hPA: L-imputed Temperature Series",ylab = "Temperature (degrees Celcius)", xlab = "Half days since February 4th 1954")
 plot(retrended_data_matrix_Linear_Interpolation[60,],xlim=c(40000,45000),main="Casey 1000hPA: L-imputed Temperature Series",ylab = "Temperature (degrees Celcius)", xlab = "Half days since February 4th 1954")
 
 
 
 plot(retrended_data_matrix_Linear_Interpolation[1,],xlim=c(1000,5000),main="Mawson 92500hPA: L-imputed Temperature Series",ylab = "Temperature (degrees Celcius)", xlab = "Half days since February 4th 1954")
 plot(retrended_data_matrix_Linear_Interpolation[15,],xlim=c(1000,5000),main="Mawson 1000hPA: L-imputed Temperature Series",ylab = "Temperature (degrees Celcius)", xlab = "Half days since February 4th 1954")
 plot(retrended_data_matrix_Linear_Interpolation[16,],xlim=c(1000,5000),main="Davis 92500hPA: L-imputed Temperature Series",ylab = "Temperature (degrees Celcius)", xlab = "Half days since February 4th 1954")
 plot(retrended_data_matrix_Linear_Interpolation[30,],xlim=c(1000,5000),main="Davis 1000hPA: L-imputed Temperature Series",ylab = "Temperature (degrees Celcius)", xlab = "Half days since February 4th 1954")
 plot(retrended_data_matrix_Linear_Interpolation[31,],xlim=c(1000,5000),main="Mirnyj 92500hPA: L-imputed Temperature Series",ylab = "Temperature (degrees Celcius)", xlab = "Half days since February 4th 1954")
 plot(retrended_data_matrix_Linear_Interpolation[45,],xlim=c(1000,5000),main="Mirnyj 1000hPA: L-imputed Temperature Series",ylab = "Temperature (degrees Celcius)", xlab = "Half days since February 4th 1954")
 plot(retrended_data_matrix_Linear_Interpolation[46,],xlim=c(1000,5000),main="Casey 92500hPA: L-imputed Temperature Series",ylab = "Temperature (degrees Celcius)", xlab = "Half days since February 4th 1954")
 plot(retrended_data_matrix_Linear_Interpolation[60,],xlim=c(1000,5000),main="Casey 1000hPA: L-imputed Temperature Series",ylab = "Temperature (degrees Celcius)", xlab = "Half days since February 4th 1954")
 
 
 
 trend_removed_data_matrix_COMPLICATED_LM_Scaled[is.na(trend_removed_data_matrix_COMPLICATED_LM_Scaled)]=0
 cor(t(trend_removed_data_matrix_COMPLICATED_LM_Scaled[]))[1:15,]
 
 #cointegration test results tas+igra2
 #correlation matrix igra 2
  
 cor(t(trend_removed_data_matrix_COMPLICATED_LM_Scaled[]))[1:15,]
A=round(cor(t(trend_removed_data_matrix_COMPLICATED_LM_Scaled[]))[1:15,],3)
 
 
 array_to_LaTeX <- function(arr){
    rows <- apply(arr, MARGIN=1, paste, collapse = " & ")
    matrix_string <- paste(rows, collapse = " \\\\ ")
   return(paste("\\begin{bmatrix}", matrix_string, "\\end{bmatrix}"))}
 
 
 cor(t(trend_removed_data_matrix_COMPLICATED_LM_Scaled[]))[1:15,]
 signif(cor(t(trend_removed_data_matrix_COMPLICATED_LM_Scaled[]))[1:15,],2)
 
 A=round(cor(t(trend_removed_data_matrix_COMPLICATED_LM_Scaled[]))[1:15,],2)
 cat(array_to_LaTeX( t(A)))

 B=round(cor(t(trend_removed_data_matrix_COMPLICATED_LM_Scaled[]))[1:15+15,],2)
 cat(array_to_LaTeX( t(B)))
 
 C=round(cor(t(trend_removed_data_matrix_COMPLICATED_LM_Scaled[]))[1:15+30,],2)
 cat(array_to_LaTeX(t( C)))
 
 D=round(cor(t(trend_removed_data_matrix_COMPLICATED_LM_Scaled[]))[1:15+45,],2)
 cat(array_to_LaTeX( t(D)))
 