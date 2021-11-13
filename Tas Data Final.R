

Marrawah_Data <- read.csv("C:/Users/starc/OneDrive/Desktop/Masters 2020/May 26 Contemporary files/TAS Temp Data For Comparison/Marrawah/IDCJAC0010_091223_1800/IDCJAC0010_091223_1800_Data.csv")

Cape_Grim_Data <- read.csv("C:/Users/starc/OneDrive/Desktop/Masters 2020/May 26 Contemporary files/TAS Temp Data For Comparison/Cape Grim (Woolnorth)/IDCJAC0010_091245_1800/IDCJAC0010_091245_1800_Data.csv")

Smithton_Data=read.csv("C:/Users/starc/OneDrive/Desktop/Masters 2020/MAY 26 Contemporary Files/TAS Temp Data For Comparison/Smithton/IDCJAC0010_091292_1800/IDCJAC0010_091292_1800_Data.csv")

Luncheon_Hill_Data=read.csv("C:/Users/starc/OneDrive/Desktop/Masters 2020/MAY 26 Contemporary Files/TAS Temp Data For Comparison/Luncheon Hill/IDCJAC0010_091259_1800/IDCJAC0010_091259_1800_Data.csv")



#making first date for both series 1985 jan 1st
Marrawah_Data=Marrawah_Data[-(1:5114),]
Smithton_Data
Cape_Grim_Data
?read_csv
#check
dim(Marrawah_Data)
dim(Cape_Grim_Data)

t=1:13286
period=365
length(t)
Marrawah_temps=as.matrix(Marrawah_Data[,6])
dim(Marrawah_temps)
Cape_grim_temps=as.matrix(Cape_Grim_Data[,6])
dim(Cape_grim_temps)
Smithton_temps=as.matrix(Smithton_Data[,6])

Smithton_temps=c(rep(NA,13286-9270),Smithton_temps)

dim(Smithton_temps)
length(Smithton_temps)

Luncheon_Hill_temps=as.matrix(Luncheon_Hill_Data[,6])
dim(Luncheon_Hill_temps)
Luncheon_Hill_temps=c(rep(NA,13286-9636),Luncheon_Hill_temps)
length(Luncheon_Hill_temps)


par(mfrow=c(4,1))


plot(t,Marrawah_temps)
plot(t,Cape_grim_temps)
plot(t,Smithton_temps)
plot(t,Luncheon_Hill_temps)

Cape_grim_temps[5000:5100]

Marrawah_temps[5000:5100]












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


prune_this_numbr_of_first_entries=4000
min_time=1
min_time


max_time=13286-prune_this_numbr_of_first_entries

#this needs to be reset manually based on above...
#I HAVE CHANGED THIS TO ACTUALLY MEAN THE MIN-MAX TIMES OBSERVED OVER ALL SERIES...
longest_num_obs=length(min_time:max_time) 

longest_num_obs
#this needs to be reset manually based on how many stations we consider...
NUM_STATIONS=4
#reset manually 

AllTimes=seq(min_time,max_time,by=1)
AllTimes
length(AllTimes)
data_matrix=as.matrix(cbind(Marrawah_temps,Cape_grim_temps,Smithton_temps,Luncheon_Hill_temps))
dim(data_matrix)
data_matrix=t(data_matrix)
data_matrix=apply(data_matrix, 2, as.numeric)


dim(data_matrix)
labels(data_matrix)

data_matrix[3,5000:6000]

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

labels(as.array(data_matrix)[1,])



#####################################
#further pruning data matrix to achieve almost full sampling...
#####################################
data_matrix=data_matrix[ ,-(1:prune_this_numbr_of_first_entries)]
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


#data_matrix[1,1000:1100]=NA


data_matrix[1,1:3000]=NA
#data_matrix[2,1:3000+4000]=NA
data_matrix[3,1:3000+3000]=NA
#data_matrix[4,1:3000+1000]=NA
NUM_STATIONS


Num_reps=10
MSE_L_Imputation_Matrix=matrix(NA,nrow =NUM_STATIONS,ncol = Num_reps)
MSE_N_Imputation_Matrix=matrix(NA,nrow =NUM_STATIONS,ncol = Num_reps)
MSE_K_Imputation_Matrix=matrix(NA,nrow =NUM_STATIONS,ncol = Num_reps)
MSE_P_Imputation_Matrix=matrix(NA,nrow =NUM_STATIONS,ncol = Num_reps)
MSE_P_plus_filter_Imputation_Matrix=matrix(NA,nrow =NUM_STATIONS,ncol = Num_reps)

for(e in 1:Num_reps){


######################################
#Allocating to a test set here
######################################

#We take one years worth of samples (possibly NA) across the data matrix at random locations and store them in a testing set, then delete this observation
Test_SET_locations_matrix=matrix(NA,NUM_STATIONS,period)



for(i in 1:NUM_STATIONS){
  Test_SET_locations_matrix[i,]=sample(1:dim(data_matrix)[2],period)
}

Test_SET_matrix_1_Period=matrix(NA,NUM_STATIONS,period)

for(i in 1:NUM_STATIONS){
  Test_SET_matrix_1_Period[i,]=known_data_matrix[i,Test_SET_locations_matrix[i,]]
}





for(i in 1:NUM_STATIONS){
  data_matrix[i,Test_SET_locations_matrix[i,]]=NA
}



#######################################

#plots
par(mfrow=c(4,1))
plot(data_matrix[1,])
plot(data_matrix[2,])
plot(data_matrix[3,])
plot(data_matrix[4,])



par(mfrow=c(1,1))


index1=1
for( i in 1:5){
  
  
  
  #plot(retrended_data_matrix[i,],type='l',ylim=c(-100,10),main=i)
  plot(data_matrix[index1,],ylim=c(-10,40),xlim=c(((i-1)*2000+1),(i)*2000))
  
  
}

index1=2
for( i in 1:5){
  
  
  
  #plot(retrended_data_matrix[i,],type='l',ylim=c(-100,10),main=i)
  plot(data_matrix[index1,],ylim=c(-10,40),xlim=c(((i-1)*2000+1),(i)*2000))
  
  
}

index1=3
for( i in 1:5){
  
  
  
  #plot(retrended_data_matrix[i,],type='l',ylim=c(-100,10),main=i)
  plot(data_matrix[index1,],ylim=c(-10,40),xlim=c(((i-1)*2000+1),(i)*2000))
  
  
}


index1=4
for( i in 1:5){
  
  
  
  #plot(retrended_data_matrix[i,],type='l',ylim=c(-100,10),main=i)
  plot(data_matrix[index1,],ylim=c(-10,40),xlim=c(((i-1)*2000+1),(i)*2000))
  
  
}



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

summary(ur.kpss(na.omit(data_matrix[1,]),type="mu",use.lag=5))
summary(ur.kpss(na.omit(data_matrix[1,]),type="tau",use.lag=5))

kpss.test(na.omit(data_matrix[1,]),null="Trend",lshort=TRUE)


#generally REJECT null (that series is stationary) SO WE DO NEED TO DETREND/DIFFERENCE...
#test#2 
adf.test(na.omit(data_matrix[1,]),k=1)
?adf.test
#reject null (that series is nonstationary) CONTRADICTORY...HOWEVER, NOTE WE HAD TO REMOVE NA VALUES AND HENCE NULL HYPOTHESIS OF NON-STATIONARITY IS INHERENTLY VIOLATED...

#Use a kpss test assuming stationary around a trend, generally we accept the hypothesis that there is trend-stationarity!!
kpss_p_values_raw_data=rep(NA,(NUM_STATIONS*1-num_removals))
for(i in 1:(NUM_STATIONS*1-num_removals)){kpss_p_values_raw_data[i]=kpss.test(na.omit(data_matrix[i,]),null="Trend",lshort=TRUE)[[3]]
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


phase_vectors=rep(NA,NUM_STATIONS*1-num_removals)

for(i in 1:(NUM_STATIONS*1-num_removals)){
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
  
  plot((model1$coefficients[1]+model1$coefficients[2]*t+model1$coefficients[3]*cosine_series+model1$coefficients[4]*sine_series+model1$coefficients[5]*cosine_series2+model1$coefficients[6]*sine_series2+model1$coefficients[7]*cosine_series3+model1$coefficients[8]*sine_series3+model1$coefficients[9]*cosine_series4+model1$coefficients[10]*sine_series4),col="red",ylim=c(0,50))
  
  par(new=TRUE)
  plot(data_matrix[i,],ylim=c(0,50))
  
  
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
plot(model1$coefficients[1]+model1$coefficients[2]*t+model1$coefficients[3]*cosine_series+model1$coefficients[4]*sine_series+model1$coefficients[5]*cosine_series2+model1$coefficients[6]*sine_series2+model1$coefficients[7]*cosine_series3+model1$coefficients[8]*sine_series3+model1$coefficients[9]*cosine_series4+model1$coefficients[10]*sine_series4,xlim=c(1,2000))

#plot(model1$coefficients[1]+model1$coefficients[2]*t+model1$coefficients[3]*cosine_series+model1$coefficients[4]*sine_series+model1$coefficients[5]*cosine_series2+model1$coefficients[6]*sine_series2+model1$coefficients[7]*cosine_series3+model1$coefficients[8]*sine_series3+model1$coefficients[9]*cosine_series4+model1$coefficients[10]*sine_series4+model1$coefficients[11]*cosine_series5+model1$coefficients[12]*sine_series5+model1$coefficients[13]*cosine_series6+model1$coefficients[14]*sine_series6+model1$coefficients[15]*cosine_series7+model1$coefficients[16]*sine_series7+model1$coefficients[17]*cosine_series8+model1$coefficients[18]*sine_series8+model1$coefficients[19]*cosine_series9+model1$coefficients[20]*sine_series9+model1$coefficients[21]*cosine_series10+model1$coefficients[22]*sine_series10,xlim=c(1,4000))
#We see that complicated lm someties seems to have too large an amplitude??


#plots to see phase behviour...
for(i in 1:(NUM_STATIONS*1-num_removals)){
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
  
  
  plot(xlim=c(1,365*2),(model1$coefficients[1]+model1$coefficients[2]*t+model1$coefficients[3]*cosine_series+model1$coefficients[4]*sine_series+model1$coefficients[5]*cosine_series2+model1$coefficients[6]*sine_series2+model1$coefficients[7]*cosine_series3+model1$coefficients[8]*sine_series3+model1$coefficients[9]*cosine_series4+model1$coefficients[10]*sine_series4),col=i)
  
  par(new=TRUE)
  
}

#diagnostic plots
#checking fit....
par(mfrow=c(2,2))

for(i in 1:(NUM_STATIONS*1-num_removals)){
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
par(mfrow=c(4,2))

for(i in 1:(NUM_STATIONS*1-num_removals)){
  plot(data_matrix[i,],type="l",ylim=c(--10,50),main=i)
  
  plot(trend_removed_data_matrix_COMPLICATED_LM[i,],type="l",ylim=c(-10,50),main=i)
  
}


for(i in 1:(NUM_STATIONS*1-num_removals)){
  plot(data_matrix[i,],type="l",ylim=c(-10,50),main=i,xlim=c(4500,4550))
  
  plot(trend_removed_data_matrix_COMPLICATED_LM[i,],type="l",ylim=c(-10,50),main=i,xlim=c(4500,4550))
  
}


#overalapping plots to check for phase
for(i in 1:(NUM_STATIONS*1-num_removals-1)){
  plot(data_matrix[i,],type="l",ylim=c(-10,50),main=i,xlim=c(4500,5800))
  par(new=TRUE)
  plot(data_matrix[i+1,],type="l",ylim=c(-10,50),main=i,xlim=c(4500,5800),col="red")
  
}

for(i in 1:(NUM_STATIONS*1-num_removals-1)){
  plot(data_matrix[i,],type="l",ylim=c(-10,50),main=i)
  par(new=TRUE)
  plot(data_matrix[i+1,],type="l",ylim=c(-10,50),main=i,col="red")
  
}


############################################################################################################################
############################################################################################################################
#Univariate stationarity tests
############################################################################################################################
############################################################################################################################


trend_removed_data_matrix_COMPLICATED_LM[trend_removed_data_matrix_COMPLICATED_LM==0]=NA
#performing two popular tests... (null is stationary and nonstationary respectively)  
#test#1
test_stats_trend_removed_complicated_LM=rep(NA,(NUM_STATIONS*1-num_removals))
for(i in 1:(NUM_STATIONS*1-num_removals)){test_stats_trend_removed_complicated_LM[i]=summary(ur.kpss(trend_removed_data_matrix_COMPLICATED_LM[i,],lags = "nil",type = "mu"))@teststat}
test_stats_trend_removed_complicated_LM
summary(ur.kpss(trend_removed_data_matrix_COMPLICATED_LM[i,],lags = "nil",type = "mu"))
?ur.kpss()
#Note 0.463 is the 5% Confidence level cutoff
#THis seems to do a better job than STL??!
#NOTE-we use a mu test as we're assuming we removed linear term with lm...

#IF we use more lags we achieve:
test_stats_trend_removed_complicated_LM_WITH_LAGS=rep(NA,(NUM_STATIONS*1-num_removals))
for(i in 1:(NUM_STATIONS*1-num_removals)){test_stats_trend_removed_complicated_LM_WITH_LAGS[i]=summary(ur.kpss(trend_removed_data_matrix_COMPLICATED_LM[i,],use.lag=1, type = "mu"))@teststat}
test_stats_trend_removed_complicated_LM_WITH_LAGS
#many are stationary with just 1 lag :) :) :) 



test_stats_trend_removed_complicated_LM_WITH_MANY_LAGS=rep(NA,(NUM_STATIONS*1-num_removals))
for(i in 1:(NUM_STATIONS*1-num_removals)){test_stats_trend_removed_complicated_LM_WITH_MANY_LAGS[i]=summary(ur.kpss(trend_removed_data_matrix_COMPLICATED_LM[i,], type = "mu"))@teststat}
test_stats_trend_removed_complicated_LM_WITH_MANY_LAGS
#mostly accept stationarity 

#what does ADF have to say? NOTE lag order has been set to 1
test_stats_trend_removed_complicated_LM_ADF=rep(NA,(NUM_STATIONS*1-num_removals))
for(i in 1:(NUM_STATIONS*1-num_removals)){test_stats_trend_removed_complicated_LM_ADF[i]=adf.test(na.omit(trend_removed_data_matrix_COMPLICATED_LM[i,],k=1))[[3]]}
test_stats_trend_removed_complicated_LM_ADF
#THEY ALL INDICATE STATIONARY TOO!!
#hence if we difference the data an appropriate number of times we should be confident of stationarity







#but does it remove periodicity?? We can't check properly but make NAs zero and then do an acf... should still see basterdised peaks
trend_removed_data_matrix_COMPLICATED_LM[is.na(trend_removed_data_matrix_COMPLICATED_LM)]=0
for(i in 1:(NUM_STATIONS*1-num_removals)) { 
  plot(acf(trend_removed_data_matrix_COMPLICATED_LM[i,],lag.max = 1600),main=i)
}

#GENERALLY DOES A BETTER JOB AT REMOVING PERIODICITY??!  
#HOWEVER, we see significant lags... indicating AR(p) model may be required... (i.e. we need to difference to achieve stationarity...)


############################################
#we use PACF to see if an AR model is appropriate for residuals...
#############################################
#NOTE, since data values are missing, I set them to zero... may give very spurious results...
trend_removed_data_matrix_COMPLICATED_LM[is.na(trend_removed_data_matrix_COMPLICATED_LM)]=0

pacf(trend_removed_data_matrix_COMPLICATED_LM[1,],lag.max = 1000)
pacf(trend_removed_data_matrix_COMPLICATED_LM[1,],lag.max = 10)


#The below shows our data is basically already centred...
mean(trend_removed_data_matrix_COMPLICATED_LM[1,])

mean(trend_removed_data_matrix_COMPLICATED_LM[2,])

mean(trend_removed_data_matrix_COMPLICATED_LM[3,])


#now we look at pacf of y^2 plots to test for GARCH behaviour...??
pacf(trend_removed_data_matrix_COMPLICATED_LM[1,]^2,lag.max = 1000)
pacf(trend_removed_data_matrix_COMPLICATED_LM[1,]^2,lag.max = 10)


trend_removed_data_matrix_COMPLICATED_LM[trend_removed_data_matrix_COMPLICATED_LM==0]=NA





############################################
############################################
#Finding and scaling by stratified variance/variance function
#############################################
#############################################
library(fda)

trend_removed_data_matrix_COMPLICATED_LM[trend_removed_data_matrix_COMPLICATED_LM==0]=NA
period=365
num_times=9286
num_basis_elts=10
NUM_STATIONS
daybasis65 <- create.fourier.basis(rangeval=c(0, period), nbasis=num_basis_elts)
day.5
####################################
#cutting up observations into slices
####################################
trend_removed_data_matrix_COMPLICATED_LM_LIST=list()
for(i in 1:NUM_STATIONS){trend_removed_data_matrix_COMPLICATED_LM_LIST[[i]]=matrix(rep(NA,floor(num_times/period)*period),floor(num_times/period),period)}

for(i in 1:NUM_STATIONS){
  
  for(j in 1:(num_times/period)){
    trend_removed_data_matrix_COMPLICATED_LM_LIST[[i]][j,]=trend_removed_data_matrix_COMPLICATED_LM[i,((period*(j-1)+1):(period*j))]
  }
  
}


####################################
#daily stratified standard errors - this works best!!
####################################
Daily_std_error_est_list=list()
for(k in 1:NUM_STATIONS){
  Daily_std_error_est_list[[k]]=rep(NA,period)
  
}



for(k in 1:NUM_STATIONS){
  for(i in 1:period){
    Daily_std_error_est_list[[k]][i]=sd(trend_removed_data_matrix_COMPLICATED_LM_LIST[[k]][,i],na.rm = TRUE)
  }
}


plot(Daily_std_error_est_list[[1]])
plot(Daily_std_error_est_list[[2]])
plot(Daily_std_error_est_list[[3]])
plot(Daily_std_error_est_list[[4]])

#scaling residual series... 
#1:6/1:2
par(mfrow=c(2,1))
#plot(trend_removed_data_matrix_COMPLICATED_LM[1,]/Daily_std_error_est_list[[1]])
plot(trend_removed_data_matrix_COMPLICATED_LM[1,]/Daily_std_error_est_list[[1]],type="l")
#plot(trend_removed_data_matrix_COMPLICATED_LM[1,])
plot(trend_removed_data_matrix_COMPLICATED_LM[1,],type='l')

#plot(trend_removed_data_matrix_COMPLICATED_LM[2,]/Daily_std_error_est_list[[2]])
plot(trend_removed_data_matrix_COMPLICATED_LM[2,]/Daily_std_error_est_list[[2]],type="l")
#plot(trend_removed_data_matrix_COMPLICATED_LM[2,])
plot(trend_removed_data_matrix_COMPLICATED_LM[2,],type='l')

#plot(trend_removed_data_matrix_COMPLICATED_LM[3,]/Daily_std_error_est_list[[3]])
plot(trend_removed_data_matrix_COMPLICATED_LM[3,]/Daily_std_error_est_list[[3]],type="l")
#plot(trend_removed_data_matrix_COMPLICATED_LM[3,])
plot(trend_removed_data_matrix_COMPLICATED_LM[3,],type='l')

#plot(trend_removed_data_matrix_COMPLICATED_LM[4,]/Daily_std_error_est_list[[4]])
plot(trend_removed_data_matrix_COMPLICATED_LM[4,]/Daily_std_error_est_list[[4]],type="l")
#plot(trend_removed_data_matrix_COMPLICATED_LM[4,])
plot(trend_removed_data_matrix_COMPLICATED_LM[4,],type='l')

#############comment the following line on/off to use daily stratified approach... ############3

#for(k in 1:NUM_STATIONS){trend_removed_data_matrix_COMPLICATED_LM[k,]=trend_removed_data_matrix_COMPLICATED_LM[k,]/Daily_std_error_est_list[[k]]
#}
##########################################################################


####################################
# smooth variance function approach - use for interpretability purposes and out of necessity (not all days obseved introduces NA's...)
####################################


Smoothed_std_error_curves=list()

for(i in 1:(NUM_STATIONS)){
  t=1:period
  w=2*pi/365
  
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




for(k in 1:(NUM_STATIONS)){
  plot(Smoothed_std_error_curves[[k]])
}
#before
plot(trend_removed_data_matrix_COMPLICATED_LM[1,])

for(q in 1:NUM_STATIONS){
  
  trend_removed_data_matrix_COMPLICATED_LM[q,]=trend_removed_data_matrix_COMPLICATED_LM[q,]/Smoothed_std_error_curves[[q]]
  
}
#after
plot(trend_removed_data_matrix_COMPLICATED_LM[1,])

####################################
#FDA smooth variance function approach - use for interpretability purposes
####################################

#daytempfd_VEC=list()
#tempmeanfd_VEC=list()
#tempstdvfd_VEC=list()

#for(p in 1:NUM_STATIONS){
# for(h in 1:floor(num_times/period)){
#   trend_removed_data_matrix_COMPLICATED_LM_LIST[[p]]=trend_removed_data_matrix_COMPLICATED_LM_LIST[[p]][complete.cases(trend_removed_data_matrix_COMPLICATED_LM_LIST[[p]]),]
#}
#}

#trend_removed_data_matrix_COMPLICATED_LM_LIST[[1]][1:365]


#for(k in 1:NUM_STATIONS){
#daytempfd_VEC[[k]] <- Data2fd(argvals=(1:period),y =t(trend_removed_data_matrix_COMPLICATED_LM_LIST[[k]]),basisobj = daybasis65 )

#tempmeanfd_VEC[[k]]= mean.fd(daytempfd_VEC[[k]])
#tempstdvfd_VEC[[k]]= sd.fd(daytempfd_VEC[[k]])

# #plot(tempmeanfd_VEC[[k]], main="k")

##plot(tempstdvfd_VEC[[k]], main="k")
#}

#mean of resdiduals under fourier
#plot(tempmeanfd_VEC[[1]])
#plot(tempmeanfd_VEC[[2]])
#plot(tempmeanfd_VEC[[3]])
#plot(tempmeanfd_VEC[[4]])

#sd of resdiduals under fourier
#plot(tempstdvfd_VEC[[1]])
#plot(tempstdvfd_VEC[[2]])
#plot(tempstdvfd_VEC[[3]])
#plot(tempstdvfd_VEC[[4]])



#for(q in 1:NUM_STATIONS){
#omega=2*pi/period

#d=1:num_times
#f=eval.fd((1:period),tempstdvfd_VEC[[q]])
#plot(f,xlim=c(1,1200),main=q)
#plot(f,xlim=c(1,365),main=q)
#trend_removed_data_matrix_COMPLICATED_LM[q,]=trend_removed_data_matrix_COMPLICATED_LM[q,]/as.numeric(f)

#}



#par(mfrow=c(2,1))

#I haven't stuffed up below, just been lazy in this version and directly changed defn of trend removed comp lm above... Hene the multiplication rather than deivison by f
#with saling
#plot(trend_removed_data_matrix_COMPLICATED_LM[q,],type="l",xlim=c(1,4000))

#without scaling
#plot(trend_removed_data_matrix_COMPLICATED_LM[q,]*as.numeric(f),type="l",xlim=c(1,4000))

#plot(trend_removed_data_matrix_COMPLICATED_LM[q,],type="l",xlim=c(1,1200))


#plot(trend_removed_data_matrix_COMPLICATED_LM[q,]*as.numeric(f),type="l",xlim=c(1,1200))
#with scaling
#plot(trend_removed_data_matrix_COMPLICATED_LM[q,],type="l",xlim=c(1,10000))

#without scaling
#plot(trend_removed_data_matrix_COMPLICATED_LM[q,]*as.numeric(f),type="l",xlim=c(1,10000))

##############################################
##############################################
##############################################


#####################################################
#####################################################
#Fitting spatial weight matrix
#####################################################
#####################################################


#Trend_Removed_data_matrix_with_zero_for_NAs=trend_removed_data_matrix_COMPLICATED_LM
#Trend_Removed_data_matrix_with_zero_for_NAs[is.na(Trend_Removed_data_matrix_with_zero_for_NAs)]=0

#X=cor(t(Trend_Removed_data_matrix_with_zero_for_NAs))-diag(rep(1,dim(Trend_Removed_data_matrix_with_zero_for_NAs)[1]))
#View(X)
#R=t(apply(X, 1, sort,decreasing=TRUE)[ 1:(20), ])  # 
#Q=t(apply(X, 1, order,decreasing=TRUE)[ 1:(20), ])  # 


#E=matrix(rep(0,dim(Trend_Removed_data_matrix_with_zero_for_NAs)[1]^2),dim(Trend_Removed_data_matrix_with_zero_for_NAs)[1],dim(Trend_Removed_data_matrix_with_zero_for_NAs)[1])

#for(i in 1:(NUM_STATIONS*1-num_removals)){
# E[i,Q[i,]]=R[i,]
#E[Q[i,],i]=R[i,]
#}


#E
#W=E
#W[W>0]=1
#W[W<0]=0
#W=normalize.rows(E,method = "manhattan")





############################################################
#W=matrix(rep(1,dim(Trend_Removed_data_matrix_with_zero_for_NAs)[1]^2),dim(Trend_Removed_data_matrix_with_zero_for_NAs)[1],dim(Trend_Removed_data_matrix_with_zero_for_NAs)[1])
#W=W-diag(1,dim(Trend_Removed_data_matrix_with_zero_for_NAs)[1])
#W=normalize.rows(W,method="manhattan")
##################################################



#W based on euclidean distance



#distance_calc=function(alt1,lat1,lon1,alt2,lat2,lon2){

#lat_1 = lat1 * (pi / 180);
# lon_1 = lon1 *(pi/180);
# alt_1 = alt1;

# lat_2 = lat2 * (pi / 180);
# lon_2 = lon2 * (pi / 180);
# alt_2 = alt2;

#  r = 6376.5 *1000;  
#  r1=r+alt_1
# r2=r+alt_2

# x_1 = r1 * sin(lon_1) * cos(lat_1);
#y_1 = r1 * sin(lon_1) * sin(lat_1);
# z_1 = r1* cos(lon_1);

# x_2 = r2 * sin(lon_2) * cos(lat_2);
# y_2 = r2 * sin(lon_2) * sin(lat_2);
# z_2 = r2 * cos(lon_2);

#  dist = sqrt((x_2 - x_1) * (x_2 - x_1) + (y_2 - y_1) * (y_2 - y_1) + (z_2 - z_1) * (z_2 - z_1));
# return(dist)
#}




#pressure_vector=c(1000,2000,3000,5000,7000,10000,15000,20000,25000,30000,40000,50000,70000,85000,92500)

#height_vector=c(32328,26399,24070,20501,18178,15238,12600,10961,9808,8650 ,6739,5099,2618,1247,468)


#latitude_vec=c(-71.8,-70.67,-85,-75.52,-62.2,-64.23,-70.77,-69,-67.67,-67.6,-68.58,-66.55,-78.45-66.25)

#longitude_vec=c(-2.78,-8.25,0,-26.6,-58.93,-56.72,11.83,39.58,45.85,62.88,77.98,93.02,106.87,110.53)



#Essentially inverting ddistances betweeen stations to obtain spatial weight matrix  


#W_unormalised=matrix(rep(NA,14^2*1^2),length(c(3,7))*1,length(c(3,7))*1)
#W_unormalised



#for(i in 1:NUM_STATIONS){
#for(j in 1:NUM_STATIONS){
# for(k in 1:1){
#   for(l in 1:1){



#   W_unormalised[((i-1)*1+k),((j-1)*1+l)]=1/(1+distance_calc(height_vector[k],latitude_vec[i],longitude_vec[j],height_vector[l],latitude_vec[i],longitude_vec[j]))

#  }

#  }
# }

#}
#removed_station_vector_initial



#W_unormalised=W_unormalised[removed_station_vector_initial,removed_station_vector_initial]
#W_unormalised=W_unormalised-diag(1,dim(W_unormalised)[1])
#W_unormalised
##We are only using station 3 and 8...
#W=normalize.rows(W_unormalised,method="manhattan")
#W

#################
##W based on slightly positive correlations - all set to 1 if so, no weighting
trend_removed_data_matrix_COMPLICATED_LM[is.na(trend_removed_data_matrix_COMPLICATED_LM)]=0
#data_matrix[is.na(data_matrix)]=0
W=cor(t(trend_removed_data_matrix_COMPLICATED_LM))
#W=cor(t(data_matrix))

W
#W[W>0.05]=1
W[W<0.05]=0
W=W-diag(rep(1,dim(trend_removed_data_matrix_COMPLICATED_LM)[1]))
W=normalize.rows(W,method="manhattan")
#View(W)
W

#data_matrix[data_matrix==0]=NA

#trend_removed_data_matrix_COMPLICATED_LM[is.na(trend_removed_data_matrix_COMPLICATED_LM)]=0
#W=cor(t(trend_removed_data_matrix_COMPLICATED_LM))

#W=matrix(c(0,3,2,1,3,0,2,1,1,3,0,2,2,1,3,0),4,4)

#W=normalize.rows(W,method="manhattan")
#W
####################################################
#Knn type W ...
#W=matrix(c(0,0.5,0.3,0.5,0,0.3,0.3,0.3,0),3,3)
#W=normalize.rows(W,method = "manhattan")
###################################################
#parella algorithm starts running...  
##################################################
trend_removed_data_matrix_COMPLICATED_LM[trend_removed_data_matrix_COMPLICATED_LM==0]=NA

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


delta_matrix=apply(trend_removed_data_matrix_COMPLICATED_LM,2,miss_vector_ones_and_zeros)
#mean centreing data matrix with columns of "x" as sets of measurements from given station
#scale(x, scale = FALSE)


sum(delta_matrix[2,])


#replacing misses with zero for data matrix
trend_removed_data_matrix_COMPLICATED_LM[is.na(trend_removed_data_matrix_COMPLICATED_LM)]=0 
trend_removed_data_matrix_COMPLICATED_LM[2,]


#fix up dimension...
y_bar=as.numeric(rowSums(trend_removed_data_matrix_COMPLICATED_LM))/rowSums(delta_matrix)

rowSums(trend_removed_data_matrix_COMPLICATED_LM)/rowSums(delta_matrix)

y_bar
trend_removed_data_matrix_COMPLICATED_LM[1,]
sweep(trend_removed_data_matrix_COMPLICATED_LM,1,y_bar)


y_tilde_matrix=delta_matrix*sweep(trend_removed_data_matrix_COMPLICATED_LM,1,y_bar)
y_tilde_matrix[,7777] 


#iteration 

num_iterations=300  #change this to be large enough for convergence (or use while loop eventually)

#need to instanciate blank yhat...
yhat_matrix=matrix(rep(0,dim(y_tilde_matrix)[1]*dim(y_tilde_matrix)[2]),dim(y_tilde_matrix)[1],dim(y_tilde_matrix)[2])
s=2
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
    
    lam=(t(solve(t(Xi_HAT)%*%Xi_HAT)%*%t(Xi_HAT)%*%Yi_HAT))
    
    lambda0[i]=lam[1]
    lambda1[i]=lam[2]
    lambda2[i]=lam[3]
  }
  print(norm((diag(lambda0)%*%W), type = "2")+norm((diag(lambda1)+diag(lambda2)%*%W), type = "2"))
  print(solve(diag(NUM_STATIONS)-diag(lambda0)%*%W))
  
  #b)
  #yhat_matrix two steps for t-1=0 case... I'm assuming other terms are just zero...
  yhat_matrix[,1]=y_tilde_matrix[,1]
  for(i in 2:dim(y_tilde_matrix)[2]){
    yhat_matrix[,i]=diag(lambda0)%*%W%*%y_tilde_matrix[,i]+diag(lambda1)%*%y_tilde_matrix[,i-1]+diag(lambda2)%*%W%*%y_tilde_matrix[,i-1]  
  }
  
  
  #ybar
  x=rep(0,dim(y_tilde_matrix)[1])
  for(i in 1:dim(y_tilde_matrix)[2]){
    x=x+delta_matrix[,i]*trend_removed_data_matrix_COMPLICATED_LM[,i]+(rep(1,length(trend_removed_data_matrix_COMPLICATED_LM[,1]))-delta_matrix[,i])*(yhat_matrix[,i]+y_bar)
  }
  y_bar=x/dim(y_tilde_matrix)[2]
  
  
  #ytilde_matrix
  for(i in 1:dim(y_tilde_matrix)[2]){
    y_tilde_matrix[,i]=delta_matrix[,i]*(trend_removed_data_matrix_COMPLICATED_LM[,i]-y_bar)+(rep(1,dim(y_tilde_matrix)[1])-delta_matrix[,i])*yhat_matrix[,i]
  }
  
  
}


#the result
result=y_tilde_matrix+y_bar
#want this to be false...
all(result==trend_removed_data_matrix_COMPLICATED_LM)
dim(result)
#compare with this
trend_removed_data_matrix_COMPLICATED_LM[1,]

trend_removed_data_matrix_COMPLICATED_LM_IMPUTED=result

par(mfrow=c(3,1))
#plotting  the imputed series...
for(i in 1:(NUM_STATIONS*1-num_removals)) {
  plot(trend_removed_data_matrix_COMPLICATED_LM_IMPUTED[i,],type='l',ylim=c(-150,10),main=i)
  plot(trend_removed_data_matrix_COMPLICATED_LM[i,],type='l',ylim=c(-150,10),main=i)
  plot(data_matrix[i,],type='l',ylim=c(-150,10),main=i)
  
  
}



############################################
#we use PACF to see if an AR model is appropriate for residuals... retrospectively... Just for fun
#############################################

pacf(trend_removed_data_matrix_COMPLICATED_LM_IMPUTED[1,],lag.max = 1000)
pacf(trend_removed_data_matrix_COMPLICATED_LM_IMPUTED[1,],lag.max = 10)


###########################################
#Undoing variance scaling with the flavour of the day  
################################################
##toggle this on and the loop below it off if using stratified variance approach
#NOTE we de-scale the imputed and non imputed version for plotting purposes later... 

#for(k in 1:NUM_STATIONS){
# trend_removed_data_matrix_COMPLICATED_LM_IMPUTED[k,]=trend_removed_data_matrix_COMPLICATED_LM_IMPUTED[k,]*Daily_std_error_est_list[[k]]
#trend_removed_data_matrix_COMPLICATED_LM[k,]=trend_removed_data_matrix_COMPLICATED_LM[k,]*Daily_std_error_est_list[[k]]
#}

#note



for(q in 1:NUM_STATIONS){
  
  trend_removed_data_matrix_COMPLICATED_LM[q,]=trend_removed_data_matrix_COMPLICATED_LM[q,]*Smoothed_std_error_curves[[q]]
  trend_removed_data_matrix_COMPLICATED_LM_IMPUTED[q,]=trend_removed_data_matrix_COMPLICATED_LM_IMPUTED[q,]*Smoothed_std_error_curves[[q]]
  
}


###########################################
#Undoing detrending with the flavour of the day (need to have this last)
################################################
retrended_data_matrix=matrix(rep(NA,(dim(trend_removed_data_matrix_COMPLICATED_LM_IMPUTED)[1]*(dim(trend_removed_data_matrix_COMPLICATED_LM_IMPUTED)[2]))),dim(trend_removed_data_matrix_COMPLICATED_LM_IMPUTED)[1],(dim(trend_removed_data_matrix_COMPLICATED_LM_IMPUTED)[2]))

dim(retrended_data_matrix)
dim(trend_removed_data_matrix_COMPLICATED_LM_IMPUTED)
for(i in 1:((NUM_STATIONS*1)-num_removals)){
  
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
  retrended_data_matrix[i,]=trend_removed_data_matrix_COMPLICATED_LM_IMPUTED[i,]+(model1$coefficients[1]+model1$coefficients[2]*t+model1$coefficients[3]*cosine_series+model1$coefficients[4]*sine_series+model1$coefficients[5]*cosine_series2+model1$coefficients[6]*sine_series2+model1$coefficients[7]*cosine_series3+model1$coefficients[8]*sine_series3+model1$coefficients[9]*cosine_series4+model1$coefficients[10]*sine_series4 )
  #####################################
  #toggle the below line on/off and the above off/on respectively to have only lm fit in reco vs with parella error
  #####################################
  #retrended_data_matrix[i,]=(model1$coefficients[1]+model1$coefficients[2]*t+model1$coefficients[3]*cosine_series+model1$coefficients[4]*sine_series+model1$coefficients[5]*cosine_series2+model1$coefficients[6]*sine_series2+model1$coefficients[7]*cosine_series3+model1$coefficients[8]*sine_series3+model1$coefficients[9]*cosine_series4+model1$coefficients[10]*sine_series4 )
  #####################################
  #####################################
  
  #model1=lm(data_matrix[i,]~t+cosine_series+sine_series+cosine_series2+sine_series2+cosine_series3+sine_series3+cosine_series4+sine_series4+cosine_series5+sine_series5+cosine_series6+sine_series6+cosine_series7+sine_series7+cosine_series8+sine_series8+cosine_series9+sine_series9+cosine_series10+sine_series10)
  
  #retrended_data_matrix[i,]=trend_removed_data_matrix_COMPLICATED_LM_IMPUTED[i,]+(model1$coefficients[1]+model1$coefficients[2]*t+model1$coefficients[3]*cosine_series+model1$coefficients[4]*sine_series+model1$coefficients[5]*cosine_series2+model1$coefficients[6]*sine_series2+model1$coefficients[7]*cosine_series3+model1$coefficients[8]*sine_series3+model1$coefficients[9]*cosine_series4+model1$coefficients[10]*sine_series4+model1$coefficients[11]*cosine_series5+model1$coefficients[12]*sine_series5+model1$coefficients[13]*cosine_series6+model1$coefficients[14]*sine_series6+model1$coefficients[15]*cosine_series7+model1$coefficients[16]*sine_series7+model1$coefficients[17]*cosine_series8+model1$coefficients[18]*sine_series8+model1$coefficients[19]*cosine_series9+model1$coefficients[20]*sine_series9+model1$coefficients[21]*cosine_series10+model1$coefficients[22]*sine_series10 )
  
  
  
  
}

#plot of last lm fit...
#plot( xlim=c(1,2000),(model1$coefficients[1]+model1$coefficients[2]*t+model1$coefficients[3]*cosine_series+model1$coefficients[4]*sine_series+model1$coefficients[5]*cosine_series2+model1$coefficients[6]*sine_series2+model1$coefficients[7]*cosine_series3+model1$coefficients[8]*sine_series3+model1$coefficients[9]*cosine_series4+model1$coefficients[10]*sine_series4+model1$coefficients[11]*cosine_series5+model1$coefficients[12]*sine_series5+model1$coefficients[13]*cosine_series6+model1$coefficients[14]*sine_series6))


################################################
#adding noise
################################################


residuals_std_estimate_vector=rep(NA,(NUM_STATIONS*15-num_removals))


COMPLICATED_LM_reconstructed_series_WITH_ADDED_NOISE=retrended_data_matrix


for(i in 1:(NUM_STATIONS*1-num_removals)){
  
  residuals_std_estimate_vector[i]=sd(trend_removed_data_matrix_COMPLICATED_LM[i,(!is.na(data_matrix[i,]))])
  #STL_reconstructed_series[i,]=result[i,]+stl(obs1,s.window = "periodic")$time.series[,"seasonal"]+stl(obs1,s.window = "periodic")$time.series[,"trend"]
  
  #STL_noiseless_reconstructed_series[i,]=result[i,]+stl(obs1,s.window = "periodic")$time.series[,"seasonal"]+stl(obs1,s.window = "periodic")$time.series[,"trend"]
  
  
  COMPLICATED_LM_reconstructed_series_WITH_ADDED_NOISE[i,(is.na(data_matrix[i,]))]=COMPLICATED_LM_reconstructed_series_WITH_ADDED_NOISE[i,(is.na(data_matrix[i,]))]+rnorm(sum(is.na(data_matrix[i,])),0,residuals_std_estimate_vector[i])
  
}
residuals_std_estimate_vector

#############################################################
#final plots
########################################################


par(mfrow=c(1,1))
for(i in 1:(NUM_STATIONS*1-num_removals)) { 
  plot(trend_removed_data_matrix_COMPLICATED_LM_IMPUTED[i,],type='l',main=i,xlim=c(900,1100),col=i,ylim=c(-10,10))
  par(new=TRUE) 
}

cor(trend_removed_data_matrix_COMPLICATED_LM_IMPUTED[1,],trend_removed_data_matrix_COMPLICATED_LM_IMPUTED[2,])
cor(trend_removed_data_matrix_COMPLICATED_LM_IMPUTED[1,],trend_removed_data_matrix_COMPLICATED_LM_IMPUTED[3,])
cor(trend_removed_data_matrix_COMPLICATED_LM_IMPUTED[1,],trend_removed_data_matrix_COMPLICATED_LM_IMPUTED[4,])
cor(trend_removed_data_matrix_COMPLICATED_LM_IMPUTED[2,],trend_removed_data_matrix_COMPLICATED_LM_IMPUTED[3,])
cor(trend_removed_data_matrix_COMPLICATED_LM_IMPUTED[2,],trend_removed_data_matrix_COMPLICATED_LM_IMPUTED[4,])
cor(trend_removed_data_matrix_COMPLICATED_LM_IMPUTED[4,],trend_removed_data_matrix_COMPLICATED_LM_IMPUTED[3,])


par(mfrow=c(3,1))
for(i in 1:(NUM_STATIONS*1-num_removals)) { 
  plot(retrended_data_matrix[i,],type='l',ylim=c(-40,40),main=i)
  plot(data_matrix[i,],ylim=c(-40,40),type='l')
  plot(COMPLICATED_LM_reconstructed_series_WITH_ADDED_NOISE[i,],type='l',ylim=c(-40,40),main=i)
  
}


par(mfrow=c(5,1))
for(i in 1:(NUM_STATIONS*1-num_removals)) { 
  plot(retrended_data_matrix[i,],type='l',ylim=c(-40,40),main=i)
  plot(trend_removed_data_matrix_COMPLICATED_LM_IMPUTED[i,],type='l',ylim=c(-40,40))
  plot(trend_removed_data_matrix_COMPLICATED_LM[i,],type='l',ylim=c(-40,40))
  plot(data_matrix[i,],ylim=c(-40,40))
  plot(COMPLICATED_LM_reconstructed_series_WITH_ADDED_NOISE[i,],type='l',ylim=c(-40,40),main=i)
  
}


par(mfrow=c(5,1))
for(i in 1:(NUM_STATIONS*1-num_removals)) { 
  plot(retrended_data_matrix[i,],type='l',ylim=c(-40,40),main=i)
  plot(trend_removed_data_matrix_COMPLICATED_LM_IMPUTED[i,],type='l',ylim=c(-40,40))
  plot(trend_removed_data_matrix_COMPLICATED_LM[i,],type='l',ylim=c(-40,40))
  plot(data_matrix[i,],ylim=c(-40,40))
  plot(COMPLICATED_LM_reconstructed_series_WITH_ADDED_NOISE[i,],type='l',ylim=c(-40,40),main=i)
  
}

#close ups
par(mfrow=c(2,1))
index1=1
for( i in 1:5){
  
  
  
  #plot(retrended_data_matrix[i,],type='l',ylim=c(-100,10),main=i)
  plot(data_matrix[index1,],ylim=c(-40,40),type='l',xlim=c(((i-1)*2000+1),(i)*2000))
  plot(trend_removed_data_matrix_COMPLICATED_LM_IMPUTED[index1,],type='l',ylim=c(-40,40),xlim=c(((i-1)*2000+1),(i)*2000),main=i)
  
  
}

#
par(mfrow=c(2,2))
for(i in 1:(NUM_STATIONS*1-num_removals)) { 
  
  plot(trend_removed_data_matrix_COMPLICATED_LM[i,],type='l',ylim=c(-10,40))
  plot(trend_removed_data_matrix_COMPLICATED_LM_IMPUTED[i,],type='l',ylim=c(-10,40 ),main=i)
  
}

################################################
##calculating MSE predictions... (of data vs without noise)

MSE_1_period_removed_vec_NO_NOISE=rep(NA,NUM_STATIONS)
for(i in 1:NUM_STATIONS){
  MSE_1_period_removed_vec_NO_NOISE[i]=mean((known_data_matrix[i,Test_SET_locations_matrix[i,]]-retrended_data_matrix[i,Test_SET_locations_matrix[i,]])^2,na.rm=TRUE)
}
MSE_1_period_removed_vec_NO_NOISE

plot(Test_SET_matrix_1_Period[1,])
plot(retrended_data_matrix[Test_SET_locations_matrix[i,]])

for(i in 1:NUM_STATIONS){
  plot(retrended_data_matrix[i,],ylim=c(0,40))
  plot(data_matrix[i,],ylim=c(0,40))
  
}
#MSE_1_period_removed_vec_WITH_NOISE=rep(NA,NUM_STATIONS)
#for(i in 1:NUM_STATIONS){
# MSE_1_period_removed_vec_WITH_NOISE[i]=mean((Test_SET_matrix_1_Period[i,]-COMPLICATED_LM_reconstructed_series_WITH_ADDED_NOISE[Test_SET_locations_matrix[i,]])^2,na.rm=TRUE)
#}
#MSE_1_period_removed_vec_WITH_NOISE



##################################################
trend_removed_data_matrix_COMPLICATED_LM[trend_removed_data_matrix_COMPLICATED_LM==0]=NA
for(q in 1:NUM_STATIONS){
  
  trend_removed_data_matrix_COMPLICATED_LM[q,]=trend_removed_data_matrix_COMPLICATED_LM[q,]/Smoothed_std_error_curves[[q]]
  
}
#our data is scaled residual series... 
#using AR(1) model with unconstrained parameter matrces
#fitting with MARSS - see page 246 onwards of MARS users guide... can alo fit MAR(p) model

dim(trend_removed_data_matrix_COMPLICATED_LM)

#install.packages("stats")
library(stats)
#install.packages("MARSS")
library(MARSS)
Z <- diag(1,4)
Z
B <- matrix(c("b11","b12","b13","b14","b21","b22","b23","b24","b31","b32","b33","b34","b41","b42","b43","b44"), 4, 4)
B
U <- matrix(0, 4, 1)
U
#note Q is made symmetric 
Q <- matrix(c("q11","q12","q13","q14","q12","q22","q23","q24","q13","q23","q33","q34","q14","q24","q34","q44"), 4, 4)
Q
A <- matrix(0, 4, 1)
A
R <- matrix(0, 4, 4)
R
#see page 252 on how to set this... 
#have to name it differently otherwise it screws up defn of PI as in circles... 
pinit <-as.matrix(rowMeans(trend_removed_data_matrix_COMPLICATED_LM,na.rm=TRUE))
dim(pi)

V <- matrix(0, 4, 4)
V

model.list.2m <- list(Z = Z, B = B, U = U, Q = Q, A = A,R = R, x0 = pinit, V0 = V, tinitx = 1)

mar2 <- MARSS(trend_removed_data_matrix_COMPLICATED_LM[, (2:dim(trend_removed_data_matrix_COMPLICATED_LM)[2])], model = model.list.2m)
#sves pdf of  model fitted to working directory :) 
toLatex(mar2$model)
#plot(mar2)
summary(mar2)
#imputed values -note: "fitted" values are different...

length(mar2$ytT[1,])
imputed_kalman_scaled_residuals=cbind(trend_removed_data_matrix_COMPLICATED_LM[,1],mar2$ytT)
#imputed_kalman_scaled_residuals=mar2$ytT

imputed_kalman_RE_scaled_residuals=imputed_kalman_scaled_residuals
for(i in 1:((NUM_STATIONS*1)-num_removals)){
  
  imputed_kalman_RE_scaled_residuals[i,]=Smoothed_std_error_curves[[i]]*imputed_kalman_scaled_residuals[i,]
}

for(q in 1:NUM_STATIONS){
  
  trend_removed_data_matrix_COMPLICATED_LM[q,]=trend_removed_data_matrix_COMPLICATED_LM[q,]*Smoothed_std_error_curves[[q]]
  
}









par(mfrow=c(2,1))

plot(imputed_kalman_RE_scaled_residuals[3,1:9000])
plot(trend_removed_data_matrix_COMPLICATED_LM_IMPUTED[3,1:9000])

known_residuals=matrix(rep(NA,(dim(trend_removed_data_matrix_COMPLICATED_LM_IMPUTED)[1]*(dim(trend_removed_data_matrix_COMPLICATED_LM_IMPUTED)[2]))),dim(trend_removed_data_matrix_COMPLICATED_LM_IMPUTED)[1],(dim(trend_removed_data_matrix_COMPLICATED_LM_IMPUTED)[2]))

for(i in 1:((NUM_STATIONS*1)-num_removals)){
  
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

par(mfrow=c(3,1))
for(i in 1:((NUM_STATIONS*1)-num_removals)){
  plot(imputed_kalman_RE_scaled_residuals[i,is.na(data_matrix[i,])],ylim=c(-20,20))
  plot(trend_removed_data_matrix_COMPLICATED_LM_IMPUTED[i,is.na(data_matrix[i,])],ylim=c(-20,20))
  plot(known_residuals[i,is.na(data_matrix[i,])],ylim=c(-20,20))
}

MSE_1_KALMAN=rep(NA,NUM_STATIONS)
for(i in 1:NUM_STATIONS){
  MSE_1_KALMAN[i]=mean((imputed_kalman_RE_scaled_residuals[i,Test_SET_locations_matrix[i,]]-known_residuals[i,Test_SET_locations_matrix[i,]])^2,na.rm=TRUE)
}
MSE_1_KALMAN


MSE_1_Parrella=rep(NA,NUM_STATIONS)
for(i in 1:NUM_STATIONS){
  MSE_1_Parrella[i]=mean((trend_removed_data_matrix_COMPLICATED_LM_IMPUTED[i,Test_SET_locations_matrix[i,]]-known_residuals[i,Test_SET_locations_matrix[i,]])^2,na.rm=TRUE)
}
MSE_1_Parrella
################################################
##calculating MSE predictions...kalman

#imputed_kalman_RE_scaled_residuals=imputed_kalman_scaled_residuals
#for(i in 1:((NUM_STATIONS*1)-num_removals)){

# imputed_kalman_RE_scaled_residuals[i,]=Smoothed_std_error_curves[[i]]*imputed_kalman_scaled_residuals[i,]
#}



retrended_data_matrix_KALMAN=matrix(rep(NA,(dim(trend_removed_data_matrix_COMPLICATED_LM_IMPUTED)[1]*(dim(trend_removed_data_matrix_COMPLICATED_LM_IMPUTED)[2]))),dim(trend_removed_data_matrix_COMPLICATED_LM_IMPUTED)[1],(dim(trend_removed_data_matrix_COMPLICATED_LM_IMPUTED)[2]))

for(i in 1:((NUM_STATIONS*1)-num_removals)){
  
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
  retrended_data_matrix_KALMAN[i,]=imputed_kalman_RE_scaled_residuals[i,]+(model1$coefficients[1]+model1$coefficients[2]*t+model1$coefficients[3]*cosine_series+model1$coefficients[4]*sine_series+model1$coefficients[5]*cosine_series2+model1$coefficients[6]*sine_series2+model1$coefficients[7]*cosine_series3+model1$coefficients[8]*sine_series3+model1$coefficients[9]*cosine_series4+model1$coefficients[10]*sine_series4 )
  
}

par(mfrow=c(3,1))
for(i in 1:NUM_STATIONS){
  plot(retrended_data_matrix_KALMAN[i,],ylim=c(0,40))
  plot(data_matrix[i,],ylim=c(0,40))
  plot(retrended_data_matrix[i,],ylim=c(0,40))
  
  
}



MSE_1_period_removed_vec_KALMAN=rep(NA,NUM_STATIONS)
for(i in 1:NUM_STATIONS){
  MSE_1_period_removed_vec_KALMAN[i]=mean((retrended_data_matrix_KALMAN[i,Test_SET_locations_matrix[i,]]-known_data_matrix[i,Test_SET_locations_matrix[i,]])^2,na.rm=TRUE)
}
MSE_1_period_removed_vec_KALMAN



(Test_SET_matrix_1_Period[i,]-retrended_data_matrix[Test_SET_locations_matrix[i,]])^2

(retrended_data_matrix_KALMAN[i,Test_SET_locations_matrix[i,]]-Test_SET_matrix_1_Period[i,])^2
retrended_data_matrix_KALMAN[i,Test_SET_locations_matrix[i,]]
Test_SET_matrix_1_Period[i,]

par(mfrow=c(2,1))
#first location fitted values
plot(imputed_kalman_RE_scaled_residuals[1,])
plot(trend_removed_data_matrix_COMPLICATED_LM_IMPUTED[1,])

plot(imputed_kalman_RE_scaled_residuals[1,],xlim=c(1,1000),type='l')
plot(trend_removed_data_matrix_COMPLICATED_LM_IMPUTED[1,],xlim=c(1,1000),type='l')

#2nd location fitted values
#first location fitted values
plot(imputed_kalman_RE_scaled_residuals[2,])
plot(trend_removed_data_matrix_COMPLICATED_LM_IMPUTED[2,])

plot(imputed_kalman_RE_scaled_residuals[2,],xlim=c(1,1000),type='l')
plot(trend_removed_data_matrix_COMPLICATED_LM_IMPUTED[2,],xlim=c(1,1000),type='l')

#3rd location fitted values
#first location fitted values
plot(imputed_kalman_RE_scaled_residuals[3,])
plot(trend_removed_data_matrix_COMPLICATED_LM_IMPUTED[3,])

plot(imputed_kalman_RE_scaled_residuals[3,],xlim=c(1,1000),type='l')
plot(trend_removed_data_matrix_COMPLICATED_LM_IMPUTED[3,],xlim=c(1,1000),type='l')

#4th location fitted values
#first location fitted values
plot(imputed_kalman_RE_scaled_residuals[4,])
plot(trend_removed_data_matrix_COMPLICATED_LM_IMPUTED[4,])

plot(imputed_kalman_RE_scaled_residuals[4,],xlim=c(1,1000),type='l')
plot(trend_removed_data_matrix_COMPLICATED_LM_IMPUTED[4,],xlim=c(1,1000),type='l')


MSE_1_period_removed_vec_LM_ONLY=rep(NA,NUM_STATIONS)
for(i in 1:NUM_STATIONS){
  
  
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


################################################
?print.marssMLE
##################################################
eigen(W)
lambda0
lambda1
lambda2


#install.packages("imputeTS")
library("imputeTS")
plot(na.interpolation(trend_removed_data_matrix_COMPLICATED_LM[2,],option = "linear"))
plot(trend_removed_data_matrix_COMPLICATED_LM[2,])


retrended_data_matrix_Linear_Interpolation=matrix(rep(NA,(dim(trend_removed_data_matrix_COMPLICATED_LM_IMPUTED)[1]*(dim(trend_removed_data_matrix_COMPLICATED_LM_IMPUTED)[2]))),dim(trend_removed_data_matrix_COMPLICATED_LM_IMPUTED)[1],(dim(trend_removed_data_matrix_COMPLICATED_LM_IMPUTED)[2]))

for(i in 1:((NUM_STATIONS*1)-num_removals)){
  
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
plot(retrended_data_matrix_Linear_Interpolation[2,])


MSE_1_period_removed_Linear_Interpolation=rep(NA,NUM_STATIONS)
for(i in 1:NUM_STATIONS){
  
  MSE_1_period_removed_Linear_Interpolation[i]=mean((retrended_data_matrix_Linear_Interpolation[i,Test_SET_locations_matrix[i,]]-known_data_matrix[i,Test_SET_locations_matrix[i,]])^2,na.rm=TRUE)
}
MSE_1_period_removed_Linear_Interpolation


##############################

FILTERED_retrended_data_matrix=matrix(rep(NA,(dim(trend_removed_data_matrix_COMPLICATED_LM_IMPUTED)[1]*(dim(trend_removed_data_matrix_COMPLICATED_LM_IMPUTED)[2]))),dim(trend_removed_data_matrix_COMPLICATED_LM_IMPUTED)[1],(dim(trend_removed_data_matrix_COMPLICATED_LM_IMPUTED)[2]))
filtered_residuals_without_known_ones_set=matrix(rep(NA,(dim(trend_removed_data_matrix_COMPLICATED_LM_IMPUTED)[1]*(dim(trend_removed_data_matrix_COMPLICATED_LM_IMPUTED)[2]))),dim(trend_removed_data_matrix_COMPLICATED_LM_IMPUTED)[1],(dim(trend_removed_data_matrix_COMPLICATED_LM_IMPUTED)[2]))
filtered_residuals_with_known_ones_set=matrix(rep(NA,(dim(trend_removed_data_matrix_COMPLICATED_LM_IMPUTED)[1]*(dim(trend_removed_data_matrix_COMPLICATED_LM_IMPUTED)[2]))),dim(trend_removed_data_matrix_COMPLICATED_LM_IMPUTED)[1],(dim(trend_removed_data_matrix_COMPLICATED_LM_IMPUTED)[2]))
for(i in 1:((NUM_STATIONS*1)-num_removals)){
  
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
  
  filtered_residuals_without_known_ones_set[i,]=ma(trend_removed_data_matrix_COMPLICATED_LM_IMPUTED[i,],2)
  
  filtered_residuals_with_known_ones_set[i,]=filtered_residuals_without_known_ones_set[i,]
  filtered_residuals_with_known_ones_set[i,!is.na(trend_removed_data_matrix_COMPLICATED_LM[i,])]=trend_removed_data_matrix_COMPLICATED_LM[i,!is.na(trend_removed_data_matrix_COMPLICATED_LM[i,])]
  
  FILTERED_retrended_data_matrix[i,]=filtered_residuals_with_known_ones_set[i,]+(model1$coefficients[1]+model1$coefficients[2]*t+model1$coefficients[3]*cosine_series+model1$coefficients[4]*sine_series+model1$coefficients[5]*cosine_series2+model1$coefficients[6]*sine_series2+model1$coefficients[7]*cosine_series3+model1$coefficients[8]*sine_series3+model1$coefficients[9]*cosine_series4+model1$coefficients[10]*sine_series4 )
  
}



MSE_1_period_removed_vec_FILTERED_NO_NOISE=rep(NA,((NUM_STATIONS*1)))
for(i in 1:((NUM_STATIONS))){
  MSE_1_period_removed_vec_FILTERED_NO_NOISE[i]=mean((known_data_matrix[i,Test_SET_locations_matrix[i,]]-FILTERED_retrended_data_matrix[i,Test_SET_locations_matrix[i,]])^2,na.rm=TRUE)
}
MSE_1_period_removed_vec_FILTERED_NO_NOISE
sum(MSE_1_period_removed_vec_FILTERED_NO_NOISE)













par(mfrow=c(3,1))
plot(data_matrix[3,])

plot(retrended_data_matrix[3,])

plot(retrended_data_matrix_KALMAN[3,])

####
#plot(data_matrix[1,1:100])

plot(retrended_data_matrix[3,1:100])

plot(retrended_data_matrix_KALMAN[3,1:100])
###
#plot(data_matrix[1,1:100+400])

plot(retrended_data_matrix[3,1:100+400])

plot(retrended_data_matrix_KALMAN[3,1:100+400])

MSE_L_Imputation_Matrix[,e]=MSE_1_period_removed_Linear_Interpolation
MSE_N_Imputation_Matrix[,e]=MSE_1_period_removed_vec_LM_ONLY
MSE_K_Imputation_Matrix[,e]=MSE_1_period_removed_vec_KALMAN
MSE_P_Imputation_Matrix[,e]=MSE_1_period_removed_vec_NO_NOISE
MSE_P_plus_filter_Imputation_Matrix[,e]=MSE_1_period_removed_vec_FILTERED_NO_NOISE


MSE_1_period_removed_Linear_Interpolation

MSE_1_period_removed_vec_KALMAN

MSE_1_period_removed_vec_LM_ONLY

MSE_1_period_removed_vec_NO_NOISE

MSE_1_period_removed_vec_FILTERED_NO_NOISE


}






#bad location with smoothing
plot(retrended_data_matrix[3,],ylim=c(10,40))
plot(FILTERED_retrended_data_matrix[3,],ylim=c(10,40))

#good location with smoothing
plot(retrended_data_matrix[1,],ylim=c(10,40))
plot(FILTERED_retrended_data_matrix[1,],ylim=c(10,40))


trend_removed_data_matrix_COMPLICATED_LM[is.na(trend_removed_data_matrix_COMPLICATED_LM)]=0
cor(t(trend_removed_data_matrix_COMPLICATED_LM))


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

model1=lm(data_matrix[1,]~t+cosine_series+sine_series+cosine_series2+sine_series2+cosine_series3+sine_series3+cosine_series4+sine_series4)

plot(model1, which =3 ,main="Marrawah: Standardised Residuals vs. Fitted Values ", head("")) 
plot(model1, which =2 ,main="Marrawah Q-Q Plot",head("")) 
model2=lm(data_matrix[3,]~t+cosine_series+sine_series+cosine_series2+sine_series2+cosine_series3+sine_series3+cosine_series4+sine_series4)

plot(model2, which =3 ,main="Smithon: Standardised Residuals vs. Fitted Values ", head("")) 
plot(model2, which =2 ,main="Smithton Q-Q Plot",head("")) 

#visual inspection for our detrendig and for stationarity
#Below we plot original (relatively dense dataset) vs. detrended data
par(new=FALSE)
par(mfrow=c(2,2))

trend_removed_data_matrix_COMPLICATED_LM[trend_removed_data_matrix_COMPLICATED_LM==0]=NA

  plot(data_matrix[1,],type="l",ylim=c(--10,35),main="Marrawah: Raw Temperature Series",xlab = "Days since January 1st 2000",ylab = "Temperature (degrees Celcius)")
  
  plot(trend_removed_data_matrix_COMPLICATED_LM[1,],type="l",ylim=c(-10,10),main="Marrawah: Detrended Residual Series",xlab = "Days since January 1st 2000",ylab = "Temperature (degrees Celcius)")
  
  
  plot(data_matrix[3,],type="l",ylim=c(--10,35),main="Smithton: Raw Temperature Series",xlab = "Days since January 1st 2000",ylab = "Temperature (degrees Celcius)")
  
  plot(trend_removed_data_matrix_COMPLICATED_LM[3,],type="l",ylim=c(-10,10),main="Smithton: Detrended Residual Series",xlab = "Days since January 1st 2000",ylab = "Temperature (degrees Celcius)")
  
  
#########################################################
  plot(trend_removed_data_matrix_COMPLICATED_LM[1,],type="l",ylim=c(-10,10),xlim=c(5000,8000),main="Marrawah: Detrended Residual Series",xlab = "Days since January 1st 2000",ylab = "Temperature (degrees Celcius)")
  plot(trend_removed_data_matrix_COMPLICATED_LM[3,],type="l",ylim=c(-10,10),xlim=c(6000,9000),main="Smithton: Detrended Residual Series",xlab = "Days since January 1st 2000",ylab = "Temperature (degrees Celcius)")
  
  
   for(q in 1:NUM_STATIONS){
    
    trend_removed_data_matrix_COMPLICATED_LM[q,]=trend_removed_data_matrix_COMPLICATED_LM[q,]/Smoothed_std_error_curves[[q]]
    
  }
   
  plot(trend_removed_data_matrix_COMPLICATED_LM[1,],type="l",ylim=c(-10,10),xlim=c(5000,8000),main="Marrawah: Detrended & Scaled Residual Series",xlab = "Days since January 1st 2000",ylab = "Temperature (degrees Celcius)")
   
  
   
  plot(trend_removed_data_matrix_COMPLICATED_LM[3,],type="l",ylim=c(-10,10),xlim=c(6000,9000),main="Smithton: Detrended & Scaled Residual Series",xlab = "Days since January 1st 2000",ylab = "Temperature (degrees Celcius)")
  for(q in 1:NUM_STATIONS){
    
    trend_removed_data_matrix_COMPLICATED_LM[q,]=trend_removed_data_matrix_COMPLICATED_LM[q,]*Smoothed_std_error_curves[[q]]
    
  }
###################################################################
#MSE stuff 
MSE_L_Imputation_Matrix 
MSE_N_Imputation_Matrix 
MSE_K_Imputation_Matrix 
MSE_P_Imputation_Matrix 
MSE_P_plus_filter_Imputation_Matrix

rowMeans(MSE_L_Imputation_Matrix)
var(t(MSE_L_Imputation_Matrix))^.5



rowMeans(MSE_N_Imputation_Matrix)
(var(t(MSE_N_Imputation_Matrix)))^.5

 
rowMeans(MSE_K_Imputation_Matrix)
var(t(MSE_K_Imputation_Matrix))^.5


rowMeans(MSE_P_Imputation_Matrix)
var(t(MSE_P_Imputation_Matrix))^.5


rowMeans(MSE_P_plus_filter_Imputation_Matrix)
var(t(MSE_P_plus_filter_Imputation_Matrix))^.5

par(mfrow=c( 2,1))

par(mar = c(5.1, 12, 4.1, 4))
#boxplot spatial location 1
boxplot(t(rbind( MSE_L_Imputation_Matrix[1,], MSE_N_Imputation_Matrix[1,] , MSE_K_Imputation_Matrix[1,] ,MSE_P_Imputation_Matrix[1,]  ,MSE_P_plus_filter_Imputation_Matrix[1,]  )),
      main = "Comparison of MSE for K/P/L/N Imputation - Tasmanian Data - Marrawah Station",
        at = 1:5,
        names = c("L-imputation", "N-Imputation", "K-imputation", "P-imputation", "P_plus_filtering-imputation"),
        las = 1,
        col = c(1,2,3,4,5),
        border = "brown",
        horizontal = TRUE,
        notch = FALSE,
        xlab="MSE"
)

boxplot(t(rbind( MSE_L_Imputation_Matrix[3,], MSE_N_Imputation_Matrix[3,] , MSE_K_Imputation_Matrix[3,] ,MSE_P_Imputation_Matrix[3,]  ,MSE_P_plus_filter_Imputation_Matrix[3,]  )),
        main = "Comparison of MSE for K/P/L/N Imputation - Tasmanian Data - Smithton Station",
        at = 1:5,
        names = c("L-imputation", "N-Imputation", "K-imputation", "P-imputation", "P_plus_filtering-imputation"),
        las = 1,
        col = c(1,2,3,4,5),
        border = "brown",
        horizontal = TRUE,
        notch = FALSE,
        xlab="MSE"
)

#Correlation matrix 

 
trend_removed_data_matrix_COMPLICATED_LM[is.na(trend_removed_data_matrix_COMPLICATED_LM)]=0
cor(t(trend_removed_data_matrix_COMPLICATED_LM))

par(mfrow=c(2,2))
acf(trend_removed_data_matrix_COMPLICATED_LM[1,], main="Marrawah Scaled Residual Temperature Series ACF")
acf(trend_removed_data_matrix_COMPLICATED_LM[2,], main="Cape Grim Scaled Residual Temperature Series ACF")
acf(trend_removed_data_matrix_COMPLICATED_LM[3,], main="Smithton Scaled Residual Temperature Series ACF")
acf(trend_removed_data_matrix_COMPLICATED_LM[4,], main="Luncheon Hill Scaled Residual Temperature Series ACF")


pacf(trend_removed_data_matrix_COMPLICATED_LM[1,] , main="Marrawah Scaled Residual Temperature Series PACF")
pacf(trend_removed_data_matrix_COMPLICATED_LM[2,], main="Cape Grim Scaled Residual Temperature Series PACF")
pacf(trend_removed_data_matrix_COMPLICATED_LM[3,], main="Smithton Scaled Residual Temperature Series PACF")
pacf(trend_removed_data_matrix_COMPLICATED_LM[4,], main="Cape Grim Scaled Residual Temperature Series PACF")


 

acf(trend_removed_data_matrix_COMPLICATED_LM[1,], main="Marrawah Scaled Residual Temperature Series ACF")
 acf(trend_removed_data_matrix_COMPLICATED_LM[3,], main="Smithton Scaled Residual Temperature Series ACF")
 

pacf(trend_removed_data_matrix_COMPLICATED_LM[1,] , main="Marrawah Scaled Residual Temperature Series PACF")
 pacf(trend_removed_data_matrix_COMPLICATED_LM[3,], main="Smithton Scaled Residual Temperature Series PACF")

 
 #reconstruted series
 par(mfrow=c(3,1))
 plot(data_matrix[1,],main="Marrawah: Raw Temperature Series",ylab = "Temperature (degrees Celcius)", xlab = "Days since Janurary 1st 2000")
 plot(retrended_data_matrix[1,],main="Marrawah: P-imputation Reconstructed Temperature Series",ylab = "Temperature (degrees Celcius)", xlab = "Days since Janurary 1st 2000")
 plot(retrended_data_matrix_KALMAN[1,],main="Marrawah: K-imputation Reconstructed Temperature Series",ylab = "Temperature (degrees Celcius)", xlab = "Days since Janurary 1st 2000")
 
 
 plot(data_matrix[3,],main="Smithton: Raw Temperature Series",ylab = "Temperature (degrees Celcius)", xlab = "Days since Janurary 1st 2000")
 plot(retrended_data_matrix[3,],main="Smithton: P-imputation Reconstructed Temperature Series",ylab = "Temperature (degrees Celcius)", xlab = "Days since Janurary 1st 2000")
 plot(retrended_data_matrix_KALMAN[3,],main="Smithton: K-imputation Reconstructed Temperature Series",ylab = "Temperature (degrees Celcius)", xlab = "Days since Janurary 1st 2000")
 
 
 par(mfrow=c(2,2))
 plot(data_matrix[1,],main="Marrawah: Raw Temperature Series",ylab = "Temperature (degrees Celcius)", xlab = "Days since Janurary 1st 2000")
 plot(data_matrix[2,],main="Cape Grim: Raw Temperature Series",ylab = "Temperature (degrees Celcius)", xlab = "Days since Janurary 1st 2000")
 plot(data_matrix[3,],main="Smithton: Raw Temperature Series",ylab = "Temperature (degrees Celcius)", xlab = "Days since Janurary 1st 2000")
 plot(data_matrix[4,],main="Luncheon Hill: Raw Temperature Series",ylab = "Temperature (degrees Celcius)", xlab = "Days since Janurary 1st 2000")
 
 
 
 #for(q in 1:NUM_STATIONS){
   
   #trend_removed_data_matrix_COMPLICATED_LM[q,]=trend_removed_data_matrix_COMPLICATED_LM[q,]/Smoothed_std_error_curves[[q]]
   #trend_removed_data_matrix_COMPLICATED_LM_IMPUTED[q,]=trend_removed_data_matrix_COMPLICATED_LM_IMPUTED[q,]/Smoothed_std_error_curves[[q]]
   
 #}
 
 
 trend_removed_data_matrix_COMPLICATED_LM[trend_removed_data_matrix_COMPLICATED_LM==0]=NA
 plot(trend_removed_data_matrix_COMPLICATED_LM[1,],main="Marrawah: Detrended & Scaled Temperature Series",ylab = "Temperature (degrees Celcius)", xlab = "Days since Janurary 1st 2000")
 plot(trend_removed_data_matrix_COMPLICATED_LM[2,],main="Cape Grim: Detrended & Scaled Series",ylab = "Temperature (degrees Celcius)", xlab = "Days since Janurary 1st 2000")
 plot(trend_removed_data_matrix_COMPLICATED_LM[3,],main="Smithton: Detrended & Scaled Series",ylab = "Temperature (degrees Celcius)", xlab = "Days since Janurary 1st 2000")
 plot(trend_removed_data_matrix_COMPLICATED_LM[4,],main="Luncheon Hill:Detrended & Scaled Series",ylab = "Temperature (degrees Celcius)", xlab = "Days since Janurary 1st 2000")
 
 
 plot(retrended_data_matrix[1,],main="Marrawah: P-imputed Temperature Series",ylab = "Temperature (degrees Celcius)", xlab = "Days since Janurary 1st 2000")
 plot(retrended_data_matrix[2,],main="Cape Grim: P-imputed Temperature Series",ylab = "Temperature (degrees Celcius)", xlab = "Days since Janurary 1st 2000")
 plot(retrended_data_matrix[3,],main="Smithton: P-imputed Temperature Series",ylab = "Temperature (degrees Celcius)", xlab = "Days since Janurary 1st 2000")
 plot(retrended_data_matrix[4,],main="Luncheon Hill: P-imputed Temperature Series",ylab = "Temperature (degrees Celcius)", xlab = "Days since Janurary 1st 2000")
 
 
 
 plot(retrended_data_matrix_KALMAN[1,],main="Marrawah: K-imputed Temperature Series",ylab = "Temperature (degrees Celcius)", xlab = "Days since Janurary 1st 2000")
 plot(retrended_data_matrix_KALMAN[2,],main="Cape Grim: K-imputed Temperature Series",ylab = "Temperature (degrees Celcius)", xlab = "Days since Janurary 1st 2000")
 plot(retrended_data_matrix_KALMAN[3,],main="Smithton: K-imputed Temperature Series",ylab = "Temperature (degrees Celcius)", xlab = "Days since Janurary 1st 2000")
 plot(retrended_data_matrix_KALMAN[4,],main="Luncheon Hill: K-imputed Temperature Series",ylab = "Temperature (degrees Celcius)", xlab = "Days since Janurary 1st 2000")
 
 
 plot(retrended_data_matrix_Linear_Interpolation[1,],main="Marrawah: L-imputed Temperature Series",ylab = "Temperature (degrees Celcius)", xlab = "Days since Janurary 1st 2000")
 plot(retrended_data_matrix_Linear_Interpolation[2,],main="Cape Grim: L-imputed Temperature Series",ylab = "Temperature (degrees Celcius)", xlab = "Days since Janurary 1st 2000")
 plot(retrended_data_matrix_Linear_Interpolation[3,],main="Smithton: L-imputed Temperature Series",ylab = "Temperature (degrees Celcius)", xlab = "Days since Janurary 1st 2000")
 plot(retrended_data_matrix_Linear_Interpolation[4,],main="Luncheon Hill: L-imputed Temperature Series",ylab = "Temperature (degrees Celcius)", xlab = "Days since Janurary 1st 2000")
 
 