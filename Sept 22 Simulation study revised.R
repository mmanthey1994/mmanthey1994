#Not simulating data right!!? correlation is so low!



library("Hmisc")
library("imputeTS")

library("urca")

#install.packages("tseries")
library(cleanIGRA)
library(magrittr)
library(dplyr)
library("wordspace")
library(tseries)
#install.packages("forecast")
library("forecast")

num_reps=1
convergence_num=rep(NA,num_reps)
MSE_matrix=matrix(NA,3,num_reps)

N_imputation_MSE_matrix=matrix(NA,1,num_reps)
dim(MSE_matrix)
for(b in 1:num_reps){
  
  
  #install.packages("Hmisc")
  
  
  percent_missingness_vector=1/10*c(1,1,1) 
  num_iterations=100
  
  y=matrix(rep(NA,3*10000),3,10000)
  y[,1]=c(0,0,0)
  
  W=matrix(c(0,1/2,1/2,1/2,0,1/2,1/2,1/2,0),3,3,byrow = TRUE)
  W
  
  
  #We use different spatial weight matrces to show robustness to mispessification
  W_model=matrix(c(4,1,2/3,4,0,3,5,5,4),3,3,byrow = TRUE)
  
  W_model=W_model%*%t(W_model)
  W_model[1,1]=0
  W_model[2,2]=0
  W_model[3,3]=0
  W_model=normalize.rows(W_model,method = "manhattan")
  
  rankMatrix(W)
  rankMatrix(W_model)
  
  #TOGGLE THIS ON IF YOU WANT TO CHECK HOW MISPECIFICATION OF SPATIAL WEIGHT AFFECTS THINGS... 
  W_model=W
 
  
   normal_error_sd=5
  
  lam0=0.1*diag(  runif(3,-6,6))
  lam1=0.1*diag(  runif(3,-6,6)  )
  lam2=0.1*diag(  runif(3,-6,6)  )
  
  
  
  
   
  #conditions for parrella to work
  norm((lam0%*%W_model), type = "2")+norm((diag(lam1)+lam2%*%W_model), type = "2")
  solve(diag(3)-lam0%*%W_model)
  
  #this matrix can be set to something other than the identity to induce correlation in error terms... 
  COV_Noise_Multiplier=diag(rep(1,3))
  
  for(i in 2:10000){
    
    y[,i]= solve(diag(rep(1,3))-lam0%*%W_model)%*%lam1%*%y[,i-1]+solve(diag(rep(1,3))-lam0%*%W_model)%*%lam2%*%W_model%*%y[,i-1]+solve(diag(rep(1,3))-lam0%*%W_model)%*%COV_Noise_Multiplier%*%rnorm(3,0,normal_error_sd)
    
  }
  
   
  
  data_matrix=t(y)
   
  
  known_data_matrix=data_matrix
  ?sample
  par(mfrow=c(3,1))
  #plot(data_matrix[,1],type='l',xlim=c(0,100))
  #plot(data_matrix[,2],type='l',xlim=c(0,100))
  #plot(data_matrix[,3],type='l',xlim=c(0,100))
  
  
  #we randomly sample all matrices and give one a very large missingness period
  data_matrix[sample(1:10000,10000*percent_missingness_vector[1]),1]=NA
  data_matrix[1:1000,1]=NA
  data_matrix[sample(1:10000,10000*percent_missingness_vector[2]),2]=NA
  data_matrix[2000:3000,2]=NA
  data_matrix[sample(1:10000,10000*percent_missingness_vector[3]),3]=NA
  data_matrix[5000:6000,3]=NA
  
  
  
  data_matrix=t(data_matrix)
  
  
  ############################################
 # cor((known_data_matrix))
  
  #data_matrix[is.na(data_matrix)]=0
   
  
  
 # W=cor(t(data_matrix))
  #W
  
#W=W-diag(rep(1,3))
  #W=normalize.rows(W,method="manhattan")
 # W
 ## plot(data_matrix[1,3000:3100],type="l")
 # par(new=TRUE)
  #plot(data_matrix[2,3000:3100],type="l")
  #################################################
  
  data_matrix[data_matrix==0]=NA
  
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
  
  
  delta_matrix=apply(data_matrix,2,miss_vector_ones_and_zeros)
  #mean centreing data matrix with columns of "x" as sets of measurements from given station
  #scale(x, scale = FALSE)
  
  
  sum(delta_matrix[2,])
  
  
  #replacing misses with zero for data matrix
  data_matrix[is.na(data_matrix)]=0 
  data_matrix[1,]
  
  
  #fix up dimension...
  y_bar=rowSums(data_matrix)/rowSums(delta_matrix)
  
  y_bar
  data_matrix[1,]
  sweep(data_matrix,1,y_bar)
  
  
  y_tilde_matrix=delta_matrix*sweep(data_matrix,1,y_bar)
  y_tilde_matrix[,1] 
  
  
  #iteration 
  
  #change this to be large enough for convergence (or use while loop eventually)
  
  #need to instanciate blank yhat...
  yhat_matrix=matrix(rep(0,dim(y_tilde_matrix)[1]*dim(y_tilde_matrix)[2]),dim(y_tilde_matrix)[1],dim(y_tilde_matrix)[2])
  for(s in 1:num_iterations){
    print("at iteration")
    print(s)
    
    #a)
    F=matrix(rep(0,dim(y_tilde_matrix)[1]^2),dim(y_tilde_matrix)[1],dim(y_tilde_matrix)[1])
    for(i in 1:(dim(y_tilde_matrix)[2]-1)){F=F+y_tilde_matrix[,i+1]%*%t(y_tilde_matrix[,i])}
    G=matrix(rep(0,dim(y_tilde_matrix)[1]^2),dim(y_tilde_matrix)[1],dim(y_tilde_matrix)[1])
    for(i in 1:(dim(y_tilde_matrix)[2])){G=G+y_tilde_matrix[,i]%*%t(y_tilde_matrix[,i])}
    
    Sigma1HAT=1/dim(y_tilde_matrix)[2]*F
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
      
      lam=(t(solvet(t(Xi_HAT)%*%Xi_HAT,tol = 1e-40)%*%t(Xi_HAT)%*%Yi_HAT))
      
      lambda0[i]=lam[1]
      lambda1[i]=lam[2]
      lambda2[i]=lam[3]
    }
    
    convergence_num[b]=norm((diag(lambda0)%*%W), type = "2")+norm((diag(lambda1)+diag(lambda2)%*%W), type = "2")
    print(norm((diag(lambda0)%*%W), type = "2")+norm((diag(lambda1)+diag(lambda2)%*%W), type = "2"))
    print(solve(diag(3)-diag(lambda0)%*%W_model))
    
    #b)
    #yhat_matrix two steps for t-1=0 case...  
    yhat_matrix[,1]=y_tilde_matrix[,1]
    for(i in 2:dim(y_tilde_matrix)[2]){
      yhat_matrix[,i]=diag(lambda0)%*%W%*%y_tilde_matrix[,i]+diag(lambda1)%*%y_tilde_matrix[,i-1]+diag(lambda2)%*%W%*%y_tilde_matrix[,i-1]  
    }
    
    
    #ybar
    x=rep(0,dim(y_tilde_matrix)[1])
    for(i in 1:dim(y_tilde_matrix)[2]){
      x=x+delta_matrix[,i]*data_matrix[,i]+(rep(1,length(data_matrix[,1]))-delta_matrix[,i])*(yhat_matrix[,i]+y_bar)
    }
    y_bar=x/dim(y_tilde_matrix)[2]
    
    
    #ytilde_matrix
    for(i in 1:dim(y_tilde_matrix)[2]){
      y_tilde_matrix[,i]=delta_matrix[,i]*(data_matrix[,i]-y_bar)+(rep(1,dim(y_tilde_matrix)[1])-delta_matrix[,i])*yhat_matrix[,i]
    }
    
    
  }
  
  
  #the result
  result=y_tilde_matrix+y_bar
  #want this to be false...
  all(result==data_matrix)
  dim(result)
  #compare with this
  data_matrix[1,]
  
  data_matrix_IMPUTED=result
  
  par(mfrow=c(2,3))
  #plotting  the imputed series...
  #for(i in 1:3 ){
  #plot(data_matrix_IMPUTED[i,],type='l',main=i)
  #plot(data_matrix[i,],type='l',main=i)
  
  #}
 # for(i in 1:3 ){
  #  plot(data_matrix_IMPUTED[i,],type='l',main=i,xlim=c(0,100),col="red")
    ##plot(data_matrix[i,],type='l',main=i,xlim=c(0,300))
   # par(new=TRUE)
    #plot(known_data_matrix[,i],type='l',main=i,xlim=c(0,100))
    
  #}
  
  #for(i in 1:3 ){
   # plot(data_matrix_IMPUTED[i,],type='l',main=i,col="red",xlim=c(5000,5100))
    
    #par(new=TRUE)
    #plot(known_data_matrix[,i],type='l',main=i,xlim=c(5000,5100))
    
#  }
  
  
  par(mfrow=c(1,3))
  
  
  
  #for(i in 1:3 ){
   # plot(data_matrix_IMPUTED[i,],type='l',main=i,xlim=c(0,100),col="red")
    ##plot(data_matrix[i,],type='l',main=i,xlim=c(0,300))
    #par(new=TRUE)
  #  plot(known_data_matrix[,i],type='l',main=i,xlim=c(0,100))
    
  #}
  
  #for(i in 1:3 ){
   # plot(data_matrix_IMPUTED[i,],type='l',main=i,xlim=c(0,2000),col="red")
    ##plot(data_matrix[i,],type='l',main=i,xlim=c(0,300))
    #par(new=TRUE)
    #plot(known_data_matrix[,i],type='l',main=i,xlim=c(0,2000))
    
  #}
  
  ########################################################################################
  ########################################################################################
  ########################################################################################
  data_matrix[data_matrix==0]=NA
  
  #install.packages("stats")
  library(stats)
  #install.packages("MARSS")
  library(MARSS)
  Z <- diag(1,3)
  Z
  B <- matrix(c("b11","b12","b13","b21","b22","b23","b31","b32","b33"), 3, 3)
  B
  U <- matrix(0, 3, 1)
  U
  #note Q is made symmetric 
  Q <- matrix(c("q11","q12","q13","q12","q22","q23","q13","q23","q33"), 3, 3)
  Q
  A <- matrix(0, 3, 1)
  A
  R <- matrix(0, 3, 3)
  R
  #see page 252 on how to set this... 
  #have to name it differently otherwise it screws up defn of PI as in circles... 
  pinit <-as.matrix(rowMeans(data_matrix,na.rm=TRUE))
  
  
  dim(pi)
  
  V <- matrix(0, 3, 3)
  V
  
  model.list.2m <- list(Z = Z, B = B, U = U, Q = Q, A = A,R = R, x0 = pinit, V0 = V, tinitx = 1)
  
  mar2 <- MARSS(data_matrix[, (2:dim(data_matrix)[2])], model = model.list.2m)
  
  
  
  ##sves pdf of  model fitted to working directory :) 
  toLatex(mar2$model)
  ##plot(mar2)
  summary(mar2)
  
  #imputed values -note: "fitted" values are different...
  par(mfrow=c(1,3))
  dim(mar2$ytT)
  
  imputed_kalman=cbind(data_matrix[,1],mar2$ytT)

  
  
  #####################################
  #####################################
  #MSE's for first spatial location
  #####################################
  #####################################
  #kalman
  MSE_matrix[1,b]= mean((t(known_data_matrix)[1,is.na(data_matrix[1,])]-imputed_kalman[1,is.na(data_matrix[1,])])^2,na.rm=T)
  
  #parrella
  MSE_matrix[2,b]= mean((t(known_data_matrix)[1,is.na(data_matrix[1,])]-data_matrix_IMPUTED[1,is.na(data_matrix[1,])])^2,na.rm=T)
  
  #linear interpolation
  MSE_matrix[3,b]= mean((t(known_data_matrix)[1,is.na(data_matrix[1,])]-na.interpolation(data_matrix[1, ],option = "linear")[is.na(data_matrix[1,])])^2,na.rm=T)
  
  
  N_imputation_MSE_matrix[1,b]=mean((t(known_data_matrix)[1,is.na(data_matrix[1,])]^2),na.rm=T)
  #####################################
  #####################################
  
  
}


 
par(mfrow=c(1,1))


plot(data_matrix_IMPUTED[1,],type='l',xlim=c(985,1015),col="red",ylim=c(-7,7),xlab = "Timestep", ylab = "Value/Estiated Value")
#plot(data_matrix[i,],type='l',main=i,xlim=c(1800,2000),ylim=c(-7,7))
par(new=TRUE)
plot(known_data_matrix[,1],type='l',xlim=c(985,1015),ylim=c(-7,7),xlab = "Timestep", ylab = "Value/Estiated Value")
par(new=TRUE)

plot(imputed_kalman[1,],type='l',xlim=c(985,1015),col="green",ylim=c(-7,7),xlab = "Timestep", ylab = "Value/Estiated Value")
par(new=TRUE)

plot(na.interpolation(data_matrix[1, ],option = "linear"),type='l',main="Comparison of Imputation Methods P/K/L/N on Simulated Data",xlim=c(985,1015),col="blue",ylim=c(-7,7),xlab = "Timestep", ylab = "Value/Estiated Value")
abline(v=1000, col="purple",lty=2)
legend(1001,-3.2, legend=c("Ground truth data values", "P-imputation estimates", "K-imputation estimates", "Linear interpolation estimates", "Fully-to-partially observed transition"),
       col=c("black", "red","green","blue","purple"), lty=c(1,1,1,1,2), cex=0.8)




#spatial location 1 only... simulated data... symmmetric in terms of average conclusions.. 
MSE_matrix
N_imputation_MSE_matrix
 


#may need to ignore some entries where divergence occurs...
par(mar = c(5.1, 7, 4.1, 4))
boxplot(t(rbind((MSE_matrix[,-c(5,6,8,9)]),N_imputation_MSE_matrix[,-c(5,6,8,9)])),
        main = "Comparison of MSE for K/P/L/N Imputation - Simulated Data",
        at = c(1,2,3,4),
        names = c("K-imputation", "P-Imputation", "L-imputation", "N-imputation"),
        las = 1,
        col = c(1,2,3,4),
        border = "brown",
        horizontal = TRUE,
        notch = FALSE,
        xlab="MSE"
)

rowMeans(MSE_matrix[,-c(5,6,8,9)])
mean(N_imputation_MSE_matrix[,-c(5,6,8,9)])
 

cov(t(MSE_matrix[,-c(5,6,8,9)]))
var(N_imputation_MSE_matrix[,-c(5,6,8,9)])
 



boxplot(t(rbind((MSE_matrix[,]),N_imputation_MSE_matrix[,])),
        main = "Comparison of MSE for K/P/L/N Imputation - Simulated Data",
        at = c(1,2,3,4),
        names = c("K-imputation", "P-Imputation", "L-imputation", "N-imputation"),
        las = 1,
        col = c(1,2,3,4),
        border = "brown",
        horizontal = TRUE,
        notch = FALSE,
        xlab="MSE"
)
convergence_num
cor((known_data_matrix))
W_model
