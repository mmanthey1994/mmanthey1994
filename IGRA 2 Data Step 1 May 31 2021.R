#IGRA 2 Data processing (step 1)
#the .txt format is really annoying and I can't use Jackson's stuff anymore... 
#also get weird errors when automatically trying to read data with read.csv... have to do it manually 

##########################################################
 
setwd("C:/Users/starc/OneDrive/Desktop/Masters 2020/MAY 26 Contemporary Files/IGRA Version 2 Raw data NEW may 31 2021")

#################################################
#me loading and cleaning data from all stations...

ALLfilenames = list.files(path = "C:/Users/starc/OneDrive/Desktop/Masters 2020/MAY 26 Contemporary Files/IGRA Version 2 Raw data NEW may 31 2021",pattern="89*")
ALLfilenames

ALLfiles=list()
 #These files should be selected from the above working directory... could not get this going automatically for some reason...
  ALLfiles[[1]] <- read.csv(file.choose(),sep = "",header = FALSE)
  ALLfiles[[2]] <- read.csv(file.choose(),sep = "",header = FALSE)
  ALLfiles[[3]] <- read.csv(file.choose(),sep = "",header = FALSE)
  ALLfiles[[4]] <- read.csv(file.choose(),sep = "",header = FALSE)
  ALLfiles[[5]] <- read.csv(file.choose(),sep = "",header = FALSE)
  ALLfiles[[6]] <- read.csv(file.choose(),sep = "",header = FALSE)
  ALLfiles[[7]] <- read.csv(file.choose(),sep = "",header = FALSE)
  ALLfiles[[8]] <- read.csv(file.choose(),sep = "",header = FALSE)
  ALLfiles[[9]] <- read.csv(file.choose(),sep = "",header = FALSE)
  ALLfiles[[10]] <- read.csv(file.choose(),sep = "",header = FALSE)
  ALLfiles[[11]] <- read.csv(file.choose(),sep = "",header = FALSE)
  ALLfiles[[12]] <- read.csv(file.choose(),sep = "",header = FALSE)
  ALLfiles[[13]] <- read.csv(file.choose(),sep = "",header = FALSE)
  ALLfiles[[14]] <- read.csv(file.choose(),sep = "",header = FALSE)
   head(ALLfiles[[14]])
   tail(ALLfiles[[14]])
   
   (ALLfiles[[14]])[2094500 :2094663  ,]
   (ALLfiles[[1]])
   
  as.data.frame.numeric_version(ALLfiles[[1]][])
  ##############################################################
  pressure_vector=c(1000,2000,3000,5000,7000,10000,15000,20000,25000,30000,40000,50000,70000,85000,92500)
  #####################################################
  
  #finding min time
  ALLfiles[[1]][1,]
  ALLfiles[[2]][1,]
  ALLfiles[[3]][1,]
  ALLfiles[[4]][1,] 
  ALLfiles[[5]][1,]  
  ALLfiles[[6]][1,]  
  ALLfiles[[7]][1,]  
  ALLfiles[[8]][1,]  
  ALLfiles[[9]][1,]  
  ALLfiles[[10]][1,]  #this has min time
  ALLfiles[[11]][1,]  
  ALLfiles[[12]][1,]  
  ALLfiles[[13]][1,]  
  ALLfiles[[14]][1,]
  
  #first date = 2/4/1954
  as.Date("04/02/1954", "%m/%d/%Y")
  
  #finding max time
  ALLfiles[[1]][(dim(ALLfiles[[1]])[1]-100):dim(ALLfiles[[1]])[1],] #1992
  ALLfiles[[2]][(dim(ALLfiles[[2]])[1]-100):dim(ALLfiles[[2]])[1],]
  ALLfiles[[3]][(dim(ALLfiles[[3]])[1]-100):dim(ALLfiles[[3]])[1],]
  ALLfiles[[4]][(dim(ALLfiles[[4]])[1]-100):dim(ALLfiles[[4]])[1],]  
  ALLfiles[[5]][(dim(ALLfiles[[5]])[1]-100):dim(ALLfiles[[5]])[1],] 
  ALLfiles[[6]][(dim(ALLfiles[[6]])[1]-100):dim(ALLfiles[[6]])[1],]  
  ALLfiles[[7]][(dim(ALLfiles[[7]])[1]-100):dim(ALLfiles[[7]])[1],]  
  ALLfiles[[8]][(dim(ALLfiles[[8]])[1]-100):dim(ALLfiles[[8]])[1],]  
  ALLfiles[[9]][(dim(ALLfiles[[9]])[1]-100):dim(ALLfiles[[9]])[1],]  
  ALLfiles[[10]][(dim(ALLfiles[[10]])[1]-100):dim(ALLfiles[[10]])[1],]  
  ALLfiles[[11]][(dim(ALLfiles[[11]])[1]-100):dim(ALLfiles[[11]])[1],]  
  ALLfiles[[12]][(dim(ALLfiles[[12]])[1]-100):dim(ALLfiles[[12]])[1],]  
  ALLfiles[[13]][(dim(ALLfiles[[13]])[1]-100):dim(ALLfiles[[13]])[1],]  
  
  ALLfiles[[14]][(dim(ALLfiles[[14]])[1]-100):dim(ALLfiles[[14]])[1],]  
    
  #last date = 29/5/2021
   
 as.Date("5/29/2021", "%m/%d/%Y")
 num_days_between_first_and_last=as.numeric(  as.Date("5/29/2021", "%m/%d/%Y"))-as.numeric(as.Date("5/29/1954", "%m/%d/%Y"))
 num_days_between_first_and_last
 as.numeric(as.Date("5/29/1954", "%m/%d/%Y"))
 #######################################################################################################
  
 Allfiles_1_list_hour_zero=list()
 Allfiles_1_list_hour_one=list()
 ALLfiles[[1]]
 dim(ALLfiles[[1]])[1]
 
 counter=0
 num_entries_for_zero=0
 num_entries_for_one=0
 
 for(k in 1:dim(ALLfiles[[1]])[1]){
   if(ALLfiles[[1]][k,1]=="#AYM00089001"&ALLfiles[[1]][k,5]=="00"){
     num_entries_for_zero=num_entries_for_zero+1
     
     for(q in k:(dim(ALLfiles[[1]])[1]-1)){
       if(ALLfiles[[1]][q,1]!="#AYM00089001"){counter=counter+1}
       if(ALLfiles[[1]][q+1,1]=="#AYM00089001"){break}
     }
     Allfiles_1_list_hour_zero[[num_entries_for_zero]]=ALLfiles[[1]][k:(k+counter),]
     counter=0
   }
   
   if(ALLfiles[[1]][k,1]=="#AYM00089001"&ALLfiles[[1]][k,5]=="12"){
     num_entries_for_one=num_entries_for_one+1
     
     for(q in k:(dim(ALLfiles[[1]])[1]-1)){
       if(ALLfiles[[1]][q,1]!="#AYM00089001"){counter=counter+1}
       if(ALLfiles[[1]][q+1,1]=="#AYM00089001"){break}
     }
     Allfiles_1_list_hour_one[[num_entries_for_one]]=ALLfiles[[1]][k:(k+counter),]
     counter=0
   }
   
    
 }   
 
 Allfiles_1_list_hour_zero[[2]]
 Allfiles_1_list_hour_one[[262]]
 ALLfiles[[1]][13200:13348,] 
 
 #######################################################################################################
 #######################################################################################################
 
 Allfiles_2_list_hour_zero=list()
 Allfiles_2_list_hour_one=list()
 ALLfiles[[2]]
 dim(ALLfiles[[2]])[1]
 
 counter=0
 num_entries_for_zero=0
 num_entries_for_one=0
 
 for(k in 1:dim(ALLfiles[[2]])[1]){
   if(ALLfiles[[2]][k,1]=="#AYM00089002"&ALLfiles[[2]][k,5]=="00"){
     num_entries_for_zero=num_entries_for_zero+1
     
     for(q in k:(dim(ALLfiles[[2]])[1]-1)){
       if(ALLfiles[[2]][q,1]!="#AYM00089002"){counter=counter+1}
       if(ALLfiles[[2]][q+1,1]=="#AYM00089002"){break}
     }
     Allfiles_2_list_hour_zero[[num_entries_for_zero]]=ALLfiles[[2]][k:(k+counter),]
     counter=0
   }
   
   if(ALLfiles[[2]][k,1]=="#AYM00089002"&ALLfiles[[2]][k,5]=="12"){
     num_entries_for_one=num_entries_for_one+1
     
     for(q in k:(dim(ALLfiles[[2]])[1]-1)){
       if(ALLfiles[[2]][q,1]!="#AYM00089002"){counter=counter+1}
       if(ALLfiles[[2]][q+1,1]=="#AYM00089002"){break}
     }
     Allfiles_2_list_hour_one[[num_entries_for_one]]=ALLfiles[[2]][k:(k+counter),]
     counter=0
   }
   
   
 }   
 
 Allfiles_2_list_hour_zero[[2]]
 Allfiles_2_list_hour_one[[2]]
 
 
 
 #######################################################################################################
 #######################################################################################################
 
 Allfiles_3_list_hour_zero=list()
 Allfiles_3_list_hour_one=list()
 ALLfiles[[3]]
 dim(ALLfiles[[3]])[1]
 
 counter=0
 num_entries_for_zero=0
 num_entries_for_one=0
 
 for(k in 1:dim(ALLfiles[[3]])[1]){
   if(ALLfiles[[3]][k,1]=="#AYM00089009"&ALLfiles[[3]][k,5]=="00"){
     num_entries_for_zero=num_entries_for_zero+1
     
     for(q in k:(dim(ALLfiles[[3]])[1]-1)){
       if(ALLfiles[[3]][q,1]!="#AYM00089009"){counter=counter+1}
       if(ALLfiles[[3]][q+1,1]=="#AYM00089009"){break}
     }
     Allfiles_3_list_hour_zero[[num_entries_for_zero]]=ALLfiles[[3]][k:(k+counter),]
     counter=0
   }
   
   if(ALLfiles[[3]][k,1]=="#AYM00089009"&ALLfiles[[3]][k,5]=="12"){
     num_entries_for_one=num_entries_for_one+1
     
     for(q in k:(dim(ALLfiles[[3]])[1]-1)){
       if(ALLfiles[[3]][q,1]!="#AYM00089009"){counter=counter+1}
       if(ALLfiles[[3]][q+1,1]=="#AYM00089009"){break}
     }
     Allfiles_3_list_hour_one[[num_entries_for_one]]=ALLfiles[[3]][k:(k+counter),]
     counter=0
   }
   
   
 }   
 
 Allfiles_3_list_hour_zero[[2]]
 Allfiles_3_list_hour_one[[2]]
 
 
 
 #######################################################################################################
 
 #######################################################################################################
 
 Allfiles_4_list_hour_zero=list()
 Allfiles_4_list_hour_one=list()
 ALLfiles[[4]]
 dim(ALLfiles[[4]])[1]
 
 counter=0
 num_entries_for_zero=0
 num_entries_for_one=0
 
 for(k in 1:dim(ALLfiles[[4]])[1]){
   if(ALLfiles[[4]][k,1]=="#AYM00089022"&ALLfiles[[4]][k,5]=="00"){
     num_entries_for_zero=num_entries_for_zero+1
     
     for(q in k:(dim(ALLfiles[[4]])[1]-1)){
       if(ALLfiles[[4]][q,1]!="#AYM00089022"){counter=counter+1}
       if(ALLfiles[[4]][q+1,1]=="#AYM00089022"){break}
     }
     Allfiles_4_list_hour_zero[[num_entries_for_zero]]=ALLfiles[[4]][k:(k+counter),]
     counter=0
   }
   
   if(ALLfiles[[4]][k,1]=="#AYM00089022"&ALLfiles[[4]][k,5]=="12"){
     num_entries_for_one=num_entries_for_one+1
     
     for(q in k:(dim(ALLfiles[[4]])[1]-1)){
       if(ALLfiles[[4]][q,1]!="#AYM00089022"){counter=counter+1}
       if(ALLfiles[[4]][q+1,1]=="#AYM00089022"){break}
     }
     Allfiles_4_list_hour_one[[num_entries_for_one]]=ALLfiles[[4]][k:(k+counter),]
     counter=0
   }
   
   
 }   
 
 Allfiles_4_list_hour_zero[[2]]
 Allfiles_4_list_hour_one[[2]]
 
 
 
 #######################################################################################################
 #######################################################################################################
 
 Allfiles_5_list_hour_zero=list()
 Allfiles_5_list_hour_one=list()
 ALLfiles[[5]]
 dim(ALLfiles[[5]])[1]
 
 counter=0
 num_entries_for_zero=0
 num_entries_for_one=0
 
 for(k in 1:dim(ALLfiles[[5]])[1]){
   if(ALLfiles[[5]][k,1]=="#AYM00089050"&ALLfiles[[5]][k,5]=="00"){
     num_entries_for_zero=num_entries_for_zero+1
     
     for(q in k:(dim(ALLfiles[[5]])[1]-1)){
       if(ALLfiles[[5]][q,1]!="#AYM00089050"){counter=counter+1}
       if(ALLfiles[[5]][q+1,1]=="#AYM00089050"){break}
     }
     Allfiles_5_list_hour_zero[[num_entries_for_zero]]=ALLfiles[[5]][k:(k+counter),]
     counter=0
   }
   
   if(ALLfiles[[5]][k,1]=="#AYM00089050"&ALLfiles[[5]][k,5]=="12"){
     num_entries_for_one=num_entries_for_one+1
     
     for(q in k:(dim(ALLfiles[[5]])[1]-1)){
       if(ALLfiles[[5]][q,1]!="#AYM00089050"){counter=counter+1}
       if(ALLfiles[[5]][q+1,1]=="#AYM00089050"){break}
     }
     Allfiles_5_list_hour_one[[num_entries_for_one]]=ALLfiles[[5]][k:(k+counter),]
     counter=0
   }
   
   
 }   
 
 Allfiles_5_list_hour_zero[[2]]
 Allfiles_5_list_hour_one[[2]]
 
 
 
 #######################################################################################################
 #######################################################################################################
 
 Allfiles_6_list_hour_zero=list()
 Allfiles_6_list_hour_one=list()
 ALLfiles[[6]]
 dim(ALLfiles[[6]])[1]
 
 counter=0
 num_entries_for_zero=0
 num_entries_for_one=0
 
 for(k in 1:dim(ALLfiles[[6]])[1]){
   if(ALLfiles[[6]][k,1]=="#AYM00089055"&ALLfiles[[6]][k,5]=="00"){
     num_entries_for_zero=num_entries_for_zero+1
     
     for(q in k:(dim(ALLfiles[[6]])[1]-1)){
       if(ALLfiles[[6]][q,1]!="#AYM00089055"){counter=counter+1}
       if(ALLfiles[[6]][q+1,1]=="#AYM00089055"){break}
     }
     Allfiles_6_list_hour_zero[[num_entries_for_zero]]=ALLfiles[[6]][k:(k+counter),]
     counter=0
   }
   
   if(ALLfiles[[6]][k,1]=="#AYM00089055"&ALLfiles[[6]][k,5]=="12"){
     num_entries_for_one=num_entries_for_one+1
     
     for(q in k:(dim(ALLfiles[[6]])[1]-1)){
       if(ALLfiles[[6]][q,1]!="#AYM00089055"){counter=counter+1}
       if(ALLfiles[[6]][q+1,1]=="#AYM00089055"){break}
     }
     Allfiles_6_list_hour_one[[num_entries_for_one]]=ALLfiles[[6]][k:(k+counter),]
     counter=0
   }
   
   
 }   
 
 Allfiles_6_list_hour_zero[[2]]
 Allfiles_6_list_hour_one[[2]]
 
 
 
 #######################################################################################################
 #######################################################################################################
 
 Allfiles_7_list_hour_zero=list()
 Allfiles_7_list_hour_one=list()
 ALLfiles[[7]]
 dim(ALLfiles[[7]])[1]
 
 counter=0
 num_entries_for_zero=0
 num_entries_for_one=0
 
 for(k in 1:dim(ALLfiles[[7]])[1]){
   if(ALLfiles[[7]][k,1]=="#AYM00089512"&ALLfiles[[7]][k,5]=="00"){
     num_entries_for_zero=num_entries_for_zero+1
     
     for(q in k:(dim(ALLfiles[[7]])[1]-1)){
       if(ALLfiles[[7]][q,1]!="#AYM00089512"){counter=counter+1}
       if(ALLfiles[[7]][q+1,1]=="#AYM00089512"){break}
     }
     Allfiles_7_list_hour_zero[[num_entries_for_zero]]=ALLfiles[[7]][k:(k+counter),]
     counter=0
   }
   
   if(ALLfiles[[7]][k,1]=="#AYM00089512"&ALLfiles[[7]][k,5]=="12"){
     num_entries_for_one=num_entries_for_one+1
     
     for(q in k:(dim(ALLfiles[[7]])[1]-1)){
       if(ALLfiles[[7]][q,1]!="#AYM00089512"){counter=counter+1}
       if(ALLfiles[[7]][q+1,1]=="#AYM00089512"){break}
     }
     Allfiles_7_list_hour_one[[num_entries_for_one]]=ALLfiles[[7]][k:(k+counter),]
     counter=0
   }
   
   
 }   
 
 Allfiles_7_list_hour_zero[[2]]
 Allfiles_7_list_hour_one[[2]]
 
 
 
 #######################################################################################################
 #######################################################################################################
 
 Allfiles_8_list_hour_zero=list()
 Allfiles_8_list_hour_one=list()
 ALLfiles[[8]]
 dim(ALLfiles[[8]])[1]
 
 counter=0
 num_entries_for_zero=0
 num_entries_for_one=0
 
 for(k in 1:dim(ALLfiles[[8]])[1]){
   if(ALLfiles[[8]][k,1]=="#AYM00089532"&ALLfiles[[8]][k,5]=="00"){
     num_entries_for_zero=num_entries_for_zero+1
     
     for(q in k:(dim(ALLfiles[[8]])[1]-1)){
       if(ALLfiles[[8]][q,1]!="#AYM00089532"){counter=counter+1}
       if(ALLfiles[[8]][q+1,1]=="#AYM00089532"){break}
     }
     Allfiles_8_list_hour_zero[[num_entries_for_zero]]=ALLfiles[[8]][k:(k+counter),]
     counter=0
   }
   
   if(ALLfiles[[8]][k,1]=="#AYM00089532"&ALLfiles[[8]][k,5]=="12"){
     num_entries_for_one=num_entries_for_one+1
     
     for(q in k:(dim(ALLfiles[[8]])[1]-1)){
       if(ALLfiles[[8]][q,1]!="#AYM00089532"){counter=counter+1}
       if(ALLfiles[[8]][q+1,1]=="#AYM00089532"){break}
     }
     Allfiles_8_list_hour_one[[num_entries_for_one]]=ALLfiles[[8]][k:(k+counter),]
     counter=0
   }
   
   
 }   
 
 Allfiles_8_list_hour_zero[[2]]
 Allfiles_8_list_hour_one[[2]]
 
 
 
 #######################################################################################################
 #######################################################################################################
 
 Allfiles_9_list_hour_zero=list()
 Allfiles_9_list_hour_one=list()
 ALLfiles[[9]]
 dim(ALLfiles[[9]])[1]
 
 counter=0
 num_entries_for_zero=0
 num_entries_for_one=0
 
 for(k in 1:dim(ALLfiles[[9]])[1]){
   if(ALLfiles[[9]][k,1]=="#AYM00089542"&ALLfiles[[9]][k,5]=="00"){
     num_entries_for_zero=num_entries_for_zero+1
     
     for(q in k:(dim(ALLfiles[[9]])[1]-1)){
       if(ALLfiles[[9]][q,1]!="#AYM00089542"){counter=counter+1}
       if(ALLfiles[[9]][q+1,1]=="#AYM00089542"){break}
     }
     Allfiles_9_list_hour_zero[[num_entries_for_zero]]=ALLfiles[[9]][k:(k+counter),]
     counter=0
   }
   
   if(ALLfiles[[9]][k,1]=="#AYM00089542"&ALLfiles[[9]][k,5]=="12"){
     num_entries_for_one=num_entries_for_one+1
     
     for(q in k:(dim(ALLfiles[[9]])[1]-1)){
       if(ALLfiles[[9]][q,1]!="#AYM00089542"){counter=counter+1}
       if(ALLfiles[[9]][q+1,1]=="#AYM00089542"){break}
     }
     Allfiles_9_list_hour_one[[num_entries_for_one]]=ALLfiles[[9]][k:(k+counter),]
     counter=0
   }
   
   
 }   
 
 Allfiles_9_list_hour_zero[[2]]
 Allfiles_9_list_hour_one[[2]]
 
 
 
 #######################################################################################################
 #######################################################################################################
 
 Allfiles_10_list_hour_zero=list()
 Allfiles_10_list_hour_one=list()
 ALLfiles[[10]]
 dim(ALLfiles[[10]])[1]
 
 counter=0
 num_entries_for_zero=0
 num_entries_for_one=0
 
 for(k in 1:dim(ALLfiles[[10]])[1]){
   if(ALLfiles[[10]][k,1]=="#AYM00089564"&ALLfiles[[10]][k,5]=="00"){
     num_entries_for_zero=num_entries_for_zero+1
     
     for(q in k:(dim(ALLfiles[[10]])[1]-1)){
       if(ALLfiles[[10]][q,1]!="#AYM00089564"){counter=counter+1}
       if(ALLfiles[[10]][q+1,1]=="#AYM00089564"){break}
     }
     Allfiles_10_list_hour_zero[[num_entries_for_zero]]=ALLfiles[[10]][k:(k+counter),]
     counter=0
   }
   
   if(ALLfiles[[10]][k,1]=="#AYM00089564"&ALLfiles[[10]][k,5]=="12"){
     num_entries_for_one=num_entries_for_one+1
     
     for(q in k:(dim(ALLfiles[[10]])[1]-1)){
       if(ALLfiles[[10]][q,1]!="#AYM00089564"){counter=counter+1}
       if(ALLfiles[[10]][q+1,1]=="#AYM00089564"){break}
     }
     Allfiles_10_list_hour_one[[num_entries_for_one]]=ALLfiles[[10]][k:(k+counter),]
     counter=0
   }
   
   
 }   
 
 Allfiles_10_list_hour_zero[[2]]
 Allfiles_10_list_hour_one[[200]]
 
 
 
 #######################################################################################################
 #######################################################################################################
 
 Allfiles_11_list_hour_zero=list()
 Allfiles_11_list_hour_one=list()
 ALLfiles[[11]]
 dim(ALLfiles[[11]])[1]
 
 counter=0
 num_entries_for_zero=0
 num_entries_for_one=0
 
 for(k in 1:dim(ALLfiles[[11]])[1]){
   if(ALLfiles[[11]][k,1]=="#AYM00089571"&ALLfiles[[11]][k,5]=="00"){
     num_entries_for_zero=num_entries_for_zero+1
     
     for(q in k:(dim(ALLfiles[[11]])[1]-1)){
       if(ALLfiles[[11]][q,1]!="#AYM00089571"){counter=counter+1}
       if(ALLfiles[[11]][q+1,1]=="#AYM00089571"){break}
     }
     Allfiles_11_list_hour_zero[[num_entries_for_zero]]=ALLfiles[[11]][k:(k+counter),]
     counter=0
   }
   
   if(ALLfiles[[11]][k,1]=="#AYM00089571"&ALLfiles[[11]][k,5]=="12"){
     num_entries_for_one=num_entries_for_one+1
     
     for(q in k:(dim(ALLfiles[[11]])[1]-1)){
       if(ALLfiles[[11]][q,1]!="#AYM00089571"){counter=counter+1}
       if(ALLfiles[[11]][q+1,1]=="#AYM00089571"){break}
     }
     Allfiles_11_list_hour_one[[num_entries_for_one]]=ALLfiles[[11]][k:(k+counter),]
     counter=0
   }
   
   
 }   
 
 Allfiles_11_list_hour_zero[[2]]
 Allfiles_11_list_hour_one[[2]]
 
 
 
 #######################################################################################################
 #######################################################################################################
 
 Allfiles_12_list_hour_zero=list()
 Allfiles_12_list_hour_one=list()
 ALLfiles[[12]]
 dim(ALLfiles[[12]])[1]
 
 counter=0
 num_entries_for_zero=0
 num_entries_for_one=0
 
 for(k in 1:dim(ALLfiles[[12]])[1]){
   if(ALLfiles[[12]][k,1]=="#AYM00089592"&ALLfiles[[12]][k,5]=="00"){
     num_entries_for_zero=num_entries_for_zero+1
     
     for(q in k:(dim(ALLfiles[[12]])[1]-1)){
       if(ALLfiles[[12]][q,1]!="#AYM00089592"){counter=counter+1}
       if(ALLfiles[[12]][q+1,1]=="#AYM00089592"){break}
     }
     Allfiles_12_list_hour_zero[[num_entries_for_zero]]=ALLfiles[[12]][k:(k+counter),]
     counter=0
   }
   
   if(ALLfiles[[12]][k,1]=="#AYM00089592"&ALLfiles[[12]][k,5]=="12"){
     num_entries_for_one=num_entries_for_one+1
     
     for(q in k:(dim(ALLfiles[[12]])[1]-1)){
       if(ALLfiles[[12]][q,1]!="#AYM00089592"){counter=counter+1}
       if(ALLfiles[[12]][q+1,1]=="#AYM00089592"){break}
     }
     Allfiles_12_list_hour_one[[num_entries_for_one]]=ALLfiles[[12]][k:(k+counter),]
     counter=0
   }
   
   
 }   
 
 Allfiles_12_list_hour_zero[[2]]
 Allfiles_12_list_hour_one[[2]]
 
 
 
 #######################################################################################################
 #######################################################################################################
 
 Allfiles_13_list_hour_zero=list()
 Allfiles_13_list_hour_one=list()
 ALLfiles[[13]]
 dim(ALLfiles[[13]])[1]
 
 counter=0
 num_entries_for_zero=0
 num_entries_for_one=0
 
 for(k in 1:dim(ALLfiles[[13]])[1]){
   if(ALLfiles[[13]][k,1]=="#AYM00089606"&ALLfiles[[13]][k,5]=="00"){
     num_entries_for_zero=num_entries_for_zero+1
     
     for(q in k:(dim(ALLfiles[[13]])[1]-1)){
       if(ALLfiles[[13]][q,1]!="#AYM00089606"){counter=counter+1}
       if(ALLfiles[[13]][q+1,1]=="#AYM00089606"){break}
     }
     Allfiles_13_list_hour_zero[[num_entries_for_zero]]=ALLfiles[[13]][k:(k+counter),]
     counter=0
   }
   
   if(ALLfiles[[13]][k,1]=="#AYM00089606"&ALLfiles[[13]][k,5]=="12"){
     num_entries_for_one=num_entries_for_one+1
     
     for(q in k:(dim(ALLfiles[[13]])[1]-1)){
       if(ALLfiles[[13]][q,1]!="#AYM00089606"){counter=counter+1}
       if(ALLfiles[[13]][q+1,1]=="#AYM00089606"){break}
     }
     Allfiles_13_list_hour_one[[num_entries_for_one]]=ALLfiles[[13]][k:(k+counter),]
     counter=0
   }
   
   
 }   
 
 Allfiles_13_list_hour_zero[[2]]
 Allfiles_13_list_hour_one[[2]]
 
 
 
 #######################################################################################################
 #######################################################################################################
 
 Allfiles_14_list_hour_zero=list()
 Allfiles_14_list_hour_one=list()
 ALLfiles[[14]]
 dim(ALLfiles[[14]])[1]
 
 counter=0
 num_entries_for_zero=0
 num_entries_for_one=0
 
 for(k in 1:dim(ALLfiles[[14]])[1]){
   if(ALLfiles[[14]][k,1]=="#AYM00089611"&ALLfiles[[14]][k,5]=="00"){
     num_entries_for_zero=num_entries_for_zero+1
     
     for(q in k:(dim(ALLfiles[[14]])[1]-1)){
       if(ALLfiles[[14]][q,1]!="#AYM00089611"){counter=counter+1}
       if(ALLfiles[[14]][q+1,1]=="#AYM00089611"){break}
     }
     Allfiles_14_list_hour_zero[[num_entries_for_zero]]=ALLfiles[[14]][k:(k+counter),]
     counter=0
   }
   
   if(ALLfiles[[14]][k,1]=="#AYM00089611"&ALLfiles[[14]][k,5]=="12"){
     num_entries_for_one=num_entries_for_one+1
     
     for(q in k:(dim(ALLfiles[[14]])[1]-1)){
       if(ALLfiles[[14]][q,1]!="#AYM00089611"){counter=counter+1}
       if(ALLfiles[[14]][q+1,1]=="#AYM00089611"){break}
     }
     Allfiles_14_list_hour_one[[num_entries_for_one]]=ALLfiles[[14]][k:(k+counter),]
     counter=0
   }
   
   
 }   
 
 Allfiles_14_list_hour_zero[[2]]
 Allfiles_14_list_hour_one[[2]]
 
 ######################################################################################################
 #times two below since half days... 
 All_data_matrix=matrix(rep(NA,14*length(pressure_vector*num_days_between_first_and_last*2)),14*length(pressure_vector),num_days_between_first_and_last*2) 
 ####################################################################################################### 
 
 #######################################################################################################
 
 
  
for(t in 1:length(Allfiles_1_list_hour_zero)){
  
  for(p in 1:length(pressure_vector)){
    #will need to change p to p+15*number of prev stations for stations other than station 1
   
    if(!identical(Allfiles_1_list_hour_zero[[t]][which(strsplit(as.character(strsplit(Allfiles_1_list_hour_zero[[t]][,3],"B")),"A")==as.character(pressure_vector[p])),3], character(0))){
     All_data_matrix[p,2*(-as.numeric(as.Date("5/29/1954", "%m/%d/%Y"))+as.numeric(as.Date(paste(Allfiles_1_list_hour_zero[[t]][1,2],Allfiles_1_list_hour_zero[[t]][1,3],Allfiles_1_list_hour_zero[[t]][1,4]),"%Y %m %d")))-1]=0.1*as.numeric((strsplit(as.character(strsplit(substr(Allfiles_1_list_hour_zero[[t]][which(strsplit(as.character(strsplit(Allfiles_1_list_hour_zero[[t]][,3],"B")),"A")==as.character(pressure_vector[p])),5],1,4),"B")),"A"))[[1]][1])    }
  }
    
    
    
  }
  
 
for(t in 1:length(Allfiles_1_list_hour_one)){
  
  for(p in 1:length(pressure_vector)){
    #will need to change p to p+15*number of prev stations for stations other than station 1
    
    if(!identical(Allfiles_1_list_hour_one[[t]][which(strsplit(as.character(strsplit(Allfiles_1_list_hour_one[[t]][,3],"B")),"A")==as.character(pressure_vector[p])),3], character(0))){
      All_data_matrix[p,2*(-as.numeric(as.Date("5/29/1954", "%m/%d/%Y"))+as.numeric(as.Date(paste(Allfiles_1_list_hour_one[[t]][1,2],Allfiles_1_list_hour_one[[t]][1,3],Allfiles_1_list_hour_one[[t]][1,4]),"%Y %m %d")))]=0.1*as.numeric((strsplit(as.character(strsplit(substr(Allfiles_1_list_hour_one[[t]][which(strsplit(as.character(strsplit(Allfiles_1_list_hour_one[[t]][,3],"B")),"A")==as.character(pressure_vector[p])),5],1,4),"B")),"A"))[[1]][1])
    }
  }
  
  
  
}




  plot(All_data_matrix[14,])
  All_data_matrix[14,15000:16000]

  
  #NEED TO CLEAR 99.9s and 88.8s... 
  #######################################################################################################
 #######################################################################################################
  
  #######################################################################################################
  
  
  
  for(t in 1:length(Allfiles_2_list_hour_zero)){
    
    for(p in 1:length(pressure_vector)){
      #will need to change p to p+15*number of prev stations for stations other than station 1
      
      if(!identical(Allfiles_2_list_hour_zero[[t]][which(strsplit(as.character(strsplit(Allfiles_2_list_hour_zero[[t]][,3],"B")),"A")==as.character(pressure_vector[p])),3], character(0))){
        All_data_matrix[(p+15),2*(-as.numeric(as.Date("5/29/1954", "%m/%d/%Y"))+as.numeric(as.Date(paste(Allfiles_2_list_hour_zero[[t]][1,2],Allfiles_2_list_hour_zero[[t]][1,3],Allfiles_2_list_hour_zero[[t]][1,4]),"%Y %m %d")))-1]=0.1*as.numeric((strsplit(as.character(strsplit(substr(Allfiles_2_list_hour_zero[[t]][which(strsplit(as.character(strsplit(Allfiles_2_list_hour_zero[[t]][,3],"B")),"A")==as.character(pressure_vector[p])),5],1,4),"B")),"A"))[[1]][1])    }
    }
    
    
    
  }
  
  
  for(t in 1:length(Allfiles_2_list_hour_one)){
    
    for(p in 1:length(pressure_vector)){
      #will need to change p to p+15*number of prev stations for stations other than station 1
      
      if(!identical(Allfiles_2_list_hour_one[[t]][which(strsplit(as.character(strsplit(Allfiles_2_list_hour_one[[t]][,3],"B")),"A")==as.character(pressure_vector[p])),3], character(0))){
        All_data_matrix[(p+15),2*(-as.numeric(as.Date("5/29/1954", "%m/%d/%Y"))+as.numeric(as.Date(paste(Allfiles_2_list_hour_one[[t]][1,2],Allfiles_2_list_hour_one[[t]][1,3],Allfiles_2_list_hour_one[[t]][1,4]),"%Y %m %d")))]=0.1*as.numeric((strsplit(as.character(strsplit(substr(Allfiles_2_list_hour_one[[t]][which(strsplit(as.character(strsplit(Allfiles_2_list_hour_one[[t]][,3],"B")),"A")==as.character(pressure_vector[p])),5],1,4),"B")),"A"))[[1]][1])
      }
    }
    
    
    
  }
  
 
  #NEED TO CLEAR 99.9s and 88.8s... 
  #######################################################################################################
  #######################################################################################################
  #######################################################################################################
  
  
  
  for(t in 1:length(Allfiles_3_list_hour_zero)){
    
    for(p in 1:length(pressure_vector)){
      #will need to change p to p+15*number of prev stations for stations other than station 1
      
      if(!identical(Allfiles_3_list_hour_zero[[t]][which(strsplit(as.character(strsplit(Allfiles_3_list_hour_zero[[t]][,3],"B")),"A")==as.character(pressure_vector[p])),3], character(0))){
        All_data_matrix[p+30,2*(-as.numeric(as.Date("5/29/1954", "%m/%d/%Y"))+as.numeric(as.Date(paste(Allfiles_3_list_hour_zero[[t]][1,2],Allfiles_3_list_hour_zero[[t]][1,3],Allfiles_3_list_hour_zero[[t]][1,4]),"%Y %m %d")))-1]=0.1*as.numeric((strsplit(as.character(strsplit(substr(Allfiles_3_list_hour_zero[[t]][which(strsplit(as.character(strsplit(Allfiles_3_list_hour_zero[[t]][,3],"B")),"A")==as.character(pressure_vector[p])),5],1,4),"B")),"A"))[[1]][1])    }
    }
    
     }
  
 
  
  
  
  
  
  
  
  for(t in 1:length(Allfiles_3_list_hour_one)){
    
    for(p in 1:length(pressure_vector)){
      #will need to change p to p+15*number of prev stations for stations other than station 1
      
      if(!identical(Allfiles_3_list_hour_one[[t]][which(strsplit(as.character(strsplit(Allfiles_3_list_hour_one[[t]][,3],"B")),"A")==as.character(pressure_vector[p])),3], character(0))){
        All_data_matrix[p+30,2*(-as.numeric(as.Date("5/29/1954", "%m/%d/%Y"))+as.numeric(as.Date(paste(Allfiles_3_list_hour_one[[t]][1,2],Allfiles_3_list_hour_one[[t]][1,3],Allfiles_3_list_hour_one[[t]][1,4]),"%Y %m %d")))]=0.1*as.numeric((strsplit(as.character(strsplit(substr(Allfiles_3_list_hour_one[[t]][which(strsplit(as.character(strsplit(Allfiles_3_list_hour_one[[t]][,3],"B")),"A")==as.character(pressure_vector[p])),5],1,4),"B")),"A"))[[1]][1])
      }
    }
    
    
    
  }
  
  
  
  
  plot(All_data_matrix[14,])
  All_data_matrix[14,15000:16000]
  
  
  #NEED TO CLEAR 99.9s and 88.8s... 
  #######################################################################################################
  #######################################################################################################
  #######################################################################################################
  
  
  
  for(t in 1:length(Allfiles_4_list_hour_zero)){
    
    for(p in 1:length(pressure_vector)){
      #will need to change p to p+15*number of prev stations for stations other than station 1
      
      if(!identical(Allfiles_4_list_hour_zero[[t]][which(strsplit(as.character(strsplit(Allfiles_4_list_hour_zero[[t]][,3],"B")),"A")==as.character(pressure_vector[p])),3], character(0))){
        All_data_matrix[p+45,2*(-as.numeric(as.Date("5/29/1954", "%m/%d/%Y"))+as.numeric(as.Date(paste(Allfiles_4_list_hour_zero[[t]][1,2],Allfiles_4_list_hour_zero[[t]][1,3],Allfiles_4_list_hour_zero[[t]][1,4]),"%Y %m %d")))-1]=0.1*as.numeric((strsplit(as.character(strsplit(substr(Allfiles_4_list_hour_zero[[t]][which(strsplit(as.character(strsplit(Allfiles_4_list_hour_zero[[t]][,3],"B")),"A")==as.character(pressure_vector[p])),5],1,4),"B")),"A"))[[1]][1])    }
    }
    
    
    
  }
  
  
  for(t in 1:length(Allfiles_4_list_hour_one)){
    
    for(p in 1:length(pressure_vector)){
      #will need to change p to p+15*number of prev stations for stations other than station 1
      
      if(!identical(Allfiles_4_list_hour_one[[t]][which(strsplit(as.character(strsplit(Allfiles_4_list_hour_one[[t]][,3],"B")),"A")==as.character(pressure_vector[p])),3], character(0))){
        All_data_matrix[p+45,2*(-as.numeric(as.Date("5/29/1954", "%m/%d/%Y"))+as.numeric(as.Date(paste(Allfiles_4_list_hour_one[[t]][1,2],Allfiles_4_list_hour_one[[t]][1,3],Allfiles_4_list_hour_one[[t]][1,4]),"%Y %m %d")))]=0.1*as.numeric((strsplit(as.character(strsplit(substr(Allfiles_4_list_hour_one[[t]][which(strsplit(as.character(strsplit(Allfiles_4_list_hour_one[[t]][,3],"B")),"A")==as.character(pressure_vector[p])),5],1,4),"B")),"A"))[[1]][1])
      }
    }
    
    
    
  }
  
  
  
  
  plot(All_data_matrix[14,])
  All_data_matrix[14,15000:16000]
  
  
  #NEED TO CLEAR 99.9s and 88.8s... 
  #######################################################################################################
  #######################################################################################################
  #######################################################################################################
  
  
  
  for(t in 1:length(Allfiles_5_list_hour_zero)){
    
    for(p in 1:length(pressure_vector)){
      #will need to change p to p+15*number of prev stations for stations other than station 1
      
      if(!identical(Allfiles_5_list_hour_zero[[t]][which(strsplit(as.character(strsplit(Allfiles_5_list_hour_zero[[t]][,3],"B")),"A")==as.character(pressure_vector[p])),3], character(0))){
        All_data_matrix[p+60,2*(-as.numeric(as.Date("5/29/1954", "%m/%d/%Y"))+as.numeric(as.Date(paste(Allfiles_5_list_hour_zero[[t]][1,2],Allfiles_5_list_hour_zero[[t]][1,3],Allfiles_5_list_hour_zero[[t]][1,4]),"%Y %m %d")))-1]=0.1*as.numeric((strsplit(as.character(strsplit(substr(Allfiles_5_list_hour_zero[[t]][which(strsplit(as.character(strsplit(Allfiles_5_list_hour_zero[[t]][,3],"B")),"A")==as.character(pressure_vector[p])),5],1,4),"B")),"A"))[[1]][1])    }
    }
    
    
    
  }
  
  
  for(t in 1:length(Allfiles_5_list_hour_one)){
    
    for(p in 1:length(pressure_vector)){
      #will need to change p to p+15*number of prev stations for stations other than station 1
      
      if(!identical(Allfiles_5_list_hour_one[[t]][which(strsplit(as.character(strsplit(Allfiles_5_list_hour_one[[t]][,3],"B")),"A")==as.character(pressure_vector[p])),3], character(0))){
        All_data_matrix[p+60,2*(-as.numeric(as.Date("5/29/1954", "%m/%d/%Y"))+as.numeric(as.Date(paste(Allfiles_5_list_hour_one[[t]][1,2],Allfiles_5_list_hour_one[[t]][1,3],Allfiles_5_list_hour_one[[t]][1,4]),"%Y %m %d")))]=0.1*as.numeric((strsplit(as.character(strsplit(substr(Allfiles_5_list_hour_one[[t]][which(strsplit(as.character(strsplit(Allfiles_5_list_hour_one[[t]][,3],"B")),"A")==as.character(pressure_vector[p])),5],1,4),"B")),"A"))[[1]][1])
      }
    }
    
    
    
  }
  
  
  
  
  plot(All_data_matrix[14,])
  All_data_matrix[14,15000:16000]
  
  
  #NEED TO CLEAR 99.9s and 88.8s... 
  #######################################################################################################
  #######################################################################################################
  #######################################################################################################
  
  
  
  for(t in 1:length(Allfiles_6_list_hour_zero)){
    
    for(p in 1:length(pressure_vector)){
      #will need to change p to p+15*number of prev stations for stations other than station 1
      
      if(!identical(Allfiles_6_list_hour_zero[[t]][which(strsplit(as.character(strsplit(Allfiles_6_list_hour_zero[[t]][,3],"B")),"A")==as.character(pressure_vector[p])),3], character(0))){
        All_data_matrix[p+75,2*(-as.numeric(as.Date("5/29/1954", "%m/%d/%Y"))+as.numeric(as.Date(paste(Allfiles_6_list_hour_zero[[t]][1,2],Allfiles_6_list_hour_zero[[t]][1,3],Allfiles_6_list_hour_zero[[t]][1,4]),"%Y %m %d")))-1]=0.1*as.numeric((strsplit(as.character(strsplit(substr(Allfiles_6_list_hour_zero[[t]][which(strsplit(as.character(strsplit(Allfiles_6_list_hour_zero[[t]][,3],"B")),"A")==as.character(pressure_vector[p])),5],1,4),"B")),"A"))[[1]][1])    }
    }
    
    
    
  }
  
  
  for(t in 1:length(Allfiles_6_list_hour_one)){
    
    for(p in 1:length(pressure_vector)){
      #will need to change p to p+15*number of prev stations for stations other than station 1
      
      if(!identical(Allfiles_6_list_hour_one[[t]][which(strsplit(as.character(strsplit(Allfiles_6_list_hour_one[[t]][,3],"B")),"A")==as.character(pressure_vector[p])),3], character(0))){
        All_data_matrix[p+75,2*(-as.numeric(as.Date("5/29/1954", "%m/%d/%Y"))+as.numeric(as.Date(paste(Allfiles_6_list_hour_one[[t]][1,2],Allfiles_6_list_hour_one[[t]][1,3],Allfiles_6_list_hour_one[[t]][1,4]),"%Y %m %d")))]=0.1*as.numeric((strsplit(as.character(strsplit(substr(Allfiles_6_list_hour_one[[t]][which(strsplit(as.character(strsplit(Allfiles_6_list_hour_one[[t]][,3],"B")),"A")==as.character(pressure_vector[p])),5],1,4),"B")),"A"))[[1]][1])
      }
    }
    
    
    
  }
  
  
  
  
  plot(All_data_matrix[14,])
  All_data_matrix[14,15000:16000]
  
  
  #NEED TO CLEAR 99.9s and 88.8s... 
  #######################################################################################################
  #######################################################################################################
  #######################################################################################################
  
  
  
  for(t in 1:length(Allfiles_7_list_hour_zero)){
    
    for(p in 1:length(pressure_vector)){
      #will need to change p to p+15*number of prev stations for stations other than station 1
      
      if(!identical(Allfiles_7_list_hour_zero[[t]][which(strsplit(as.character(strsplit(Allfiles_7_list_hour_zero[[t]][,3],"B")),"A")==as.character(pressure_vector[p])),3], character(0))){
        All_data_matrix[p+90,2*(-as.numeric(as.Date("5/29/1954", "%m/%d/%Y"))+as.numeric(as.Date(paste(Allfiles_7_list_hour_zero[[t]][1,2],Allfiles_7_list_hour_zero[[t]][1,3],Allfiles_7_list_hour_zero[[t]][1,4]),"%Y %m %d")))-1]=0.1*as.numeric((strsplit(as.character(strsplit(substr(Allfiles_7_list_hour_zero[[t]][which(strsplit(as.character(strsplit(Allfiles_7_list_hour_zero[[t]][,3],"B")),"A")==as.character(pressure_vector[p])),5],1,4),"B")),"A"))[[1]][1])    }
    }
    
    
    
  }
  
  
  for(t in 1:length(Allfiles_7_list_hour_one)){
    
    for(p in 1:length(pressure_vector)){
      #will need to change p to p+15*number of prev stations for stations other than station 1
      
      if(!identical(Allfiles_7_list_hour_one[[t]][which(strsplit(as.character(strsplit(Allfiles_7_list_hour_one[[t]][,3],"B")),"A")==as.character(pressure_vector[p])),3], character(0))){
        All_data_matrix[p+90,2*(-as.numeric(as.Date("5/29/1954", "%m/%d/%Y"))+as.numeric(as.Date(paste(Allfiles_7_list_hour_one[[t]][1,2],Allfiles_7_list_hour_one[[t]][1,3],Allfiles_7_list_hour_one[[t]][1,4]),"%Y %m %d")))]=0.1*as.numeric((strsplit(as.character(strsplit(substr(Allfiles_7_list_hour_one[[t]][which(strsplit(as.character(strsplit(Allfiles_7_list_hour_one[[t]][,3],"B")),"A")==as.character(pressure_vector[p])),5],1,4),"B")),"A"))[[1]][1])
      }
    }
    
    
    
  }
  
  
  
  
  plot(All_data_matrix[14,])
  All_data_matrix[14,15000:16000]
  
  
  #NEED TO CLEAR 99.9s and 88.8s... 
  #######################################################################################################
  #######################################################################################################
  #######################################################################################################
  
  
  
  for(t in 1:length(Allfiles_8_list_hour_zero)){
    
    for(p in 1:length(pressure_vector)){
      #will need to change p to p+15*number of prev stations for stations other than station 1
      
      if(!identical(Allfiles_8_list_hour_zero[[t]][which(strsplit(as.character(strsplit(Allfiles_8_list_hour_zero[[t]][,3],"B")),"A")==as.character(pressure_vector[p])),3], character(0))){
        All_data_matrix[p+105,2*(-as.numeric(as.Date("5/29/1954", "%m/%d/%Y"))+as.numeric(as.Date(paste(Allfiles_8_list_hour_zero[[t]][1,2],Allfiles_8_list_hour_zero[[t]][1,3],Allfiles_8_list_hour_zero[[t]][1,4]),"%Y %m %d")))-1]=0.1*as.numeric((strsplit(as.character(strsplit(substr(Allfiles_8_list_hour_zero[[t]][which(strsplit(as.character(strsplit(Allfiles_8_list_hour_zero[[t]][,3],"B")),"A")==as.character(pressure_vector[p])),5],1,4),"B")),"A"))[[1]][1])    }
    }
    
    
    
  }
  
  
  for(t in 1:length(Allfiles_8_list_hour_one)){
    
    for(p in 1:length(pressure_vector)){
      #will need to change p to p+15*number of prev stations for stations other than station 1
      
      if(!identical(Allfiles_8_list_hour_one[[t]][which(strsplit(as.character(strsplit(Allfiles_8_list_hour_one[[t]][,3],"B")),"A")==as.character(pressure_vector[p])),3], character(0))){
        All_data_matrix[p+105,2*(-as.numeric(as.Date("5/29/1954", "%m/%d/%Y"))+as.numeric(as.Date(paste(Allfiles_8_list_hour_one[[t]][1,2],Allfiles_8_list_hour_one[[t]][1,3],Allfiles_8_list_hour_one[[t]][1,4]),"%Y %m %d")))]=0.1*as.numeric((strsplit(as.character(strsplit(substr(Allfiles_8_list_hour_one[[t]][which(strsplit(as.character(strsplit(Allfiles_8_list_hour_one[[t]][,3],"B")),"A")==as.character(pressure_vector[p])),5],1,4),"B")),"A"))[[1]][1])
      }
    }
    
    
    
  }
  
  
  
  
  plot(All_data_matrix[14,])
  All_data_matrix[14,15000:16000]
  
  
  #NEED TO CLEAR 99.9s and 88.8s... 
  #######################################################################################################
  #######################################################################################################
  #######################################################################################################
  
  
  
  for(t in 1:length(Allfiles_9_list_hour_zero)){
    
    for(p in 1:length(pressure_vector)){
      #will need to change p to p+15*number of prev stations for stations other than station 1
      
      if(!identical(Allfiles_9_list_hour_zero[[t]][which(strsplit(as.character(strsplit(Allfiles_9_list_hour_zero[[t]][,3],"B")),"A")==as.character(pressure_vector[p])),3], character(0))){
        All_data_matrix[p+120,2*(-as.numeric(as.Date("5/29/1954", "%m/%d/%Y"))+as.numeric(as.Date(paste(Allfiles_9_list_hour_zero[[t]][1,2],Allfiles_9_list_hour_zero[[t]][1,3],Allfiles_9_list_hour_zero[[t]][1,4]),"%Y %m %d")))-1]=0.1*as.numeric((strsplit(as.character(strsplit(substr(Allfiles_9_list_hour_zero[[t]][which(strsplit(as.character(strsplit(Allfiles_9_list_hour_zero[[t]][,3],"B")),"A")==as.character(pressure_vector[p])),5],1,4),"B")),"A"))[[1]][1])    }
    }
    
    
    
  }
  
  
  for(t in 1:length(Allfiles_9_list_hour_one)){
    
    for(p in 1:length(pressure_vector)){
      #will need to change p to p+15*number of prev stations for stations other than station 1
      
      if(!identical(Allfiles_9_list_hour_one[[t]][which(strsplit(as.character(strsplit(Allfiles_9_list_hour_one[[t]][,3],"B")),"A")==as.character(pressure_vector[p])),3], character(0))){
        All_data_matrix[p+120,2*(-as.numeric(as.Date("5/29/1954", "%m/%d/%Y"))+as.numeric(as.Date(paste(Allfiles_9_list_hour_one[[t]][1,2],Allfiles_9_list_hour_one[[t]][1,3],Allfiles_9_list_hour_one[[t]][1,4]),"%Y %m %d")))]=0.1*as.numeric((strsplit(as.character(strsplit(substr(Allfiles_9_list_hour_one[[t]][which(strsplit(as.character(strsplit(Allfiles_9_list_hour_one[[t]][,3],"B")),"A")==as.character(pressure_vector[p])),5],1,4),"B")),"A"))[[1]][1])
      }
    }
    
    
    
  }
  
  
  
  
  plot(All_data_matrix[14,])
  All_data_matrix[14,15000:16000]
  
  
  #NEED TO CLEAR 99.9s and 88.8s... 
  #######################################################################################################
  #######################################################################################################
  #######################################################################################################
  
  
  
  for(t in 1:length(Allfiles_10_list_hour_zero)){
    
    for(p in 1:length(pressure_vector)){
      #will need to change p to p+15*number of prev stations for stations other than station 1
      
      if(!identical(Allfiles_10_list_hour_zero[[t]][which(strsplit(as.character(strsplit(Allfiles_10_list_hour_zero[[t]][,3],"B")),"A")==as.character(pressure_vector[p])),3], character(0))){
        All_data_matrix[p+135,2*(-as.numeric(as.Date("5/29/1954", "%m/%d/%Y"))+as.numeric(as.Date(paste(Allfiles_10_list_hour_zero[[t]][1,2],Allfiles_10_list_hour_zero[[t]][1,3],Allfiles_10_list_hour_zero[[t]][1,4]),"%Y %m %d")))-1]=0.1*as.numeric((strsplit(as.character(strsplit(substr(Allfiles_10_list_hour_zero[[t]][which(strsplit(as.character(strsplit(Allfiles_10_list_hour_zero[[t]][,3],"B")),"A")==as.character(pressure_vector[p])),5],1,4),"B")),"A"))[[1]][1])    }
    }
    
    
    
  }
  
  
  for(t in 1:length(Allfiles_10_list_hour_one)){
    
    for(p in 1:length(pressure_vector)){
      #will need to change p to p+15*number of prev stations for stations other than station 1
      
      if(!identical(Allfiles_10_list_hour_one[[t]][which(strsplit(as.character(strsplit(Allfiles_10_list_hour_one[[t]][,3],"B")),"A")==as.character(pressure_vector[p])),3], character(0))){
        All_data_matrix[p+135,2*(-as.numeric(as.Date("5/29/1954", "%m/%d/%Y"))+as.numeric(as.Date(paste(Allfiles_10_list_hour_one[[t]][1,2],Allfiles_10_list_hour_one[[t]][1,3],Allfiles_10_list_hour_one[[t]][1,4]),"%Y %m %d")))]=0.1*as.numeric((strsplit(as.character(strsplit(substr(Allfiles_10_list_hour_one[[t]][which(strsplit(as.character(strsplit(Allfiles_10_list_hour_one[[t]][,3],"B")),"A")==as.character(pressure_vector[p])),5],1,4),"B")),"A"))[[1]][1])
      }
    }
    
    
    
  }
  
  
  
  
  plot(All_data_matrix[14,])
  All_data_matrix[14,15000:16000]
  
  
  #NEED TO CLEAR 99.9s and 88.8s... 
  #######################################################################################################
  #######################################################################################################
  #######################################################################################################
  
  
  
  for(t in 1:length(Allfiles_11_list_hour_zero)){
    
    for(p in 1:length(pressure_vector)){
      #will need to change p to p+15*number of prev stations for stations other than station 1
      
      if(!identical(Allfiles_11_list_hour_zero[[t]][which(strsplit(as.character(strsplit(Allfiles_11_list_hour_zero[[t]][,3],"B")),"A")==as.character(pressure_vector[p])),3], character(0))){
        All_data_matrix[p+150,2*(-as.numeric(as.Date("5/29/1954", "%m/%d/%Y"))+as.numeric(as.Date(paste(Allfiles_11_list_hour_zero[[t]][1,2],Allfiles_11_list_hour_zero[[t]][1,3],Allfiles_11_list_hour_zero[[t]][1,4]),"%Y %m %d")))-1]=0.1*as.numeric((strsplit(as.character(strsplit(substr(Allfiles_11_list_hour_zero[[t]][which(strsplit(as.character(strsplit(Allfiles_11_list_hour_zero[[t]][,3],"B")),"A")==as.character(pressure_vector[p])),5],1,4),"B")),"A"))[[1]][1])    }
    }
    
    
    
  }
  
  
  for(t in 1:length(Allfiles_11_list_hour_one)){
    
    for(p in 1:length(pressure_vector)){
      #will need to change p to p+15*number of prev stations for stations other than station 1
      
      if(!identical(Allfiles_11_list_hour_one[[t]][which(strsplit(as.character(strsplit(Allfiles_11_list_hour_one[[t]][,3],"B")),"A")==as.character(pressure_vector[p])),3], character(0))){
        All_data_matrix[p+150,2*(-as.numeric(as.Date("5/29/1954", "%m/%d/%Y"))+as.numeric(as.Date(paste(Allfiles_11_list_hour_one[[t]][1,2],Allfiles_11_list_hour_one[[t]][1,3],Allfiles_11_list_hour_one[[t]][1,4]),"%Y %m %d")))]=0.1*as.numeric((strsplit(as.character(strsplit(substr(Allfiles_11_list_hour_one[[t]][which(strsplit(as.character(strsplit(Allfiles_11_list_hour_one[[t]][,3],"B")),"A")==as.character(pressure_vector[p])),5],1,4),"B")),"A"))[[1]][1])
      }
    }
    
    
    
  }
  
  
  
  
  plot(All_data_matrix[14,])
  All_data_matrix[14,15000:16000]
  
  
  #NEED TO CLEAR 99.9s and 88.8s... 
  #######################################################################################################
  #######################################################################################################
  #######################################################################################################
  
  
  
  for(t in 1:length(Allfiles_12_list_hour_zero)){
    
    for(p in 1:length(pressure_vector)){
      #will need to change p to p+15*number of prev stations for stations other than station 1
      
      if(!identical(Allfiles_12_list_hour_zero[[t]][which(strsplit(as.character(strsplit(Allfiles_12_list_hour_zero[[t]][,3],"B")),"A")==as.character(pressure_vector[p])),3], character(0))){
        All_data_matrix[p+165,2*(-as.numeric(as.Date("5/29/1954", "%m/%d/%Y"))+as.numeric(as.Date(paste(Allfiles_12_list_hour_zero[[t]][1,2],Allfiles_12_list_hour_zero[[t]][1,3],Allfiles_12_list_hour_zero[[t]][1,4]),"%Y %m %d")))-1]=0.1*as.numeric((strsplit(as.character(strsplit(substr(Allfiles_12_list_hour_zero[[t]][which(strsplit(as.character(strsplit(Allfiles_12_list_hour_zero[[t]][,3],"B")),"A")==as.character(pressure_vector[p])),5],1,4),"B")),"A"))[[1]][1])    }
    }
    
    
    
  }
  
  
  for(t in 1:length(Allfiles_12_list_hour_one)){
    
    for(p in 1:length(pressure_vector)){
      #will need to change p to p+15*number of prev stations for stations other than station 1
      
      if(!identical(Allfiles_12_list_hour_one[[t]][which(strsplit(as.character(strsplit(Allfiles_12_list_hour_one[[t]][,3],"B")),"A")==as.character(pressure_vector[p])),3], character(0))){
        All_data_matrix[p+165,2*(-as.numeric(as.Date("5/29/1954", "%m/%d/%Y"))+as.numeric(as.Date(paste(Allfiles_12_list_hour_one[[t]][1,2],Allfiles_12_list_hour_one[[t]][1,3],Allfiles_12_list_hour_one[[t]][1,4]),"%Y %m %d")))]=0.1*as.numeric((strsplit(as.character(strsplit(substr(Allfiles_12_list_hour_one[[t]][which(strsplit(as.character(strsplit(Allfiles_12_list_hour_one[[t]][,3],"B")),"A")==as.character(pressure_vector[p])),5],1,4),"B")),"A"))[[1]][1])
      }
    }
    
    
    
  }
  
  
  
  
  plot(All_data_matrix[14,])
  All_data_matrix[14,15000:16000]
  
  
  #NEED TO CLEAR 99.9s and 88.8s... 
  #######################################################################################################
  #######################################################################################################
  #######################################################################################################
  
  
  
  for(t in 1:length(Allfiles_13_list_hour_zero)){
    
    for(p in 1:length(pressure_vector)){
      #will need to change p to p+15*number of prev stations for stations other than station 1
      
      if(!identical(Allfiles_13_list_hour_zero[[t]][which(strsplit(as.character(strsplit(Allfiles_13_list_hour_zero[[t]][,3],"B")),"A")==as.character(pressure_vector[p])),3], character(0))){
        All_data_matrix[p+180,2*(-as.numeric(as.Date("5/29/1954", "%m/%d/%Y"))+as.numeric(as.Date(paste(Allfiles_13_list_hour_zero[[t]][1,2],Allfiles_13_list_hour_zero[[t]][1,3],Allfiles_13_list_hour_zero[[t]][1,4]),"%Y %m %d")))-1]=0.1*as.numeric((strsplit(as.character(strsplit(substr(Allfiles_13_list_hour_zero[[t]][which(strsplit(as.character(strsplit(Allfiles_13_list_hour_zero[[t]][,3],"B")),"A")==as.character(pressure_vector[p])),5],1,4),"B")),"A"))[[1]][1])    }
    }
    
    
    
  }
  
  
  for(t in 1:length(Allfiles_13_list_hour_one)){
    
    for(p in 1:length(pressure_vector)){
      #will need to change p to p+15*number of prev stations for stations other than station 1
      
      if(!identical(Allfiles_13_list_hour_one[[t]][which(strsplit(as.character(strsplit(Allfiles_13_list_hour_one[[t]][,3],"B")),"A")==as.character(pressure_vector[p])),3], character(0))){
        All_data_matrix[p+180,2*(-as.numeric(as.Date("5/29/1954", "%m/%d/%Y"))+as.numeric(as.Date(paste(Allfiles_13_list_hour_one[[t]][1,2],Allfiles_13_list_hour_one[[t]][1,3],Allfiles_13_list_hour_one[[t]][1,4]),"%Y %m %d")))]=0.1*as.numeric((strsplit(as.character(strsplit(substr(Allfiles_13_list_hour_one[[t]][which(strsplit(as.character(strsplit(Allfiles_13_list_hour_one[[t]][,3],"B")),"A")==as.character(pressure_vector[p])),5],1,4),"B")),"A"))[[1]][1])
      }
    }
    
    
    
  }
  
  
  
  
  plot(All_data_matrix[14,])
  All_data_matrix[14,15000:16000]
  
  
  #NEED TO CLEAR 99.9s and 88.8s... 
  #######################################################################################################
  #######################################################################################################
  #######################################################################################################
  
  
  
  for(t in 1:length(Allfiles_14_list_hour_zero)){
    
    for(p in 1:length(pressure_vector)){
      #will need to change p to p+15*number of prev stations for stations other than station 1
      
      if(!identical(Allfiles_14_list_hour_zero[[t]][which(strsplit(as.character(strsplit(Allfiles_14_list_hour_zero[[t]][,3],"B")),"A")==as.character(pressure_vector[p])),3], character(0))){
        All_data_matrix[p+195,2*(-as.numeric(as.Date("5/29/1954", "%m/%d/%Y"))+as.numeric(as.Date(paste(Allfiles_14_list_hour_zero[[t]][1,2],Allfiles_14_list_hour_zero[[t]][1,3],Allfiles_14_list_hour_zero[[t]][1,4]),"%Y %m %d")))-1]=0.1*as.numeric((strsplit(as.character(strsplit(substr(Allfiles_14_list_hour_zero[[t]][which(strsplit(as.character(strsplit(Allfiles_14_list_hour_zero[[t]][,3],"B")),"A")==as.character(pressure_vector[p])),5],1,4),"B")),"A"))[[1]][1])    }
    }
    
    
    
  }
  
  
  for(t in 1:length(Allfiles_14_list_hour_one)){
    
    for(p in 1:length(pressure_vector)){
      #will need to change p to p+15*number of prev stations for stations other than station 1
      
      if(!identical(Allfiles_14_list_hour_one[[t]][which(strsplit(as.character(strsplit(Allfiles_14_list_hour_one[[t]][,3],"B")),"A")==as.character(pressure_vector[p])),3], character(0))){
        All_data_matrix[p+195,2*(-as.numeric(as.Date("5/29/1954", "%m/%d/%Y"))+as.numeric(as.Date(paste(Allfiles_14_list_hour_one[[t]][1,2],Allfiles_14_list_hour_one[[t]][1,3],Allfiles_14_list_hour_one[[t]][1,4]),"%Y %m %d")))]=0.1*as.numeric((strsplit(as.character(strsplit(substr(Allfiles_14_list_hour_one[[t]][which(strsplit(as.character(strsplit(Allfiles_14_list_hour_one[[t]][,3],"B")),"A")==as.character(pressure_vector[p])),5],1,4),"B")),"A"))[[1]][1])
      }
    }
    
    
    
  }
  
  plot(All_data_matrix[100,])
  All_data_matrix[14,15000:16000]
  
  
  #NEED TO CLEAR 99.9s and 88.8s... 
  #######################################################################################################
  #######################################################################################################
  #######################################################################################################
  All_data_matrix[All_data_matrix==(-88.88)]=NA
  All_data_matrix[All_data_matrix==(-99.99)]=NA
  
  
  All_data_matrix[All_data_matrix==(-88.8)]=NA
  All_data_matrix[All_data_matrix==(-99.9)]=NA
  
  
  All_data_matrix[is.na(All_data_matrix)]=0
  
  options(digits=5)
  par(mfrow=c(3,5))
  for(i in 1:(14*15)){
    
    plot(All_data_matrix[i,],main=i,ylim=c(-100,0))
  
  }
  
  
  ALLfilenames
   
  All_data_matrix[is.na(All_data_matrix)]=0
  View(cor(t(All_data_matrix))>0.05)
  View(cor(t(All_data_matrix))>0.1)
  
  All_data_matrix[All_data_matrix==0]=NA
  