pacman::p_load(lubridate)
setwd("~/SWATFARM/")
#1-you have to initialize with ArcSWAT/QSWAT
#2-you run the mangment routine
#3-calibrate flow and nutrients for SWAT 
#4-comparison between base scenario and new scenario
#5-we cann do chance optimaztion on calibration routine
#####note that we want to have several HRUs with several barns
##imagine that you have n barn
## for example 3 different hru wth 3 different coe
###bring all thoses hru and fert.dat set up here
#####plot all the mass balance
setwd("TxtInOutLOC_stanFERT/")
######################################################
###getting the Date from file.cio
######################################################################
pathtofile="."
cfilename=paste0(pathtofile,"/file.cio")
SWATnbyr = read.fortran(textConnection(readLines(cfilename)[8]), "f20")[1,]
SWATiyr = read.fortran(textConnection(readLines(cfilename)[9]), "f20")[1,]
SWATidaf = read.fortran(textConnection(readLines(cfilename)[10]), "f20")[1,]
SWATidal = read.fortran(textConnection(readLines(cfilename)[11]), "f20")[1,]
startdate=as_date(paste0(SWATiyr,"-01-01")) + SWATidaf -1
enddate=as_date(paste0(SWATiyr+SWATnbyr -1,"-01-01")) + SWATidal -1
AllDays=data.frame(date=seq(startdate, by = "day", length.out = enddate-startdate+1))
Date=as.array(AllDays$date)

#########################
##dairy input
#################
#----------------------------------Mortality rate-------------------------------
kMortality_calf=0.07  #rate of cows lost to disease, etc.,anually = calves 
kMortality_yearling=0.03 #rate of cows lost to disease, etc.,anually = yearling 
kMortality_bredHeifer=0.03 ##rate of cows lost to disease, etc.,anually = bred heifer
kMortality_LH=0.04 ##rate of cows lost to disease, etc.,anually = lactating heifer
kMortality_BLH=0.04 ##rate of cows lost to disease, etc.,anually = bred lactating heifer
kMortality_DBH=0.04 ##rate of cows lost to disease, etc.,anually = dry bred heifer
kMortality_SecondLC=0.05 #rate of cows lost to disease, etc.,anually = 2nd lactating cows
kMortality_BredSecLC=0.05 ##rate of cows lost to disease, etc.,anually = bred 2nd lactating cows
kMortality_DSecondLC=0.05 #rate of cows lost to disease, etc.,anually = dry 2nd lac cows
kMortality_ThirdLC=0.06 # 3rd lactation cows
kMortality_BredThirdLC=0.06 ##rate of cows lost to disease, etc.,anually = bred 3rd lactation cows
kMortality_DThirdLC=0.06 #rate of cows lost to disease, etc.,anually = dry 3rd lactation cows
kMortality_LC=0.07 #rate of cows lost to disease, etc.,anually = lactating cows
kMotality_BredLC=0.07 #rate of cows lost to disease, etc.,anually = brd lactating cows
kMortality_DC=0.08 #rate of cows lost to disease, etc.,anually = dry cows 
#----------------------------(1-culling rate)= conception rate------------------
HFCR=0.45 #heifer 1st conception rate
HSCR=0.75 #heifer 2nd conception rate 
C2CR= 0.60 # COW 2nd lactation conception rate 
C3CR = 0.50 # COW 3rd lactation conception rate
C4CR= 0.40  ##ASK ROBIN
#-------------------------- fraction for born equation---------------
PropF= 0.50 #Proportion of female calves
PropKeep = 0.70 # Born calves retained for the lactating herd
#-------------------------initial number of animal in each pool----------------------
iCalf = Animal_df$calves[6500] ###init # of calf
iHeifer_first_lact = Animal_df$LactatingHeifers[6500]  # init # of first lactation cows
iHeifer_second_lact = Animal_df$SecondLC[6500] # init # of second lactation cows
iHeifer_third_lact =Animal_df$ThirdLC[6500] # init # of third lactation cows
iHeifer_first_dry = Animal_df$DBH[6500] # init # of cows dry after first lactation 
iHeifer_second_dry = Animal_df$DSecondLC[6500]# init # of cows dry after sec lactation
iHeifer_third_dry =Animal_df$DThirdLC[6500]# init # of number of third lactation dry
iLact = Animal_df$LC[6500] # init # of number of fourth lactation or greater, cows
iDry =Animal_df$DC[6500] # init # of cow number fourth lactation or greater, dry cows
iyearling=Animal_df$yearling[6500] # init # of yearling heifers
ibredHeifer=Animal_df$bredHeifer[6500] # init # of bred heifer
iBLH=Animal_df$BLH[6500] # init # of bred lactating heifers
iBredSecLC=Animal_df$BredSecLC[6500] # init # of second lactation cows
iBredThirdLC=Animal_df$BredThirdLC[6500] # init # of third lactation bred 3rd lactation cows
iBredLC=Animal_df$BredLC[6500] # init # of bred lactation cows

####note that we assumed that lactation cows are kept in the barn and calf+dry cows are in the pasture
##################################################
iCalf = 50 ###init # of calf
iHeifer_first_lact =50  # init # of first lactation cows
iHeifer_second_lact = 50 # init # of second lactation cows
iHeifer_third_lact =50 # init # of third lactation cows
iHeifer_first_dry = 50 # init # of cows dry after first lactation 
iHeifer_second_dry = 50 # init # of cows dry after sec lactation
iHeifer_third_dry =50 # init # of number of third lactation dry
iLact = 50 # init # of number of fourth lactation or greater, cows
iDry =50 # init # of cow number fourth lactation or greater, dry cows
iyearling=50 # init # of yearling heifers
ibredHeifer=50 # init # of bred heifer
iBLH=50 # init # of bred lactating heifers
iBredSecLC=50 # init # of second lactation cows
iBredThirdLC=50 # init # of third lactation bred 3rd lactation cows
iBredLC=50 # init # of bred lactation cows
###################################################
#iBarn=iHeifer_first_lact + iHeifer_second_lact + iHeifer_third_lact + iLact # init # of animals in the barn
#iPasture=iCalf + iHeifer_first_dry + iHeifer_second_dry + iHeifer_third_dry + iDry #init # of animals in the pasture
###################################################################
Animal_df= data.frame(matrix(ncol=60, nrow=nrow(Date)))
x=c("Date","calves_born","calves_to_yearling","calves_death","calves",
    "yearling_to_bredHeifers","yearling_death","yearling","bredHeifer_death",
    "bredHeifer_to_LactHeif","bredHeifer","LH_to_BLH","lactatingHeifers_death","DB1HC",
    "LactatingHeifers","BLH_to_DBH","BLH_death","BLH","DBH_to_SecondLC",
    "DBH_death","DBH","SecondLC_death","SecondLC_to_BredSecLC",
    "SecondLC","DBHC","BredSecLC_death","BredSecLC_to_DSecondLC","BredSecLC",
    "DSecondLC_death","DSecondLC_to_ThirdLC","DSecondLC","ThirdLC_death","ThirdLC_to_BredThirdLC",
    "ThirdLC","DB2LCC","BredThirdLC_death","BredThirdLC_to_DThirdLC",
    "BredThirdLC","DThirdLC_death","DThirdLC_to_LC","DThirdLC","LC_death",
    "LC_to_BredLC","DC_to_LC","LC","DB3LCC",
    "BredLC_death","BredLC_to_DC","BredLC","DC_death",
    "DC","BCC","total_number","yearling_cull","LactHeif_cull","SecondLC_cull",
    "ThirdLC_cull","LC_cull","excessive_cull","total_barn")
colnames(Animal_df)=x
#------------------------------------ initial numbers---------------------------------------------
Animal_df$Date=Date
Animal_df$calves[1]=iCalf #calves
Animal_df$yearling[1]=iyearling #yearling
Animal_df$bredHeifer[1]=ibredHeifer # bred heifer
Animal_df$LactatingHeifers[1]=iHeifer_first_lact # first lactating heifers
Animal_df$BLH[1]=iBLH # 1st bred lactating heifer 
Animal_df$DBH[1]=iHeifer_first_dry 
Animal_df$SecondLC[1]=iHeifer_second_lact
Animal_df$BredSecLC[1]=iBredSecLC
Animal_df$DSecondLC[1]=iHeifer_second_dry
Animal_df$ThirdLC[1]=iHeifer_third_lact
Animal_df$BredThirdLC[1]=iBredThirdLC
Animal_df$DThirdLC[1]=iHeifer_third_dry
Animal_df$LC[1]=iLact
Animal_df$BredLC[1]=iBredLC
Animal_df$DC[1]=iDry
Animal_df$excessive_cull=0
#total number of animal
Animal_df$total_number=NA
Animal_df$total_barn=NA
#Animal_df$calves_born[1]=NA #assume that calves born in day 1  is 0



#Animal_df$BredLC_to_DC[1]=(Animal_df$BredLC[1])/220 #assumption: bread lactating cows to dry cows initial number
#Animal_df$DC_to_LC[1]=(Animal_df$DC[1])/60 ## assumption:dry cow to lactating cows initial numbers
#Animal_df$LC_to_BredLC[1]=(Animal_df$LC[1])*C4CR/85 ##assumption : Lactating cows to bred lactating cows initial number


#-----------------------------capacity of the farm----------------------------------------------------
Barn_capacity=310 ##capacity of the barn, assume that calves, yearling, 1st lact, 2nd lact, 3rd lact, lc are gept in the barn
#-----------------------------------loop to calculate daily population of each pool-----------------------------

for (i in (1:(nrow(Date)))){
  #i=3
  Animal_df$total_number[i]=Animal_df$calves[i]+Animal_df$yearling[i]+Animal_df$bredHeifer[i]+
    Animal_df$LactatingHeifers[i]+Animal_df$BLH[i]+Animal_df$DBH[i]+Animal_df$SecondLC[i]+
    Animal_df$BredSecLC[i]+Animal_df$DSecondLC[i]+Animal_df$ThirdLC[i]+Animal_df$BredThirdLC[i]+
    Animal_df$DThirdLC[i]+Animal_df$LC[i]+Animal_df$BredLC[i]+Animal_df$DC[i]
  Animal_df$total_barn[i]=Animal_df$calves[i]+Animal_df$yearling[i]+Animal_df$LactatingHeifers[i]+Animal_df$SecondLC[i]+Animal_df$ThirdLC[i]+Animal_df$LC[i]
  
  if (Animal_df$total_barn[i]>Barn_capacity){
    Animal_df$excessive_cull[i]=(Animal_df$total_barn[i]-Barn_capacity)/6 # the excessive will be devided between yearling, lactating heifers,
    #2nd lactation cows, 3rd lac cows,lactating cows
    #Animal_df$total_number[i]=target_population
    
  }
  
  ##-----------calves pool =nCalf-----------------------------------------------------
  #i=1
  Animal_df$calves_death[i]=Animal_df$calves[i]*kMortality_calf/365  ## calves death loss daily
  Animal_df$calves_to_yearling[i]=Animal_df$calves[i]/365 ##calves to yearling daily
  
  ##-----------yearling pool--------------------------------------------
  Animal_df$yearling_death[i]=Animal_df$yearling[i]*kMortality_yearling/365  ##  yearling death loss
  Animal_df$yearling_cull[i]=Animal_df$yearling[i]*(1-HFCR)/365  ## yearling culled
  Animal_df$yearling_to_bredHeifers[i]=(Animal_df$yearling[i]/90) ##yearling to bred heifer
  ##-----------Bred Heifer-----------------------------------------------------------------------
  Animal_df$bredHeifer_death[i]=Animal_df$bredHeifer[i]*kMortality_bredHeifer/365 ##bread heifer 
  Animal_df$bredHeifer_to_LactHeif[i]=Animal_df$bredHeifer[i]/285
  ##DB1HC#######
  if (Animal_df$bredHeifer_to_LactHeif[i]<0){
    Animal_df$bredHeifer_to_LactHeif[i]==0
  }
  Animal_df$DB1HC[i]=Animal_df$bredHeifer_to_LactHeif[i]
  
  ##-----------Lactating Heifers = nHeifer_first_lact----------------------------------------
  Animal_df$LactHeif_cull[i]=Animal_df$LactatingHeifers[i]*(1-HSCR)/365 #lactating heifer to unbred lactating heifer
  Animal_df$lactatingHeifers_death[i]=Animal_df$LactatingHeifers[i]*kMortality_LH/365  #DL lactating heifers
  Animal_df$LH_to_BLH[i]=Animal_df$LactatingHeifers[i]/85 ##lactating Heifers to bred lactation heifers
  ##-----------Bred lactating heifers----------------------------------------------------
  Animal_df$BLH_death[i]=Animal_df$BLH[i]*kMortality_BLH/365 # bred lactating heifers death loss
  Animal_df$BLH_to_DBH[i]=Animal_df$BLH[i]/220 ## bred lactating heifer to dry bred heifers
  ##------------Dry bred heifers = nHeifer_first_dry----------------------------------
  Animal_df$DBH_death[i]=Animal_df$DBH[i]*kMortality_DBH/365 ## dry bred heifer death loss
  Animal_df$DBH_to_SecondLC[i]=Animal_df$DBH[i]/60 #dry bred heifer to 2nd lactation cows
  ##----------- DBHC: Population of Dry, Bred Heifer Cows-------------------------------
  if (Animal_df$DBH_to_SecondLC[i]< 0){
    Animal_df$DBH_to_SecondLC[i]=0
  }
  Animal_df$DBHC[i]= Animal_df$DBH_to_SecondLC[i]
  ##-----------Second lactation cows= nHeifer_second_lact--------------------------------
  Animal_df$SecondLC_death[i]=Animal_df$SecondLC[i]*kMortality_SecondLC/365 # 2nd lactating cows death loss
  Animal_df$SecondLC_cull[i]=Animal_df$SecondLC[i]*(1-C2CR)/365 ##2nd lactation cows cull
  Animal_df$SecondLC_to_BredSecLC[i]=(Animal_df$SecondLC[i]/85) ## 2nd lactating cows to bred 2nd lactating cows
  
  ### Population of Dry, Bred Heifer Cows
  ##--------------- Bred 2nd lactating cows-----------------------
  Animal_df$BredSecLC_death[i]=Animal_df$BredSecLC[i]*kMortality_BredSecLC/365 # Bred 2nd lactating cows death loss
  Animal_df$BredSecLC_to_DSecondLC[i]=Animal_df$BredSecLC[i]/220  # bred 2nd lactating cows to dry 2nd loc cows 
  ##--------------- Dry 2nd lactating cows = nHeifer_second_dry-------------------
  Animal_df$DSecondLC_death[i]=Animal_df$DSecondLC[i]*kMortality_DSecondLC/365 ##dry 2nd lac cows death loss
  Animal_df$DSecondLC_to_ThirdLC[i]=Animal_df$DSecondLC[i]/60 # dry 2nd lac cows to 3rd lactation cows
  ##-----------------DB2LCC: Population of Dry, second lactation cows--------------------------
  if (Animal_df$DSecondLC_to_ThirdLC[i]<0){
    Animal_df$DSecondLC_to_ThirdLC[i]=0  
  }
  Animal_df$DB2LCC[i]=Animal_df$DSecondLC_to_ThirdLC[i] #DB2LCC
  ##--------------- 3rd lactation cows = nHeifer_third_lact--------------------------------
  Animal_df$ThirdLC_death[i]=Animal_df$ThirdLC[i]*kMortality_ThirdLC/365 # 3rd lactation cows death loss
  Animal_df$ThirdLC_cull[i]=Animal_df$ThirdLC[i]*(1-C3CR)/365 # 3rd lactation cows cull
  Animal_df$ThirdLC_to_BredThirdLC[i]=(Animal_df$ThirdLC[i]/85) # 3rd lactation cows to bred 3rd lactation cows 
  ##----------------Bred 3rd lactation cows-----------------------------------------------------
  Animal_df$BredThirdLC_death[i]=Animal_df$BredThirdLC[i]*kMortality_BredThirdLC/365 #Bred 3rd lactation cows death loss
  Animal_df$BredThirdLC_to_DThirdLC[i]=Animal_df$BredThirdLC[i]/220 #Bred 3rd lactation cows to dry 2rd lac cows
  ##----------------Dry 3rd lactation cows = nHeifer_third_dry---------------------------------
  Animal_df$DThirdLC_death[i]=Animal_df$DThirdLC[i]*kMortality_DThirdLC/365 ##Dry 3rd lactation cows death loss
  Animal_df$DThirdLC_to_LC[i]=Animal_df$DThirdLC[i]/60 ##Dry 3rd lactation cows to lactating cows
  ##-------------- DB3LCC Population of Dry, third lactation cows-------------------
  if (Animal_df$DThirdLC_to_LC[i]<0){
    Animal_df$DThirdLC_to_LC[i]=0
  }
  Animal_df$DB3LCC[i]=Animal_df$DThirdLC_to_LC[i] #DB3LCC
  ###################### lactating cows
  Animal_df$LC_death[i]=Animal_df$LC[i]*kMortality_LC/365 #Lactating cows death loss
  Animal_df$LC_cull[i]=Animal_df$LC[i]*(1-C4CR)/365
  Animal_df$LC_to_BredLC[i]=(Animal_df$LC[i]/85)##Lactating cows to bred lactating cows
  #######################bred LC
  Animal_df$BredLC_death[i]=Animal_df$BredLC[i]*kMotality_BredLC/365 #Bred lactating cows death loss
  Animal_df$BredLC_to_DC[i]=Animal_df$BredLC[i]/220 
  #######################Dry cows
  Animal_df$DC_death[i]=Animal_df$DC[i]*kMortality_DC/365 ###Dry cows death loss
  Animal_df$DC_to_LC[i]=Animal_df$DC[i]/60 ## dry cow to lactating cows
  ##----------------------BCC:Population of Dry, fourth or greater lactation cows ------------------------------------
  if(Animal_df$DC_to_LC[i]<0){
    Animal_df$DC_to_LC[i]=0
  }
  Animal_df$BCC[i]=Animal_df$DC_to_LC[i]
  #--------------------- calves born-------------------------------------------------------------
  Animal_df$calves_born[i]=(Animal_df$DB1HC[i]+Animal_df$DBHC[i]+Animal_df$DB2LCC[i]+Animal_df$DB3LCC[i]+Animal_df$BCC[i])*PropF*PropKeep#CALVES BORN
  if (Animal_df$calves_born[i]<0){
    Animal_df$calves_born[i]=0
  }
  ##---------loop for population 
  if (i+1 <= nrow(Date)){
    Animal_df$calves[i+1]=Animal_df$calves[i]+Animal_df$calves_born[i]-Animal_df$calves_death[i]-Animal_df$calves_to_yearling[i]-Animal_df$excessive_cull[i]##calf pool
    if (Animal_df$calves[i+1]<0){
      Animal_df$calves[i+1]=0
    }
    Animal_df$yearling[i+1]=Animal_df$yearling[i]+Animal_df$calves_to_yearling[i]-Animal_df$yearling_death[i]-Animal_df$yearling_to_bredHeifers[i]-Animal_df$yearling_cull[i]-Animal_df$excessive_cull[i]#yearling pool
    if(Animal_df$yearling[i+1]<0){
      Animal_df$yearling[i+1]=0
    }
    Animal_df$bredHeifer[i+1]=Animal_df$bredHeifer[i]+Animal_df$yearling_to_bredHeifers[i]-Animal_df$bredHeifer_death[i]-Animal_df$bredHeifer_to_LactHeif[i] #bred heifer pool
    if (Animal_df$bredHeifer[i+1]<0){
      Animal_df$bredHeifer[i+1]=0
    }
    Animal_df$LactatingHeifers[i+1]=Animal_df$LactatingHeifers[i]+Animal_df$bredHeifer_to_LactHeif[i]-Animal_df$LH_to_BLH[i]-Animal_df$lactatingHeifers_death[i]-Animal_df$LactHeif_cull[i]-Animal_df$excessive_cull[i] ##lactating heifer pool
    if (Animal_df$LactatingHeifers[i+1]<0){
      Animal_df$LactatingHeifers[i+1]=0
    }
    Animal_df$BLH[i+1]=Animal_df$BLH[i]+Animal_df$LH_to_BLH[i]-Animal_df$BLH_to_DBH[i]-Animal_df$BLH_death[i] #bred lactating heifers
    if (Animal_df$BLH[i+1]<0){
      Animal_df$BLH[i+1]=0
    }
    Animal_df$DBH[i+1]=Animal_df$DBH[i]+Animal_df$BLH_to_DBH[i]-Animal_df$DBH_to_SecondLC[i]-Animal_df$DBH_death[i] #dry bred heifer 
    if (Animal_df$DBH[i+1]<0){
      Animal_df$DBH[i+1]=0
    }
    Animal_df$SecondLC[i+1]=Animal_df$SecondLC[i]+Animal_df$DBH_to_SecondLC[i]-Animal_df$SecondLC_death[i]-Animal_df$SecondLC_to_BredSecLC[i]-Animal_df$SecondLC_cull[i]-Animal_df$excessive_cull[i]  ##2nd lactation cows
    if (Animal_df$SecondLC[i+1]<0){
      Animal_df$SecondLC[i+1]=0
    }
    Animal_df$BredSecLC[i+1]=Animal_df$BredSecLC[i]+Animal_df$SecondLC_to_BredSecLC[i]-Animal_df$BredSecLC_death[i]-Animal_df$BredSecLC_to_DSecondLC[i] # bred 2nd lactating cows
    if (Animal_df$BredSecLC[i+1]<0){
      Animal_df$BredSecLC[i+1]=0
    }
    Animal_df$DSecondLC[i+1]=Animal_df$DSecondLC[i]+Animal_df$BredSecLC_to_DSecondLC[i]-Animal_df$DSecondLC_death[i]-Animal_df$DSecondLC_to_ThirdLC[i] #Dry 2nd lac cows
    if (Animal_df$DSecondLC[i+1]<0){
      Animal_df$DSecondLC[i+1]=0
    }
    Animal_df$ThirdLC[i+1]=Animal_df$ThirdLC[i]+Animal_df$DSecondLC_to_ThirdLC[i]-Animal_df$ThirdLC_death[i]-Animal_df$ThirdLC_to_BredThirdLC[i]-Animal_df$ThirdLC_cull[i]-Animal_df$excessive_cull[i]#3rd lactation cows
    if (Animal_df$ThirdLC[i+1]<0){
      Animal_df$ThirdLC[i+1]=0
    }
    Animal_df$BredThirdLC[i+1]=Animal_df$BredThirdLC[i]+Animal_df$ThirdLC_to_BredThirdLC[i]-Animal_df$BredThirdLC_death[i]-Animal_df$BredThirdLC_to_DThirdLC[i] #bred 3rd lac cows 
    if(Animal_df$BredThirdLC[i+1]<0){
      Animal_df$BredThirdLC[i+1]=0
    }
    Animal_df$DThirdLC[i+1]=Animal_df$DThirdLC[i]+Animal_df$BredThirdLC_to_DThirdLC[i]-Animal_df$DThirdLC_to_LC[i]-Animal_df$DThirdLC_death[i] #dry 3rd lac cows
    if (Animal_df$DThirdLC[i+1]<0){
      Animal_df$DThirdLC[i+1]=0
    }
    Animal_df$LC[i+1]=Animal_df$LC[i]+Animal_df$DThirdLC_to_LC[i]-Animal_df$LC_to_BredLC[i]-Animal_df$LC_death[i]+Animal_df$DC_to_LC[i]-Animal_df$LC_cull[i]-Animal_df$excessive_cull[i] ##lactating cows
    if (Animal_df$LC[i+1]<0){
      Animal_df$LC[i+1]=0
    }
    Animal_df$BredLC[i+1]=Animal_df$BredLC[i]+Animal_df$LC_to_BredLC[i]-Animal_df$BredLC_death[i]-Animal_df$BredLC_to_DC[i] # bred lactating cows
    if (Animal_df$BredLC[i+1]<0){
      Animal_df$BredLC[i+1]=0
    }
    Animal_df$DC[i+1]=Animal_df$DC[i]+Animal_df$BredLC_to_DC[i]-Animal_df$DC_death[i]-Animal_df$DC_to_LC[i] #dry cows
    if (Animal_df$DC[i+1]<0){
      Animal_df$DC[i+1]=0
    }
    
  }
}
#par(mfrow=c(3,3))
#plot(Animal_df$LC)
#plot(Animal_df$DThirdLC_to_LC)
#plot(Animal_df$LC_to_BredLC)
#plot(Animal_df$LC_death)
#plot(Animal_df$DC_to_LC)
#plot( Animal_df$excessive_cull)
#plot( Animal_df$LC_cull) ##lactating cows







#Animal_df=Animal_df[1:365,]

pacman::p_load(ggplot2,patchwork)
p1 <- ggplot(data=Animal_df,aes(x=Date, y=calves)) +
  ylim(0,(max(Animal_df$calves)+1))+
  geom_line(color="red", size=2) +
  ggtitle("Number of Calf")
p2<-ggplot(Animal_df, aes(x=Date, y=LactatingHeifers)) +
  geom_line(color="black", size=2) +
  ylim(0,(max(Animal_df$LactatingHeifers)+1))+
  ggtitle("Number heifer first lactation")
p3 <-ggplot(Animal_df, aes(x=Date, y=DBH)) +
  geom_line(color="blue", size=2) +
  ylim(0,(max(Animal_df$DBH)+1))+
  ggtitle("Number heifer first dry")
p4 <-ggplot(Animal_df, aes(x=Date, y=SecondLC)) +
  geom_line(color="green", size=2) +
  ylim(0,(max(Animal_df$SecondLC)+1))+
  ggtitle("Number heifer second lactation")
p5 <- ggplot(Animal_df, aes(x=Date, y=DSecondLC)) +
  geom_line(color="orange", size=2) +
  ylim(0,(max(Animal_df$DSecondLC)+1))+
  ggtitle("Number heifer second dry")
p6 <- ggplot(Animal_df, aes(x=Date, y=ThirdLC)) +
  geom_line(color="purple", size=2) +
  ylim(0,(max(Animal_df$ThirdLC)+1))+
  ggtitle("Number heifer third lactation")
p7 <- ggplot(Animal_df, aes(x=Date, y=DThirdLC)) +
  geom_line(color="light blue", size=2) +
  ylim(0,(max(Animal_df$DThirdLC)+1))+
  ggtitle("Number heifer third dry")
p8 <- ggplot(Animal_df, aes(x=Date, y=LC)) +
  geom_line(color="pink", size=2) +
  ylim(0,(max(Animal_df$LC)+1))+
  ggtitle("Number of Lact")
p9 <- ggplot(Animal_df, aes(x=Date, y=DC)) +
  geom_line(color="yellow", size=2) +
  ylim(0,(max(Animal_df$DC)+1))+
  ggtitle("Number of Dry")
p10 <-  ggplot(Animal_df, aes(x=Date, y=total_number)) +
  geom_line(color="brown", size=2) +
  ylim(0,(max(Animal_df$total_number)+1))+
  ggtitle("total number")
p11 <-  ggplot(Animal_df, aes(x=Date, y=yearling)) +
  ylim(0,(max(Animal_df$yearling)+1))+
  geom_line(color="black", size=2) +
  ggtitle("yearling")

p12 <-  ggplot(Animal_df, aes(x=Date, y=bredHeifer)) +
  geom_line(color="black", size=2) +
  ylim(0,(max(Animal_df$bredHeifer)+1))+
  ggtitle("bredHeifer")

p13 <-  ggplot(Animal_df, aes(x=Date, y=BLH)) +
  geom_line(color="black", size=2) +
  ylim(0,(max(Animal_df$BLH)+1))+
  ggtitle("Bred lactating heifer")

p14 <-  ggplot(Animal_df, aes(x=Date, y=BredSecLC)) +
  geom_line(color="black", size=2) +
  ylim(0,(max(Animal_df$BredSecLC)+1))+
  ggtitle("Bred 2ndlactating cows")

p15 <-  ggplot(Animal_df, aes(x=Date, y=BredThirdLC)) +
  geom_line(color="black", size=2) +
  ylim(0,(max(Animal_df$BredThirdLC)+1))+
  ggtitle("Bred 3rdlactating cows")

p16 <-  ggplot(Animal_df, aes(x=Date, y=BredLC)) +
  geom_line(color="black", size=2) +
  ylim(0,(max(Animal_df$BredLC)+1))+
  ggtitle("Bred 4thlactating cows")

p1 + p2 + p3 + p4 + p5 + p6 + p7 + p8 + p9 + p10+ p11 +p12+p13+p14+p15+p16+ plot_layout(ncol = 4, widths = c(1, 1,1))
#####################################################################
p1 + p11 + p12 + p2 + p13 + p3 + p4 + p14 + p5 + p6 + p15 + p7 + p8 + p16 + p9 + p10 + plot_layout(ncol = 4, widths = c(1, 1,1))



(max(Animal_df$calves)-min(Animal_df$calves))*10^5
