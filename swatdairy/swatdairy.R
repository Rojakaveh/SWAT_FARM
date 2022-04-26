Sys.unsetenv("http_proxy"); Sys.unsetenv("https_proxy")
#########################################################################
###geting Date from SWAT initialization file.cio
###set directory to the swat initialization folder########################
setwd("~/SWATFARM/TXTINOUT_TRY/")
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

############################
#first we run swat model for standard one to get the area of each HRU
################################
setwd("~/SWATFARM/TXTINOUT_TRY/")
file.remove(list.files(pattern="output."))
runSWAT2012()
outdata_rch_STAN = readSWAT("rch",".")
#on.exit()
outdata_sub_STAN=readSWAT("sub",".")
###remember to check the columns merge two heders such as output in TxtInOutLOC
cfilename_hruoutA=paste0(pathtofile,"/output.hru")
cfilename_hruout=readLines(paste0(pathtofile,"/output.hru"))
textreplace=gsub(pattern = "WTAB CLIm WTAB SOLm", replace="WTABclimwtabsolm",x=cfilename_hruout)
writeLines(textreplace, con=cfilename_hruoutA)
outdata_hru_STAN=readSWAT("hru",".") 
####################################################################
#####here we need to generate the data table that have user info: nrows=number of HRUs , ncol= 13 ### MGT_DATAFRAME
#The info that user needs to fill out
#1-Barn_number =B01, B02, B03, B04,...B99 etc.
#2-HRU_number for the specified Barn = 000010111, 000010112, 000010113, 000010114 for B01; AND 000020121, 000020122, 000020123 for B02
###3-AREA_ha area of eaxh HRU from outdata_hru_STAN$AREAkm2[outdata_hru_STAN$GIS==i][1]*100 ) 
#4-year of fertilizer application----year_app
#5-month of fertilizer application---month_app
#6-day of fertilizer application---day_app
###7-Date of fertilizer application
#8-MGT-OP ##mgt op number, 3 for fert application
###9-FERT_NAME #this is fert name in fert.dat file ##BbbYYJJJ USE THis format for fert_name
#10-FRT_KG #the amount of fertilizer applied to HRU (kg/ha) for HRU
###11-fert_applied # area_hru*frt_kg (kg)
#12-FRT_SURFACE # 0.00
#13-FERT_ID #fertilizer ID in fert.dat file 
#################################################################
##how many lines are in fert.dat?
######################################################
pathtofile="."
linefertdat=readLines(paste0(pathtofile,"/fert.dat"))
row_fert_dat=length(linefertdat)
##########################
totalnumber_HRU=7 # this is the total number of hrus that we want to do mgt application and equals to MGT_DATAFRAME nrow 
Make_MGT_DATAFRAME= function(totalnumber_HRU){
  if(missing(totalnumber_HRU)){
    totalnumber_HRU=0
  }
  MGT_DATAFRAME=data.frame(
    Barn_number=character(),
    HRU_number=numeric(),
    #Area_ha=integer(),
    year_app=integer(),
    month_app=integer(),
    day_app=integer(),
    MGT_OP=numeric(),
    #FERT_NAME=character(),
    FRT_KG=integer()
    #FERT_APPLIED=numeric(),
    #FRT_SURFACE= numeric(),
    #FERT_ID=numeric()
  )
  MGT_DATAFRAME[1:totalnumber_HRU,] = NA 
  return(MGT_DATAFRAME)
}

mgt_datafram= Make_MGT_DATAFRAME(totalnumber_HRU)
################################################here we try to open the data fram in google sheet so that the user can modify it manually
###user just need to fill Barn_number, HRU_number,year of fertilizer application----year_app,
#####-month of fertilizer application---month_app, day of fertilizer application---day_app,-MGT-OP ##mgt op number, 3 for fert application,
####FRT_KG,FRT_SURFACE # 0.00

#pacman::p_load(googlesheets4)
gsheet = gs4_create(
  "mgt_dataframe",
  sheets=mgt_datafram
)
mgt_datafram=read_sheet(unclass(gsheet))
mgt_datafram$Area_ha=NA
mgt_datafram$Date_app=as.Date(NA)
mgt_datafram$FERT_NAME=NA
mgt_datafram$FRT_SURFACE=NA
mgt_datafram$FERT_ID=NA
#######################################################
for (i in 1:nrow(mgt_datafram)){
  mgt_datafram$Date_app[i]=as.Date(paste0(mgt_datafram$day_app[i]," ",month.name[mgt_datafram$month_app[i]],",",mgt_datafram$year_app[i]), format = "%d %B, %Y")
  mgt_datafram$Area_ha[i]=outdata_hru_STAN$AREAkm2[outdata_hru_STAN$GIS==mgt_datafram$HRU_number[i]][1]*100
  mgt_datafram$FERT_NAME[i]=paste0(mgt_datafram$Barn_number[i],mgt_datafram$year_app[i] %% 100,yday(mgt_datafram$Date_app[i]))
}
mgt_datafram$FERT_ID=seq(row_fert_dat+1,(row_fert_dat+nrow(mgt_datafram)),by=1)
mgt_datafram$FERT_APPLIED=mgt_datafram$Area_ha*mgt_datafram$FRT_KG
mgt_datafram$FRT_SURFACE=format(round(0.00, 2), nsmall = 2)


#############################################################################
####Now that we have all inputs for *.mgt files we will modify them for selected HRUs
#############################################################################
pathtofile="."
for (i in mgt_datafram$HRU_number){
  #i=10111
  b=formatC(i, width = 9, format = "d", flag = "0")
  assign(paste0("cfilename_mgtA",i,sep=""), paste0(pathtofile,"/",b,".mgt"))
  assign(paste0("cfilename_mgt",i,sep=""), readLines(paste0(pathtofile,"/",b,".mgt")))
  c=formatC(mgt_datafram$month_app[mgt_datafram$HRU_number==i], width = 2, format = "d", flag = "")
  d=formatC(mgt_datafram$day_app[mgt_datafram$HRU_number==i], width = 2, format = "d", flag = "")
  e=formatC(mgt_datafram$FERT_ID[mgt_datafram$HRU_number==i], width = 4, format = "d", flag = "")
  f=formatC(format(round(mgt_datafram$FRT_KG[mgt_datafram$HRU_number==i],5),nsmall=5), width = 12, format = "d", flag = "")
  g=formatC(mgt_datafram$FRT_SURFACE[mgt_datafram$HRU_number==i], width = 6, format = "d", flag = "")
  assign(paste0("mgt_fert",i,sep=""), paste0(" ",c," ",d,"           ",mgt_datafram$MGT_OP[mgt_datafram$HRU_number==i]," ",e,"        ",f," ",g))
  textreplace=gsub(pattern = readLines(get(paste0("cfilename_mgtA",i,sep="")))[31],replace=get(paste0("mgt_fert",i,sep="")),x=get(paste0("cfilename_mgt",i,sep=""))) 
  writeLines(textreplace, con=get(paste0("cfilename_mgtA",i,sep="")))
}
###########################################################
###source modified dairy_func from my GitHUB
#######################################################
source("https://raw.githubusercontent.com/Rojakaveh/SWAT_FARM/main/dairyfractionfunction/dairyfractionfunction") ##function
###########################################################################################
#--------getting farm info : same input as Dairy_df function
##########################getting farm info
Make_fertdairy_DATAFRAME= function(numberofbarn){
  if(missing(numberofbarn)){
    numberofbarn=0
  }
  fertdairy_DATAFRAME=data.frame(
    Barn_number=character(),
    kMortality_calf=numeric(),
    kMortality_yearling=numeric(),
    kMortality_bredHeifer=numeric(),
    kMortality_LH=numeric(),
    kMortality_BLH=numeric(),
    kMortality_DBH=numeric(),
    kMortality_SecondLC=numeric(),
    kMortality_BredSecLC=numeric(),
    kMortality_DSecondLC=numeric(),
    kMortality_ThirdLC=numeric(),
    kMortality_BredThirdLC=numeric(),
    kMortality_DThirdLC=numeric(),
    kMortality_LC=numeric(),
    kMotality_BredLC=numeric(),
    kMortality_DC=numeric(),
    HFCR=numeric(),
    HSCR=numeric(),
    C2CR= numeric(),
    C3CR = numeric(),
    C4CR= numeric(),
    PropF= numeric(),
    PropKeep = numeric(),
    iCalf=integer(),
    iHeifer_first_lact=integer(),
    iHeifer_second_lact=integer(),
    iHeifer_third_lact=integer(),
    iHeifer_first_dry=integer(),
    iHeifer_second_dry=integer(),
    iHeifer_third_dry=integer(),
    iLact=integer(),
    iDry=integer(),
    iyearling=integer(),
    ibredHeifer=integer(),
    iBLH=integer(),
    iBredSecLC=integer(),
    iBredThirdLC=integer(),
    iBredLC=integer(),
    Barn_capacity=integer(),
    Breed =integer(),
    FCM=integer(),
    calf_ME=numeric(),
    calf_CP=numeric(),
    fed_calf_P=numeric(),
    heifer_ME=numeric(),
    heifer_CP=numeric(),
    fed_heifer_P=numeric(),
    lact_CP=numeric(),
    fed_lact_P=numeric(),
    dry_CP=numeric(),
    fed_dry_P=numeric(),
    HRS=numeric(),
    Temp=numeric(),
    RHMD=numeric(),
    WS=numeric()
  )
  fertdairy_DATAFRAME[1:numberofbarn,] = NA 
  return(fertdairy_DATAFRAME)
}

fertdairy_datafram= Make_fertdairy_DATAFRAME(length(unique(mgt_datafram$Barn_number)))
################################################here we try to open the data fram in google sheet so that the user can modify it manually
##to enter each farm input data
#pacman::p_load(googlesheets4)
gsheet = gs4_create(
  "fertdairy_datafram",
  sheets=fertdairy_datafram
)
fertdairy_datafram=read_sheet(unclass(gsheet))
###----------------------------------------------------###############################
#for(j in unique(fertdairy_datafram$Barn_number)){
  #j="B01"
 # HRU_number=mgt_datafram$HRU_number[mgt_datafram$Barn_number==j]
  for (i in unique(mgt_datafram$HRU_number)){
    #i=10111
  j=mgt_datafram$Barn_number[mgt_datafram$HRU_number==i]
  Farm_df=Dairy_df(fertdairy_datafram$kMortality_calf[fertdairy_datafram$Barn_number==j],fertdairy_datafram$kMortality_yearling[fertdairy_datafram$Barn_number==j],fertdairy_datafram$kMortality_bredHeifer[fertdairy_datafram$Barn_number==j],
                   fertdairy_datafram$kMortality_LH[fertdairy_datafram$Barn_number==j],fertdairy_datafram$kMortality_BLH[fertdairy_datafram$Barn_number==j],fertdairy_datafram$kMortality_DBH[fertdairy_datafram$Barn_number==j],
                   fertdairy_datafram$kMortality_SecondLC[fertdairy_datafram$Barn_number==j],fertdairy_datafram$kMortality_BredSecLC[fertdairy_datafram$Barn_number==j],fertdairy_datafram$kMortality_DSecondLC[fertdairy_datafram$Barn_number==j],
                   fertdairy_datafram$kMortality_ThirdLC[fertdairy_datafram$Barn_number==j],fertdairy_datafram$kMortality_BredThirdLC[fertdairy_datafram$Barn_number==j],fertdairy_datafram$kMortality_DThirdLC[fertdairy_datafram$Barn_number==j],
                   fertdairy_datafram$kMortality_LC[fertdairy_datafram$Barn_number==j],fertdairy_datafram$kMotality_BredLC[fertdairy_datafram$Barn_number==j],fertdairy_datafram$kMortality_DC[fertdairy_datafram$Barn_number==j],
                   fertdairy_datafram$HFCR[fertdairy_datafram$Barn_number==j],fertdairy_datafram$HSCR[fertdairy_datafram$Barn_number==j],fertdairy_datafram$C2CR[fertdairy_datafram$Barn_number==j],fertdairy_datafram$C3CR[fertdairy_datafram$Barn_number==j],
                   fertdairy_datafram$C4CR[fertdairy_datafram$Barn_number==j],fertdairy_datafram$PropF[fertdairy_datafram$Barn_number==j],fertdairy_datafram$PropKeep[fertdairy_datafram$Barn_number==j],
                   fertdairy_datafram$iCalf[fertdairy_datafram$Barn_number==j],fertdairy_datafram$iHeifer_first_lact[fertdairy_datafram$Barn_number==j],fertdairy_datafram$iHeifer_second_lact[fertdairy_datafram$Barn_number==j],
                   fertdairy_datafram$iHeifer_third_lact[fertdairy_datafram$Barn_number==j],fertdairy_datafram$iHeifer_first_dry[fertdairy_datafram$Barn_number==j],fertdairy_datafram$iHeifer_second_dry[fertdairy_datafram$Barn_number==j],
                   fertdairy_datafram$iHeifer_third_dry[fertdairy_datafram$Barn_number==j],fertdairy_datafram$iLact[fertdairy_datafram$Barn_number==j],fertdairy_datafram$iDry[fertdairy_datafram$Barn_number==j],
                   fertdairy_datafram$iyearling[fertdairy_datafram$Barn_number==j],fertdairy_datafram$ibredHeifer[fertdairy_datafram$Barn_number==j],fertdairy_datafram$iBLH[fertdairy_datafram$Barn_number==j],
                   fertdairy_datafram$iBredSecLC[fertdairy_datafram$Barn_number==j],fertdairy_datafram$iBredThirdLC[fertdairy_datafram$Barn_number==j],fertdairy_datafram$iBredLC[fertdairy_datafram$Barn_number==j],
                   fertdairy_datafram$Barn_capacity[fertdairy_datafram$Barn_number==j],fertdairy_datafram$Breed[fertdairy_datafram$Barn_number==j],
                   fertdairy_datafram$FCM[fertdairy_datafram$Barn_number==j],fertdairy_datafram$calf_ME[fertdairy_datafram$Barn_number==j],fertdairy_datafram$calf_CP[fertdairy_datafram$Barn_number==j],
                   fertdairy_datafram$fed_calf_P[fertdairy_datafram$Barn_number==j],fertdairy_datafram$heifer_ME[fertdairy_datafram$Barn_number==j],fertdairy_datafram$heifer_CP[fertdairy_datafram$Barn_number==j],
                   fertdairy_datafram$fed_heifer_P[fertdairy_datafram$Barn_number==j],fertdairy_datafram$lact_CP[fertdairy_datafram$Barn_number==j],fertdairy_datafram$fed_lact_P[fertdairy_datafram$Barn_number==j],
                   fertdairy_datafram$dry_CP[fertdairy_datafram$Barn_number==j],fertdairy_datafram$fed_dry_P[fertdairy_datafram$Barn_number==j],fertdairy_datafram$HRS[fertdairy_datafram$Barn_number==j],
                   fertdairy_datafram$Temp[fertdairy_datafram$Barn_number==j],fertdairy_datafram$RHMD[fertdairy_datafram$Barn_number==j],fertdairy_datafram$WS[fertdairy_datafram$Barn_number==j])
  assign(paste0("FERT_DAT_datafram",i,sep=""), data.frame(IFNUM=mgt_datafram$FERT_ID[mgt_datafram$HRU_number==i],FERTNUM=mgt_datafram$FERT_NAME[mgt_datafram$HRU_number==i],FMINN=format(round(Farm_df$Nmin_frac[Farm_df$Date==mgt_datafram$Date_app[mgt_datafram$HRU_number==i]], 3),nsmall=3),
                                                          FMINP=format(round(Farm_df$Pmin_frac[Farm_df$Date==mgt_datafram$Date_app[mgt_datafram$HRU_number==i]], 3),nsmall=3),
                                                          FORGN=format(round(Farm_df$Norg_frac[Farm_df$Date==mgt_datafram$Date_app[mgt_datafram$HRU_number==i]], 3),nsmall=3),
                                                          FORGP=format(round(Farm_df$Porg_frac[Farm_df$Date==mgt_datafram$Date_app[mgt_datafram$HRU_number==i]],3),nsmall=3),
                                                          FNH3N=format(round(0.990, 3), nsmall = 3),
                                                          BACTPDB=format(round(0.000, 3), nsmall = 3),
                                                          BACTLPDB=format(round(0.000, 3), nsmall = 3),
                                                          BACTKDDB=format(round(0.000, 3), nsmall = 3),
                                                          X=format(round(0.600, 3), nsmall = 3),
                                                          Y=2))
  
    }
  
  
for (i in unique(mgt_datafram$HRU_number)){
  #i=10111
  cfilename_fertA=paste0(pathtofile,"/fert.dat")
  cfilename_fert=readLines(paste0(pathtofile,"/fert.dat"))
  l=formatC(get(paste0("FERT_DAT_datafram",i,sep= ""))$IFNUM, width = 4, format = "d", flag = "")
  assign(paste0("new_fert",i,sep=""), paste0(l," ",get(paste0("FERT_DAT_datafram",i,sep= ""))$FERTNUM,"   ",get(paste0("FERT_DAT_datafram",i,sep= ""))$FMINN,"   ",get(paste0("FERT_DAT_datafram",i,sep= ""))$FMINP,
                                             "   ",get(paste0("FERT_DAT_datafram",i,sep= ""))$FORGN,"   ",get(paste0("FERT_DAT_datafram",i,sep= ""))$FORGP,"   ",get(paste0("FERT_DAT_datafram",i,sep= ""))$FNH3N,"   ",get(paste0("FERT_DAT_datafram",i,sep= ""))$BACTPDB,
                                             "      ",get(paste0("FERT_DAT_datafram",i,sep= ""))$BACTLPDB,"      ",get(paste0("FERT_DAT_datafram",i,sep= ""))$BACTKDDB,"   ",get(paste0("FERT_DAT_datafram",i,sep= ""))$X,"   ",get(paste0("FERT_DAT_datafram",i,sep= ""))$Y) )
  write(get(paste0("new_fert",i,sep= "")),file=list.files(pattern = "fert.dat"), sep = "\n",append = TRUE)
}
#----CHECK IF THE NEW LINES ADDED CORRECTLY
file.edit("fert.dat")
###################################

###############################################################################
##########NOW RUN THE MODEL
##########################################################################
####################################################################################################
##now run the model
file.remove(list.files(pattern="output."))
runSWAT2012()
source("https://r-forge.r-project.org/scm/viewvc.php/*checkout*/pkg/SWATmodel/R/readSWAT.R?root=ecohydrology")
save(readSWAT,file="readSWAT.R")
load("readSWAT.R")
outdata_rch_MULTIBARNHRU = readSWAT("rch",".")
outdata_sub_MULTIBARNHRU=readSWAT("sub",".")

cfilename_hruoutA=paste0(pathtofile,"/output.hru")
cfilename_hruout=readLines(paste0(pathtofile,"/output.hru"))
textreplace=gsub(pattern = "WTAB CLIm WTAB SOLm", replace="WTABclimwtabsolm",x=cfilename_hruout)
writeLines(textreplace, con=cfilename_hruoutA)
outdata_hru_MULTIBARNHRU=readSWAT("hru",".") 
###-------------------------------RUN DIFFERNT FARMS-----------------------
#FARM 1 "B01"
##-------------------------------
j="B01"
Farm1=Dairy_df(fertdairy_datafram$kMortality_calf[fertdairy_datafram$Barn_number==j],fertdairy_datafram$kMortality_yearling[fertdairy_datafram$Barn_number==j],fertdairy_datafram$kMortality_bredHeifer[fertdairy_datafram$Barn_number==j],
                 fertdairy_datafram$kMortality_LH[fertdairy_datafram$Barn_number==j],fertdairy_datafram$kMortality_BLH[fertdairy_datafram$Barn_number==j],fertdairy_datafram$kMortality_DBH[fertdairy_datafram$Barn_number==j],
                 fertdairy_datafram$kMortality_SecondLC[fertdairy_datafram$Barn_number==j],fertdairy_datafram$kMortality_BredSecLC[fertdairy_datafram$Barn_number==j],fertdairy_datafram$kMortality_DSecondLC[fertdairy_datafram$Barn_number==j],
                 fertdairy_datafram$kMortality_ThirdLC[fertdairy_datafram$Barn_number==j],fertdairy_datafram$kMortality_BredThirdLC[fertdairy_datafram$Barn_number==j],fertdairy_datafram$kMortality_DThirdLC[fertdairy_datafram$Barn_number==j],
                 fertdairy_datafram$kMortality_LC[fertdairy_datafram$Barn_number==j],fertdairy_datafram$kMotality_BredLC[fertdairy_datafram$Barn_number==j],fertdairy_datafram$kMortality_DC[fertdairy_datafram$Barn_number==j],
                 fertdairy_datafram$HFCR[fertdairy_datafram$Barn_number==j],fertdairy_datafram$HSCR[fertdairy_datafram$Barn_number==j],fertdairy_datafram$C2CR[fertdairy_datafram$Barn_number==j],fertdairy_datafram$C3CR[fertdairy_datafram$Barn_number==j],
                 fertdairy_datafram$C4CR[fertdairy_datafram$Barn_number==j],fertdairy_datafram$PropF[fertdairy_datafram$Barn_number==j],fertdairy_datafram$PropKeep[fertdairy_datafram$Barn_number==j],
                 fertdairy_datafram$iCalf[fertdairy_datafram$Barn_number==j],fertdairy_datafram$iHeifer_first_lact[fertdairy_datafram$Barn_number==j],fertdairy_datafram$iHeifer_second_lact[fertdairy_datafram$Barn_number==j],
                 fertdairy_datafram$iHeifer_third_lact[fertdairy_datafram$Barn_number==j],fertdairy_datafram$iHeifer_first_dry[fertdairy_datafram$Barn_number==j],fertdairy_datafram$iHeifer_second_dry[fertdairy_datafram$Barn_number==j],
                 fertdairy_datafram$iHeifer_third_dry[fertdairy_datafram$Barn_number==j],fertdairy_datafram$iLact[fertdairy_datafram$Barn_number==j],fertdairy_datafram$iDry[fertdairy_datafram$Barn_number==j],
                 fertdairy_datafram$iyearling[fertdairy_datafram$Barn_number==j],fertdairy_datafram$ibredHeifer[fertdairy_datafram$Barn_number==j],fertdairy_datafram$iBLH[fertdairy_datafram$Barn_number==j],
                 fertdairy_datafram$iBredSecLC[fertdairy_datafram$Barn_number==j],fertdairy_datafram$iBredThirdLC[fertdairy_datafram$Barn_number==j],fertdairy_datafram$iBredLC[fertdairy_datafram$Barn_number==j],
                 fertdairy_datafram$Barn_capacity[fertdairy_datafram$Barn_number==j],fertdairy_datafram$Breed[fertdairy_datafram$Barn_number==j],
                 fertdairy_datafram$FCM[fertdairy_datafram$Barn_number==j],fertdairy_datafram$calf_ME[fertdairy_datafram$Barn_number==j],fertdairy_datafram$calf_CP[fertdairy_datafram$Barn_number==j],
                 fertdairy_datafram$fed_calf_P[fertdairy_datafram$Barn_number==j],fertdairy_datafram$heifer_ME[fertdairy_datafram$Barn_number==j],fertdairy_datafram$heifer_CP[fertdairy_datafram$Barn_number==j],
                 fertdairy_datafram$fed_heifer_P[fertdairy_datafram$Barn_number==j],fertdairy_datafram$lact_CP[fertdairy_datafram$Barn_number==j],fertdairy_datafram$fed_lact_P[fertdairy_datafram$Barn_number==j],
                 fertdairy_datafram$dry_CP[fertdairy_datafram$Barn_number==j],fertdairy_datafram$fed_dry_P[fertdairy_datafram$Barn_number==j],fertdairy_datafram$HRS[fertdairy_datafram$Barn_number==j],
                 fertdairy_datafram$Temp[fertdairy_datafram$Barn_number==j],fertdairy_datafram$RHMD[fertdairy_datafram$Barn_number==j],fertdairy_datafram$WS[fertdairy_datafram$Barn_number==j])


#####-------------------------
#farm2 " "B02"
#------------------------------
j="B02"
Farm2=Dairy_df(fertdairy_datafram$kMortality_calf[fertdairy_datafram$Barn_number==j],fertdairy_datafram$kMortality_yearling[fertdairy_datafram$Barn_number==j],fertdairy_datafram$kMortality_bredHeifer[fertdairy_datafram$Barn_number==j],
               fertdairy_datafram$kMortality_LH[fertdairy_datafram$Barn_number==j],fertdairy_datafram$kMortality_BLH[fertdairy_datafram$Barn_number==j],fertdairy_datafram$kMortality_DBH[fertdairy_datafram$Barn_number==j],
               fertdairy_datafram$kMortality_SecondLC[fertdairy_datafram$Barn_number==j],fertdairy_datafram$kMortality_BredSecLC[fertdairy_datafram$Barn_number==j],fertdairy_datafram$kMortality_DSecondLC[fertdairy_datafram$Barn_number==j],
               fertdairy_datafram$kMortality_ThirdLC[fertdairy_datafram$Barn_number==j],fertdairy_datafram$kMortality_BredThirdLC[fertdairy_datafram$Barn_number==j],fertdairy_datafram$kMortality_DThirdLC[fertdairy_datafram$Barn_number==j],
               fertdairy_datafram$kMortality_LC[fertdairy_datafram$Barn_number==j],fertdairy_datafram$kMotality_BredLC[fertdairy_datafram$Barn_number==j],fertdairy_datafram$kMortality_DC[fertdairy_datafram$Barn_number==j],
               fertdairy_datafram$HFCR[fertdairy_datafram$Barn_number==j],fertdairy_datafram$HSCR[fertdairy_datafram$Barn_number==j],fertdairy_datafram$C2CR[fertdairy_datafram$Barn_number==j],fertdairy_datafram$C3CR[fertdairy_datafram$Barn_number==j],
               fertdairy_datafram$C4CR[fertdairy_datafram$Barn_number==j],fertdairy_datafram$PropF[fertdairy_datafram$Barn_number==j],fertdairy_datafram$PropKeep[fertdairy_datafram$Barn_number==j],
               fertdairy_datafram$iCalf[fertdairy_datafram$Barn_number==j],fertdairy_datafram$iHeifer_first_lact[fertdairy_datafram$Barn_number==j],fertdairy_datafram$iHeifer_second_lact[fertdairy_datafram$Barn_number==j],
               fertdairy_datafram$iHeifer_third_lact[fertdairy_datafram$Barn_number==j],fertdairy_datafram$iHeifer_first_dry[fertdairy_datafram$Barn_number==j],fertdairy_datafram$iHeifer_second_dry[fertdairy_datafram$Barn_number==j],
               fertdairy_datafram$iHeifer_third_dry[fertdairy_datafram$Barn_number==j],fertdairy_datafram$iLact[fertdairy_datafram$Barn_number==j],fertdairy_datafram$iDry[fertdairy_datafram$Barn_number==j],
               fertdairy_datafram$iyearling[fertdairy_datafram$Barn_number==j],fertdairy_datafram$ibredHeifer[fertdairy_datafram$Barn_number==j],fertdairy_datafram$iBLH[fertdairy_datafram$Barn_number==j],
               fertdairy_datafram$iBredSecLC[fertdairy_datafram$Barn_number==j],fertdairy_datafram$iBredThirdLC[fertdairy_datafram$Barn_number==j],fertdairy_datafram$iBredLC[fertdairy_datafram$Barn_number==j],
               fertdairy_datafram$Barn_capacity[fertdairy_datafram$Barn_number==j],fertdairy_datafram$Breed[fertdairy_datafram$Barn_number==j],
               fertdairy_datafram$FCM[fertdairy_datafram$Barn_number==j],fertdairy_datafram$calf_ME[fertdairy_datafram$Barn_number==j],fertdairy_datafram$calf_CP[fertdairy_datafram$Barn_number==j],
               fertdairy_datafram$fed_calf_P[fertdairy_datafram$Barn_number==j],fertdairy_datafram$heifer_ME[fertdairy_datafram$Barn_number==j],fertdairy_datafram$heifer_CP[fertdairy_datafram$Barn_number==j],
               fertdairy_datafram$fed_heifer_P[fertdairy_datafram$Barn_number==j],fertdairy_datafram$lact_CP[fertdairy_datafram$Barn_number==j],fertdairy_datafram$fed_lact_P[fertdairy_datafram$Barn_number==j],
               fertdairy_datafram$dry_CP[fertdairy_datafram$Barn_number==j],fertdairy_datafram$fed_dry_P[fertdairy_datafram$Barn_number==j],fertdairy_datafram$HRS[fertdairy_datafram$Barn_number==j],
               fertdairy_datafram$Temp[fertdairy_datafram$Barn_number==j],fertdairy_datafram$RHMD[fertdairy_datafram$Barn_number==j],fertdairy_datafram$WS[fertdairy_datafram$Barn_number==j])


p1 <- ggplot(Farm1, aes(x=Date, y=TotalNP_barn)) +
  geom_line(color="red", size=2) +
  ggtitle("Farm1")
p2 <- ggplot(Farm2, aes(x=Date, y=TotalNP_barn)) +
  geom_line(color="black",size=2) +
  ggtitle("Farm2") 


p1 + p2 + plot_layout(ncol = 1, widths = c(1, 1))

