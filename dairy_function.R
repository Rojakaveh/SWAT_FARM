Dairy_df=function(iCalf = 220,iHeifer_first_lact = 70,iHeifer_second_lact = 70,iHeifer_third_lact =70,iHeifer_first_dry = 15,iHeifer_second_dry = 15,
                  iHeifer_third_dry = 15,iLact = 500,iDry = 100,lact_target = 100,kHeiferCull = 0.22,firstCull = 0.1,secondCull = 0.2,thirdCull = 0.3,
                  fourthCull = 0.5,kCull = 80.0,kMortality = 0.07,Breed = 1,FCM = 3.0,calf_ME = 5.0,calf_CP = 16.0,fed_calf_P = 0.45,
                  heifer_ME = 5.0,heifer_CP = 17.0,fed_heifer_P = 0.45,lact_CP = 17.0,fed_lact_P = 0.45,dry_CP = 17.0,fed_dry_P = 0.45,HRS = 12.0,Temp = 24.0,
                  RHMD = 75.0,WS = 1.0,Date=as.array(AllDays$date)){
  
  #####################################################################################
  
  
  iBarn=iHeifer_first_lact + iHeifer_second_lact + iHeifer_third_lact + iLact # init # of animals in the barn
  iPasture=iCalf + iHeifer_first_dry + iHeifer_second_dry + iHeifer_third_dry + iDry #init # of animals in the pasture
  animal_number=data.frame(matrix(ncol=40, nrow=nrow(Date)))
  x=c("Date","calves_death","calves_cull","age_out","born","first_lact_death","first_lact_cull","first_age_out",
      "first_dry_death","first_dry_cull","first_dry_age_out","second_lact_death","second_lact_cull",
      "second_age_out","second_dry_death","second_dry_cull","second_dry_age_out","third_lact_death","third_lact_cull",
      "third_age_out","third_dry_death","third_dry_cull","third_dry_age_out","lact_death","lact_cull","fourth_age_out",
      "dry_death","dry_cull","dry_age_out","nCalf","nHeifer_first_lact","nHeifer_first_dry","nHeifer_second_lact",
      "nHeifer_second_dry","nHeifer_third_lact","nHeifer_third_dry","nLact","nDry","nBarn","nPasture")
  colnames(animal_number)=x
  animal_number$nCalf[1]=iCalf 
  animal_number$nHeifer_first_lact[1]=iHeifer_first_lact  
  animal_number$nHeifer_second_lact[1]=iHeifer_second_lact  
  animal_number$nHeifer_third_lact[1]=iHeifer_third_lact 
  animal_number$nHeifer_first_dry[1]=iHeifer_first_dry 
  animal_number$nHeifer_second_dry[1]= iHeifer_second_dry
  animal_number$nHeifer_third_dry[1]=iHeifer_third_dry
  animal_number$nLact[1]=iLact
  animal_number$nDry[1]=iDry
  animal_number$nBarn[1]=iHeifer_first_lact + iHeifer_second_lact + iHeifer_third_lact + iLact 
  animal_number$nPasture[1]=iCalf + iHeifer_first_dry + iHeifer_second_dry + iHeifer_third_dry + iDry  
  animal_number$Date=Date 
  for (i in (1:(nrow(Date)))){
    #defining calf populations
    animal_number$calves_death[i]= animal_number$nCalf[i]*kMortality/365
    animal_number$calves_cull[i] = animal_number$nCalf[i]*kHeiferCull/365
    animal_number$age_out[i] = (animal_number$nCalf[i] )*(1/(365.0*2))
    animal_number$born [i]= (animal_number$nHeifer_first_dry[i] + animal_number$nHeifer_second_dry[i] + animal_number$nHeifer_third_dry[i] + animal_number$nDry[i])/(60*0.5)
    
    #Defining 1st lactation heifer pools
    animal_number$first_lact_death[i] = (animal_number$nHeifer_first_lact[i]*kMortality)/365
    animal_number$first_lact_cull[i] = (animal_number$nHeifer_first_lact[i]*firstCull)/365
    animal_number$first_age_out[i] = (animal_number$nHeifer_first_lact[i]/305)
    
    #Defining 1st dry heifer pools
    animal_number$first_dry_death[i] = (animal_number$nHeifer_first_dry[i]*kMortality)/365
    animal_number$first_dry_cull[i] = (animal_number$nHeifer_first_dry[i]*firstCull)/365
    animal_number$first_dry_age_out[i] = (animal_number$nHeifer_first_dry[i]/60)
    
    
    
    #Defining 2nd lactation heifer pools
    animal_number$second_lact_death[i] = (animal_number$nHeifer_second_lact[i]*kMortality)/365
    animal_number$second_lact_cull[i] = (animal_number$nHeifer_second_lact[i]*secondCull)/365
    animal_number$second_age_out[i] = (animal_number$nHeifer_second_lact[i]/305)
    
    #Defining 2nd dry heifer pools
    animal_number$second_dry_death[i] = (animal_number$nHeifer_second_dry[i]*kMortality)/365
    animal_number$second_dry_cull[i] = (animal_number$nHeifer_second_dry[i]*secondCull)/365
    animal_number$second_dry_age_out[i] = (animal_number$nHeifer_second_dry[i]/60)
    
    #Defining 3rd lactation heifer pools
    animal_number$third_lact_death[i] = (animal_number$nHeifer_third_lact[i]*kMortality)/365
    animal_number$third_lact_cull[i] = (animal_number$nHeifer_third_lact[i]*thirdCull)/365
    animal_number$third_age_out[i] = (animal_number$nHeifer_third_lact[i]/305)
    
    #Defining 3rd dry heifer pools
    animal_number$third_dry_death[i] = (animal_number$nHeifer_third_dry[i]*kMortality)/365
    animal_number$third_dry_cull[i] = (animal_number$nHeifer_third_dry[i]*thirdCull)/365
    animal_number$third_dry_age_out[i] = (animal_number$nHeifer_third_dry[i]/60)
    
    #Defining mature lactation heifer pools
    animal_number$lact_death[i] = (animal_number$nLact[i]*kMortality)/365
    animal_number$lact_cull[i] = (animal_number$nLact[i]*fourthCull)/365
    animal_number$fourth_age_out[i] = (animal_number$nLact[i]/305)
    
    #Defining mature dry heifer pools		 
    animal_number$dry_death[i]= (animal_number$nDry[i]*kMortality)/365
    animal_number$dry_cull[i] = (animal_number$nDry[i]*thirdCull)/365
    animal_number$dry_age_out[i] = (animal_number$nDry[i]/60)
    if (i+1 <= nrow(Date)){
      animal_number$nCalf[i+1] = animal_number$nCalf[i] + animal_number$born[i] - animal_number$age_out[i] - animal_number$calves_death[i] - animal_number$calves_cull[i]
      animal_number$nHeifer_first_lact[i+1] = animal_number$nHeifer_first_lact[i]+ animal_number$age_out[i] - animal_number$first_age_out[i] - animal_number$first_lact_cull[i] - animal_number$first_lact_death[i]
      animal_number$nHeifer_first_dry[i+1] = animal_number$nHeifer_first_dry[i] + animal_number$first_age_out[i] - animal_number$first_dry_age_out[i] - animal_number$first_dry_cull[i] - animal_number$first_dry_death[i]
      animal_number$nHeifer_second_lact[i+1] = animal_number$nHeifer_second_lact[i] + animal_number$first_dry_age_out[i] - animal_number$second_age_out[i] -animal_number$second_lact_death[i]-animal_number$second_lact_cull[i]
      animal_number$nHeifer_second_dry[i+1] = animal_number$nHeifer_second_dry[i] + animal_number$second_age_out[i] - animal_number$second_dry_age_out[i] - animal_number$second_dry_cull[i] - animal_number$second_dry_death[i]
      animal_number$nHeifer_third_lact[i+1] = animal_number$nHeifer_third_lact[i] + animal_number$second_dry_age_out[i] - animal_number$third_age_out[i] -animal_number$third_lact_death[i]-animal_number$third_lact_cull[i]
      animal_number$nHeifer_third_dry[i+1] = animal_number$nHeifer_third_dry[i] + animal_number$third_age_out[i] - animal_number$third_dry_age_out[i] - animal_number$third_dry_cull[i] - animal_number$third_dry_death[i]
      animal_number$nLact[i+1] = animal_number$nLact[i] + animal_number$third_dry_age_out[i] - animal_number$fourth_age_out[i] -animal_number$lact_death-animal_number$lact_cull[i]
      animal_number$nDry[i+1] = animal_number$nDry[i] + animal_number$fourth_age_out[i] - animal_number$dry_age_out[i] - animal_number$dry_cull[i] - animal_number$dry_death[i]
      animal_number$nBarn[i+1] = animal_number$nHeifer_first_lact[i+1] + animal_number$nHeifer_second_lact[i+1] + animal_number$nHeifer_third_lact[i+1] + animal_number$nLact[i+1]
      animal_number$nPasture[i+1] = animal_number$nCalf[i+1] + animal_number$nHeifer_first_dry[i+1] + animal_number$nHeifer_second_dry[i+1] + animal_number$nHeifer_third_dry[i+1] + animal_number$nDry[i+1]
    }
  }
  
  ##########################################################################
  ##calculation of    CETI | Current month’s effective temperature index, C  (fox et al 2004)
  #####################################################################
  CETI = 27.88 - (0.456 * Temp) + (0.010754 * Temp**2)-
    (0.4905 * RHMD) + (0.00088 * RHMD**2)+ (1.1507 * WS) -
    (0.126447 * WS**2)+ (0.019876 * Temp * RHMD)-
    (0.046313 * Temp * WS)+ (0.4167 * HRS)
  #######################################################################
  #calculation of DMINC |  DMI (DRY MATTER INTAKE) night cooling adjustment, dimensionless
  #########################################################################
  DMINC = (119.62 - 0.9708 * CETI)/100
  ################################################# DMIF_temp is DMI adjustment factor with night cooling, dimensionless
  if(Temp>20){ 
    DMIAF_temp = DMINC
  }else{
    DMIAF_temp = 1.0433 - (0.0044 * Temp) + (0.0001 * Temp**2)
  }
  
  #############################################  NEma is Dietary content of net energy for maintenance (kcal/kg)
  calf_NEma = (1.37 * calf_ME) - (0.138 * calf_ME**2) + (0.0105 * calf_ME**3) - 1.12
  heifer_NEma = (1.37 * heifer_ME) - (0.138 * heifer_ME**2) + (0.0105 * heifer_ME**3) - 1.12
  
  ##correction for breed index (fox et al 2004)
  if(Breed == 1){
    BI = 1.08
  }else{ 
    BI = 1
  }
  
  ##########################################
  ###functions
  #####################################
  ###############################################
  ###1-BODY WEIGHT function ##input t and retuen BW
  ###########################################
  dairy_ADG=function(t){
    A = 619 #asymptotic weight, kg
    k = 0.0020 #Rate parameter
    b = 0.905 # integration constant
    M = 1.2386 #inflection parameter
    BW = A*(1-(b*exp(-k*t)))^M
    return(BW)
  }
  ######################################################################
  ##calculations of weights using dairy_ADG function
  ###############################################################
  #wt updating which is a function of t (time)
  wtLact = dairy_ADG(2500) #wt of lact kg
  wtDry = dairy_ADG(2500)  #wt of dry
  wtCalf = dairy_ADG(365)  #wt of calf
  wtHeifer_first_lact = dairy_ADG(365*2) #wt of HFL
  wtHeifer_second_lact = dairy_ADG(365*3) #wt of HSL
  wtHeifer_third_lact = dairy_ADG(365*4) #wt of HTL
  WTHFD = dairy_ADG(365*2+305) #wt of HFD
  WTSFD = dairy_ADG(365*3+305) #wt of HSD
  WTHTD = dairy_ADG(365*4+305) #wt of HTD
  #################################################################################
  ######### DMI dry matter intak function for calf, BW is an argument and dry matter intake (DMI) is an output
  ##############################################################################
  calfDMI= function(BW){
    SBW = 0.94*BW #SHRUNK BODY WEIGHT
    DMI = (SBW**0.75)*(((0.2435*calf_NEma)-(0.0466*calf_NEma**2)-0.1128)/calf_NEma)*DMIAF_temp*BI
    return(DMI)
  }
  ###############################################
  ####DMI function for heifers, BW is an argument and dry matter intake (DMI) is an output
  ######################################
  heiferDMI=function(BW){
    SBW = 0.94*BW
    DMI = (SBW**0.75)*(((0.2435*calf_NEma)-(0.0466*calf_NEma**2)-0.0869)/heifer_NEma)*DMIAF_temp*BI
    return(DMI)
  }
  #################################################
  #####DMI function for lact,BW is an argument and dry matter intake (DMI) is an output
  ###################################################
  lactDMI=function(BW){
    SBW = 0.94*BW
    DMI = ((0.0185 * BW) + (0.305 * FCM))*DMIAF_temp*BI
    return(DMI)
  }
  ##############################################################
  ################ DMI function for dry cows, BW is an argument and dry matter intake (DMI) is an output
  #############################################
  dryDMI=function(BW){
    SBW = 0.94*BW
    DMI = (0.0185 * SBW)*DMIAF_temp*BI
    return(DMI)
  }
  
  #########################################################
  ###calculation of DMIs for each pool based on above functions
  ########################################################
  lact_DMI = lactDMI(wtLact)
  calf_DMI = calfDMI(wtCalf)
  heifer_first_lact_DMI = heiferDMI(wtHeifer_first_lact)
  heifer_second_lact_DMI = heiferDMI(wtHeifer_second_lact)
  heifer_third_lact_DMI = heiferDMI(wtHeifer_third_lact)
  heifer_first_dry_DMI = heiferDMI(WTHFD)
  heifer_second_dry_DMI = heiferDMI(WTSFD)
  heifer_third_dry_DMI = heiferDMI(WTHTD)
  dry_DMI = dryDMI(wtDry)
  
  #####################################################################
  ##############Nitrogen Equations per cow , gr
  #####################################################################
  ###1) N excretion from heifer , DMI is an argument and Nexc is output, gr
  heiferNexc=function(DMI){
    Nexc = ((((heifer_CP/100)*(DMI*1000))/(DMI*1000)*DMI)*78.39+51.4) #per cow , heifer_CP and DMI
    return(Nexc)
  }
  
  ###2) N excretion from calf, DMI is an argument and Nexc is output
  calfNexc=function(DMI){
    Nexc = ((((calf_CP/100)*(DMI*1000))/(DMI*1000)*DMI)*112.55) # calf_CP and DMI
    return(Nexc)
    
  }
  ###3) N excretion from lact 
  lactNexc=function(DMI){
    Nexc = (((((lact_CP/100)*(DMI*1000))/(DMI*1000)*DMI)*84.1)+(wtLact*0.196)) #lact_cp and DMI
    return(Nexc)
  }
  
  #####4)N excretion from dry ,
  dryNexc=function(DMI){
    Nexc = ((((dry_CP/100)*(DMI*1000))/(DMI*1000)*DMI)*78.39+51.4) ##dry_CP and DMI
    return(Nexc)
  }
  
  #####################################################
  # Phosphorus Equations per cow 
  ######################################################
  ###1) P excretion from calf
  calfPexc=function(DMI){
    Pexc = ((((fed_calf_P/100)*(DMI*1000))/(DMI*1000)*DMI)*622.03) # fed_calf_P and DMI 
    return(Pexc)
  }
  
  ###2) P excreation from heifer or  lact
  cowPexc=function(DMI, fed_P){
    Pexc = 7.5 + ((((fed_P/100)*(DMI*1000))/(DMI*1000)*DMI)*560.7+2.1 ) #fed_P and DMI
    return(Pexc)
  }
  
  ###3) P excretion from dry 
  dryPexc=function(DMI, fed_P){
    Pexc = ((fed_P/100)*(DMI*1000)*0.76)
    return(Pexc)
  }
  
  ###############adding a column of amount of fertilizer applied to that hru
  animal_number$Fert_applied=0
  for (i in HRU_number){
    #i=10114
    animal_number$Fert_applied[animal_number$Date==mgt_datafram$Date_app[mgt_datafram$HRU_number==i]]=mgt_datafram$FERT_APPLIED[mgt_datafram$HRU_number==i]
  }
  ##########################################################################
  ##################################################
  #N and P produced by 1 cow,gr
  ###################################################
  #1)N
  N_dry = dryNexc(dry_DMI)/1000 #  kg N by dry cow
  N_calf = calfNexc(calf_DMI)/1000  #kg N by calf
  N_heifer_first_dry = heiferNexc(heifer_first_dry_DMI)/1000 #kg N by HFD
  N_heifer_second_dry = heiferNexc(heifer_second_dry_DMI)/1000 #kg N by HSD
  N_heifer_third_dry = heiferNexc(heifer_third_dry_DMI)/1000 #kg N by HTD
  N_heifer_first_lact = heiferNexc(heifer_first_lact_DMI)/1000 #kg N by HFL
  N_heifer_second_lact= heiferNexc(heifer_second_lact_DMI)/1000 #kg N by HSL
  N_heifer_third_lact= heiferNexc(heifer_third_lact_DMI)/1000 #kg N by HTL
  N_lact= lactNexc(lact_DMI)/1000 #kg N by lact 
  #2)P
  P_dry = dryPexc(dry_DMI,fed_dry_P)/1000 #kg P by dry 
  P_calf = calfPexc(calf_DMI)/1000 #kg P by calf
  P_heifer_first_dry = dryPexc(heifer_first_dry_DMI, fed_heifer_P)/1000 #kg P by HFD
  P_heifer_second_dry = dryPexc(heifer_second_dry_DMI, fed_heifer_P)/1000 #kg P by HSD
  P_heifer_third_dry = dryPexc(heifer_third_dry_DMI, fed_heifer_P)/1000 #kg P by HTD
  P_heifer_first_lact= cowPexc(heifer_first_lact_DMI, fed_heifer_P)/1000 #kg P by HFL
  P_heifer_second_lact = cowPexc(heifer_second_lact_DMI,  fed_heifer_P)/1000 #kg P by HSL
  P_heifer_third_lact = cowPexc(heifer_third_lact_DMI,  fed_heifer_P)/1000 #kg P by HTL
  P_lact = cowPexc(lact_DMI,fed_lact_P)/1000 #kg P by lact
  #####################################################
  #N and P updates for the number of animals multiply by the number of animal in each pool
  #####################################################
  ## N is just a function of DMI 
  animal_number$lact_N=N_lact*animal_number$nLact #kg of total N produced by the number of lact cows
  animal_number$dry_N= N_dry*animal_number$nDry # kg of total N produced by the number of dry cows
  animal_number$calf_N=N_calf*animal_number$nCalf # kg of total N produced by the numbers of calves
  animal_number$heifer_first_lact_N=N_heifer_first_lact*animal_number$nHeifer_first_lact #kg of total N produced by the number of HFL
  animal_number$heifer_second_lact_N=N_heifer_second_lact*animal_number$nHeifer_second_lact #kg of total N produced by number of HSL
  animal_number$heifer_third_lact_N=N_heifer_third_lact*animal_number$nHeifer_third_lact #kg of total N produced by number of HTL
  animal_number$heifer_first_dry_N=N_heifer_first_dry*animal_number$nHeifer_first_dry #kg of total N produced by number of HFD
  animal_number$heifer_second_dry_N=N_heifer_second_dry*animal_number$nHeifer_second_dry #kg of total N produced by HSD
  animal_number$heifer_third_dry_N=N_heifer_third_dry*animal_number$nHeifer_third_dry #kg of total N produced by HTD
  
  ##P is a fuction of DMI and fed_P	except calves 
  animal_number$lact_P=P_lact*animal_number$nLact #kg of total P calf
  animal_number$dry_P = P_dry*animal_number$nDry #kg of total P dry
  animal_number$calf_P = P_calf*animal_number$nCalf #kg of total P calves
  animal_number$heifer_first_lact_P=P_heifer_first_lact*animal_number$nHeifer_first_lact #kg of total P HFL
  animal_number$heifer_second_lact_P = P_heifer_second_lact*animal_number$nHeifer_second_lact #kg of total P HSL
  animal_number$heifer_third_lact_P = P_heifer_third_lact*animal_number$nHeifer_third_lact #kg of total P HTL
  animal_number$heifer_first_dry_P = P_heifer_first_dry*animal_number$nHeifer_first_dry #kg of total P HFD
  animal_number$heifer_second_dry_P = P_heifer_second_dry*animal_number$nHeifer_second_dry #kg of total P HSD
  animal_number$heifer_third_dry_P = P_heifer_third_dry*animal_number$nHeifer_third_dry #kg of total P HTD
  ########################################################################
  ######daily TOTAL N and total P in Barn AND PASTURE, lactation cows are in the barn and dry cows+calves in the pasture
  #################################################################
  animal_number$total_N_barn= animal_number$lact_N + animal_number$heifer_first_lact_N + 
    animal_number$heifer_second_lact_N + animal_number$heifer_third_lact_N ## daily kg total N produced in the barn
  
  animal_number$total_P_barn = animal_number$lact_P + animal_number$heifer_first_lact_P +
    animal_number$heifer_second_lact_P + animal_number$heifer_third_lact_P ##daily kg of total P produced in the barn
  
  animal_number$total_N_Past = animal_number$dry_N + animal_number$calf_N + animal_number$heifer_first_dry_N +
    animal_number$heifer_second_dry_N + animal_number$heifer_third_dry_N ##daily kg of total N produced in the pasture
  
  animal_number$total_P_Past = animal_number$dry_P + animal_number$calf_P + animal_number$heifer_first_dry_P + 
    animal_number$heifer_second_dry_P + animal_number$heifer_third_dry_P  ##daily kg of total P produced in the pasture
  #################################################################
  ########daity totalNP in the barn
  #######################################################################
  animal_number$totalNP=animal_number$total_N_barn+animal_number$total_P_barn
  
  ##############################################################
  #####################1) TOTAL N AND P STORED IN THE BARN/ #########################################################
  ######2) TOTAL N+P FROM STORED MANURE IN BARN, kgams 
  #############################################################
  ##############################################################
  animal_number$Total_N_barn_stored=0
  animal_number$Total_P_barn_stored=0
  animal_number$Total_N_barn_stored[1]=animal_number$total_N_barn[1]
  animal_number$Total_P_barn_stored[1]=animal_number$total_P_barn[1]
  animal_number$TotalNP_barn=NA
  animal_number$TotalNP_barn[1]=animal_number$totalNP[1]
  for(i in 1:nrow(animal_number)){
    if (i+1 <= nrow(Date)){
      if(animal_number$Fert_applied[i] <=animal_number$TotalNP_barn[i]){
        animal_number$TotalNP_barn[i+1]=animal_number$TotalNP_barn[i]+animal_number$totalNP[i]-animal_number$Fert_applied[i]
        animal_number$Total_N_barn_stored[i+1]=animal_number$Total_N_barn_stored[i] + animal_number$total_N_barn[i+1]-(animal_number$Total_N_barn_stored[i]/animal_number$TotalNP_barn[i])*animal_number$Fert_applied[i]
        animal_number$Total_P_barn_stored[i+1] = animal_number$Total_P_barn_stored[i] + animal_number$total_P_barn[i+1]-(animal_number$Total_P_barn_stored[i]/animal_number$TotalNP_barn[i])*animal_number$Fert_applied[i]
      } else{
        stop("Increase the number of animal in the barn or decrease the amount of fertilizer applied")
        
      }
    }
  }
  ###################################################
  ###getting mineralized and organ N and P (daily kg)
  #############################################
  ##eghball 2002, P
  animal_number$Pmin = animal_number$total_P_barn * 0.75  #kgams mineralized P daily produced
  animal_number$Porg = animal_number$total_P_barn * 0.25 ##kgams organic P
  animal_number$Pmin_stored =animal_number$Total_P_barn_stored*0.75 #kgams of mineralzied P in stored manure in the barn
  animal_number$Porg_stored = animal_number$Total_P_barn_stored*0.25 #kgams of organic P in stored manure in the barn
  
  ###Van Kessel 2002, N
  animal_number$Nmin= animal_number$total_N_barn * 0.4 #kgams mineralized N daily
  animal_number$Norg= animal_number$total_N_barn * 0.6 #kgams organic N daily 
  animal_number$Nmin_stored =animal_number$Total_N_barn_stored * 0.4 #kgams of mineralized N in stored manure in the barn
  animal_number$Norg_stored = animal_number$Total_N_barn_stored * 0.6 #kgams of organic N in stored manure in the barn
  
  
  
  ####get fractions 
  #####################################
  animal_number$Pmin_frac=NA ## fraction of mineral P in fertilizer (dairy manure) (kg min_P/kg fertilizer)
  animal_number$Porg_frac=NA ## fraction of organic P in fertilizer (dairy manure) (kg org-P/kg fertilizer)
  animal_number$Norg_frac=NA ## fraction of organic N in fertilizer (dairy manure) (kg org-N/kg fertilizer)
  animal_number$Nmin_frac=NA ## fraction mineral N in fertilizer (dairy manure) (kg min_N/kg fertilizer)
  if (animal_number$TotalNP_barn == 0){
    animal_number$Pmin_frac = 0.
    animal_number$Porg_frac = 0.
    animal_number$Norg_frac = 0.
    animal_number$Nmin_frac = 0.	           
  }else {  
    animal_number$Pmin_frac = animal_number$Pmin_stored/animal_number$TotalNP_barn
    animal_number$Porg_frac = animal_number$Porg_stored/animal_number$TotalNP_barn
    animal_number$Norg_frac = animal_number$Norg_stored/animal_number$TotalNP_barn
    animal_number$Nmin_frac = animal_number$Nmin_stored/animal_number$TotalNP_barn
  }
  return(animal_number)
}
