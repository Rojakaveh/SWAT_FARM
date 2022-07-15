pacman::p_load(multisensi,sensitivity)
source("https://raw.githubusercontent.com/Rojakaveh/SWAT_FARM/main/dairysensifunction.R")
J=seq(from=1, to=730, by=5)
##### 1. buiLDING LOOPED FUNCTION
Dairy_df_Looped <- function(X, Jday=J){
  out <- matrix(nrow = nrow(X), ncol = length(Jday), NA)
  for(i in 1:nrow(X)){
    out[i, ] <- Dairy_df(kMortality_calf = X$kMortality_calf[i],kMortality_yearling = X$kMortality_yearling[i],
                         kMortality_bredHeifer = X$kMortality_bredHeifer[i],kMortality_LH = X$kMortality_LH[i],
                         kMortality_BLH = X$kMortality_BLH[i],kMortality_DBH = X$kMortality_DBH[i],
                         kMortality_SecondLC = X$kMortality_SecondLC[i],kMortality_BredSecLC = X$kMortality_BredSecLC[i],
                         kMortality_DSecondLC = X$kMortality_DSecondLC[i],kMortality_ThirdLC = X$kMortality_ThirdLC[i],
                         kMortality_BredThirdLC = X$kMortality_BredThirdLC[i],kMortality_DThirdLC = X$kMortality_DThirdLC[i],
                         kMortality_LC = X$kMortality_LC[i],kMotality_BredLC =X$kMotality_BredLC[i],
                         kMortality_DC = X$kMortality_DC[i],HFCR = X$HFCR[i],
                         HSCR = X$HSCR[i],C2CR = X$C2CR[i],
                         C3CR = X$C3CR[i],C4CR = X$C4CR[i],
                         PropF = X$PropF[i],PropKeep = X$PropKeep[i],
                         FCM = X$FCM[i],calf_ME = X$calf_ME[i],
                         calf_CP = X$calf_CP[i],fed_calf_P =X$fed_calf_P[i],
                         heifer_ME = X$heifer_ME[i],heifer_CP = X$heifer_CP[i],
                         fed_heifer_P = X$fed_heifer_P[i],lact_CP = X$lact_CP[i],
                         fed_lact_P = X$fed_lact_P[i],dry_CP = X$dry_CP[i],
                         fed_dry_P = X$fed_dry_P[i],HRS = X$HRS[i],
                         Temp = X$Temp[i],RHMD = X$RHMD[i],
                         WS = X$WS[i],Jday = Jday)
  }
  out <- as.data.frame(out)
  names(out) <- paste("Jday", Jday, sep = "")
  return(out)
  
}
######fast99 sensi

Dairy_df_Looped.seq.fast <- multisensi(design = fast99, model =  Dairy_df_Looped,
                                       center = FALSE, reduction = NULL, analysis = analysis.sensitivity,
                                       design.args=list( factors=c("kMortality_calf","kMortality_yearling",
                                                                   "kMortality_bredHeifer","kMortality_LH ",
                                                                   "kMortality_BLH","kMortality_DBH",
                                                                   "kMortality_SecondLC","kMortality_BredSecLC",
                                                                   "kMortality_DSecondLC","kMortality_ThirdLC",
                                                                   "kMortality_BredThirdLC","kMortality_DThirdLC",
                                                                   "kMortality_LC","kMotality_BredLC",
                                                                   "kMortality_DC","HFCR",
                                                                   "HSCR","C2CR",
                                                                   "C3CR","C4CR",
                                                                   "PropF","PropKeep",
                                                                   "FCM","calf_ME",
                                                                   "calf_CP","fed_calf_P",
                                                                   "heifer_ME","heifer_CP",
                                                                   "fed_heifer_P","lact_CP",
                                                                   "fed_lact_P","dry_CP",
                                                                   "fed_dry_P","HRS",
                                                                   "Temp","RHMD",
                                                                   "WS"), 
                                                         n=1000, q = "qunif",
                                                         q.arg = list(list(min = 0.03, max = 0.08), 
                                                                      list(min = 0.03, max = 0.08),
                                                                      list(min = 0.03, max = 0.08),
                                                                      list(min = 0.03, max = 0.08),
                                                                      list(min = 0.03, max = 0.08),
                                                                      list(min = 0.03, max = 0.08),
                                                                      list(min = 0.03, max = 0.08),
                                                                      list(min = 0.03, max = 0.08),
                                                                      list(min = 0.03, max = 0.08),
                                                                      list(min = 0.03, max = 0.08),
                                                                      list(min = 0.03, max = 0.08),
                                                                      list(min = 0.03, max = 0.08),
                                                                      list(min = 0.03, max = 0.08),
                                                                      list(min = 0.03, max = 0.08),
                                                                      list(min = 0.03, max = 0.08),
                                                                      list(min = 0.30, max = 0.60),
                                                                      list(min = 0.65, max = 0.85),
                                                                      list(min = 0.45, max = 0.65),
                                                                      list(min = 0.35, max = 0.55),
                                                                      list(min = 0.30, max = 0.50),
                                                                      list(min = 0.50, max = 1),
                                                                      list(min = 0.3, max = 1),
                                                                      list(min = 3, max = 7),
                                                                      list(min = 1.5, max = 4),
                                                                      list(min = 10, max = 30),
                                                                      list(min = 0.1, max = 5),
                                                                      list(min = 1.5, max = 4),
                                                                      list(min = 10, max = 25),
                                                                      list(min = 0.1, max = 5),
                                                                      list(min =10, max = 25),
                                                                      list(min = 0.1, max = 5),
                                                                      list(min = 10, max = 25),
                                                                      list(min = 0.1, max = 5),
                                                                      list(min = 6, max = 18),
                                                                      list(min = 5, max = 28),
                                                                      list(min = 50, max = 100),
                                                                      list(min = 0.1, max = 5))),
                                       analysis.args=list(keep.outputs=FALSE))

print(Dairy_df_Looped.seq.fast,digits=2)
par(mar = c(4,10, 2, 2))
plot(Dairy_df_Looped.seq.fast, normalized = TRUE, color = terrain.colors)
