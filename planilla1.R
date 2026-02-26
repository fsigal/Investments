library(quantmod)
library(tidyverse)

hoy = "2026-02-25"

actualizar_precios <- function(hoy) {

  quantmod::getSymbols("AAL")
  AAL <- tail(AAL$AAL.Close,n = 500)
  AAL <- c(min(AAL),max(AAL),mean(AAL)-2*sd(AAL),mean(AAL)+2*sd(AAL),
         AAL[hoy])

  quantmod::getSymbols("AAPL")
  AAPL <- tail(AAPL$AAPL.Close,n = 500)
  AAPL <- c(min(AAPL),max(AAPL),mean(AAPL)-2*sd(AAPL),mean(AAPL)+2*sd(AAPL),
           AAPL[hoy])
  
  quantmod::getSymbols("ABNB")
  ABNB <- tail(ABNB$ABNB.Close,n = 500)
  ABNB <- c(min(ABNB),max(ABNB),mean(ABNB)-2*sd(ABNB),mean(ABNB)+2*sd(ABNB),
           ABNB[hoy])
  
  quantmod::getSymbols("ACN")
  ACN <- tail(ACN$ACN.Close,n = 500)
  ACN <- c(min(ACN),max(ACN),mean(ACN)-2*sd(ACN),mean(ACN)+2*sd(ACN),
           ACN[hoy])
  
  quantmod::getSymbols("ADBE")
  ADBE <- tail(ADBE$ADBE.Close,n = 500)
  ADBE <- c(min(ADBE),max(ADBE),mean(ADBE)-2*sd(ADBE),mean(ADBE)+2*sd(ADBE),
           ADBE[hoy])
  
  quantmod::getSymbols("AIG")
  AIG <- tail(AIG$AIG.Close,n = 500)
  AIG <- c(min(AIG),max(AIG),mean(AIG)-2*sd(AIG),mean(AIG)+2*sd(AIG),
           AIG[hoy])
  
  quantmod::getSymbols("AMD")
  AMD <- tail(AMD$AMD.Close,n = 500)
  AMD <- c(min(AMD),max(AMD),mean(AMD)-2*sd(AMD),mean(AMD)+2*sd(AMD),
           AMD[hoy])
  
  quantmod::getSymbols("AMZN")
  AMZN <- tail(AMZN$AMZN.Close,n = 500)
  AMZN <- c(min(AMZN),max(AMZN),mean(AMZN)-2*sd(AMZN),mean(AMZN)+2*sd(AMZN),
           AMZN[hoy])
  
  quantmod::getSymbols("ANF")
  ANF <- tail(ANF$ANF.Close,n = 500)
  ANF <- c(min(ANF),max(ANF),mean(ANF)-2*sd(ANF),mean(ANF)+2*sd(ANF),
           ANF[hoy])
  
  quantmod::getSymbols("ARKK")
  ARKK <- tail(ARKK$ARKK.Close,n = 500)
  ARKK <- c(min(ARKK),max(ARKK),mean(ARKK)-2*sd(ARKK),mean(ARKK)+2*sd(ARKK),
           ARKK[hoy])
  
  quantmod::getSymbols("AXP")
  AXP <- tail(AXP$AXP.Close,n = 500)
  AXP <- c(min(AXP),max(AXP),mean(AXP)-2*sd(AXP),mean(AXP)+2*sd(AXP),
           AXP[hoy])
  
  quantmod::getSymbols("B")
  B <- tail(B$B.Close,n = 500)
  B <- c(min(B),max(B),mean(B)-2*sd(B),mean(B)+2*sd(B),
           B[hoy])
  
  quantmod::getSymbols("BA")
  BA <- tail(BA$BA.Close,n = 500)
  BA <- c(min(BA),max(BA),mean(BA)-2*sd(BA),mean(BA)+2*sd(BA),
         BA[hoy])
  
  quantmod::getSymbols("BABA")
  BABA <- tail(BABA$BABA.Close,n = 500)
  BABA <- c(min(BABA),max(BABA),mean(BABA)-2*sd(BABA),mean(BABA)+2*sd(BABA),
         BABA[hoy])
  
  quantmod::getSymbols("BAC")
  BAC <- tail(BAC$BAC.Close,n = 500)
  BAC <- c(min(BAC),max(BAC),mean(BAC)-2*sd(BAC),mean(BAC)+2*sd(BAC),
         BAC[hoy])
  
  quantmod::getSymbols("BB")
  BB <- tail(BB$BB.Close,n = 500)
  BB <- c(min(BB),max(BB),mean(BB)-2*sd(BB),mean(BB)+2*sd(BB),
         BB[hoy])
  
  quantmod::getSymbols("BCS")
  BCS <- tail(BCS$BCS.Close,n = 500)
  BCS <- c(min(BCS),max(BCS),mean(BCS)-2*sd(BCS),mean(BCS)+2*sd(BCS),
         BCS[hoy])
  
  quantmod::getSymbols("BIDU")
  BIDU <- tail(BIDU$BIDU.Close,n = 500)
  BIDU <- c(min(BIDU),max(BIDU),mean(BIDU)-2*sd(BIDU),mean(BIDU)+2*sd(BIDU),
         BIDU[hoy])
  
  quantmod::getSymbols("BIIB")
  BIIB <- tail(BIIB$BIIB.Close,n = 500)
  BIIB <- c(min(BIIB),max(BIIB),mean(BIIB)-2*sd(BIIB),mean(BIIB)+2*sd(BIIB),
         BIIB[hoy])
  
  quantmod::getSymbols("BIOX")
  BIOX <- tail(BIOX$BIOX.Close,n = 500)
  BIOX <- c(min(BIOX),max(BIOX),mean(BIOX)-2*sd(BIOX),mean(BIOX)+2*sd(BIOX),
         BIOX[hoy])
  
  quantmod::getSymbols("BITF")
  BITF <- tail(BITF$BITF.Close,n = 500)
  BITF <- c(min(BITF),max(BITF),mean(BITF)-2*sd(BITF),mean(BITF)+2*sd(BITF),
         BITF[hoy])
  
  quantmod::getSymbols("C")
  C <- tail(C$C.Close,n = 500)
  C <- c(min(C),max(C),mean(C)-2*sd(C),mean(C)+2*sd(C),
         C[hoy])
  
  quantmod::getSymbols("CAT")
  CAT <- tail(CAT$CAT.Close,n = 500)
  CAT <- c(min(CAT),max(CAT),mean(CAT)-2*sd(CAT),mean(CAT)+2*sd(CAT),
           CAT[hoy])
  
  quantmod::getSymbols("CL")
  CL <- tail(CL$CL.Close,n = 500)
  CL <- c(min(CL),max(CL),mean(CL)-2*sd(CL),mean(CL)+2*sd(CL),
          CL[hoy])
  
  quantmod::getSymbols("CLS")
  CLS <- tail(CLS$CLS.Close,n = 500)
  CLS <- c(min(CLS),max(CLS),mean(CLS)-2*sd(CLS),mean(CLS)+2*sd(CLS),
         CLS[hoy])
  
  quantmod::getSymbols("COIN")
  COIN <- tail(COIN$COIN.Close,n = 500)
  COIN <- c(min(COIN),max(COIN),mean(COIN)-2*sd(COIN),mean(COIN)+2*sd(COIN),
           COIN[hoy])
  
  quantmod::getSymbols("COST")
  COST <- tail(COST$COST.Close,n = 500)
  COST <- c(min(COST),max(COST),mean(COST)-2*sd(COST),mean(COST)+2*sd(COST),
           COST[hoy])
  
  quantmod::getSymbols("CRM")
  CRM <- tail(CRM$CRM.Close,n = 500)
  CRM <- c(min(CRM),max(CRM),mean(CRM)-2*sd(CRM),mean(CRM)+2*sd(CRM),
           CRM[hoy])
  
  quantmod::getSymbols("CSCO")
  CSCO <- tail(CSCO$CSCO.Close,n = 500)
  CSCO <- c(min(CSCO),max(CSCO),mean(CSCO)-2*sd(CSCO),mean(CSCO)+2*sd(CSCO),
           CSCO[hoy])
  
  quantmod::getSymbols("CVX")
  CVX <- tail(CVX$CVX.Close,n = 500)
  CVX <- c(min(CVX),max(CVX),mean(CVX)-2*sd(CVX),mean(CVX)+2*sd(CVX),
           CVX[hoy])
  
  quantmod::getSymbols("CX")
  CX <- tail(CX$CX.Close,n = 500)
  CX <- c(min(CX),max(CX),mean(CX)-2*sd(CX),mean(CX)+2*sd(CX),
           CX[hoy])
  
  quantmod::getSymbols("DD")
  DD <- tail(DD$DD.Close,n = 500)
  DD <- c(min(DD),max(DD),mean(DD)-2*sd(DD),mean(DD)+2*sd(DD),
           DD[hoy])
  
  quantmod::getSymbols("DE")
  DE <- tail(DE$DE.Close,n = 500)
  DE <- c(min(DE),max(DE),mean(DE)-2*sd(DE),mean(DE)+2*sd(DE),
          DE[hoy])
  
  quantmod::getSymbols("DECK")
  DECK <- tail(DECK$DECK.Close,n = 500)
  DECK <- c(min(DECK),max(DECK),mean(DECK)-2*sd(DECK),mean(DECK)+2*sd(DECK),
          DECK[hoy])
  
  quantmod::getSymbols("DEO")
  DEO <- tail(DEO$DEO.Close,n = 500)
  DEO <- c(min(DEO),max(DEO),mean(DEO)-2*sd(DEO),mean(DEO)+2*sd(DEO),
          DEO[hoy])
  
  quantmod::getSymbols("DIA")
  DIA <- tail(DIA$DIA.Close,n = 500)
  DIA <- c(min(DIA),max(DIA),mean(DIA)-2*sd(DIA),mean(DIA)+2*sd(DIA),
          DIA[hoy])
  
  quantmod::getSymbols("DOCU")
  DOCU <- tail(DOCU$DOCU.Close,n = 500)
  DOCU <- c(min(DOCU),max(DOCU),mean(DOCU)-2*sd(DOCU),mean(DOCU)+2*sd(DOCU),
          DOCU[hoy])
  
  quantmod::getSymbols("DOW")
  DOW <- tail(DOW$DOW.Close,n = 500)
  DOW <- c(min(DOW),max(DOW),mean(DOW)-2*sd(DOW),mean(DOW)+2*sd(DOW),
          DOW[hoy])
  
  quantmod::getSymbols("E")
  E <- tail(E$E.Close,n = 500)
  E <- c(min(E),max(E),mean(E)-2*sd(E),mean(E)+2*sd(E),
           E[hoy])
  
  quantmod::getSymbols("EA")
  EA <- tail(EA$EA.Close,n = 500)
  EA <- c(min(EA),max(EA),mean(EA)-2*sd(EA),mean(EA)+2*sd(EA),
         EA[hoy])
  
  quantmod::getSymbols("EBAY")
  EBAY <- tail(EBAY$EBAY.Close,n = 500)
  EBAY <- c(min(EBAY),max(EBAY),mean(EBAY)-2*sd(EBAY),mean(EBAY)+2*sd(EBAY),
         EBAY[hoy])
  
  quantmod::getSymbols("EEM")
  EEM <- tail(EEM$EEM.Close,n = 500)
  EEM <- c(min(EEM),max(EEM),mean(EEM)-2*sd(EEM),mean(EEM)+2*sd(EEM),
         EEM[hoy])
  
  quantmod::getSymbols("EFX")
  EFX <- tail(EFX$EFX.Close,n = 500)
  EFX <- c(min(EFX),max(EFX),mean(EFX)-2*sd(EFX),mean(EFX)+2*sd(EFX),
         EFX[hoy])
  
  quantmod::getSymbols("ERIC")
  ERIC <- tail(ERIC$ERIC.Close,n = 500)
  ERIC <- c(min(ERIC),max(ERIC),mean(ERIC)-2*sd(ERIC),mean(ERIC)+2*sd(ERIC),
         ERIC[hoy])
  
  quantmod::getSymbols("ETHA")
  ETHA <- tail(ETHA$ETHA.Close,n = 500)
  ETHA <- c(min(ETHA),max(ETHA),mean(ETHA)-2*sd(ETHA),mean(ETHA)+2*sd(ETHA),
         ETHA[hoy])
  
  quantmod::getSymbols("EWZ")
  EWZ <- tail(EWZ$EWZ.Close,n = 500)
  EWZ <- c(min(EWZ),max(EWZ),mean(EWZ)-2*sd(EWZ),mean(EWZ)+2*sd(EWZ),
         EWZ[hoy])
  
  quantmod::getSymbols("F")
  F <- tail(F$F.Close,n = 500)
  F <- c(min(F),max(F),mean(F)-2*sd(F),mean(F)+2*sd(F),
         F[hoy])
  
  quantmod::getSymbols("FCX")
  FCX <- tail(FCX$FCX.Close,n = 500)
  FCX <- c(min(FCX),max(FCX),mean(FCX)-2*sd(FCX),mean(FCX)+2*sd(FCX),
         FCX[hoy])
  
  quantmod::getSymbols("FDX")
  FDX <- tail(FDX$FDX.Close,n = 500)
  FDX <- c(min(FDX),max(FDX),mean(FDX)-2*sd(FDX),mean(FDX)+2*sd(FDX),
         FDX[hoy])
  
  quantmod::getSymbols("FMCC")
  FMCC <- tail(FMCC$FMCC.Close,n = 500)
  FMCC <- c(min(FMCC),max(FMCC),mean(FMCC)-2*sd(FMCC),mean(FMCC)+2*sd(FMCC),
         FMCC[hoy])
  
  quantmod::getSymbols("FMX")
  FMX <- tail(FMX$FMX.Close,n = 500)
  FMX <- c(min(FMX),max(FMX),mean(FMX)-2*sd(FMX),mean(FMX)+2*sd(FMX),
         FMX[hoy])
  
  quantmod::getSymbols("FNMA")
  FNMA <- tail(FNMA$FNMA.Close,n = 500)
  FNMA <- c(min(FNMA),max(FNMA),mean(FNMA)-2*sd(FNMA),mean(FNMA)+2*sd(FNMA),
         FNMA[hoy])
  
  quantmod::getSymbols("FSLR")
  FSLR <- tail(FSLR$FSLR.Close,n = 500)
  FSLR <- c(min(FSLR),max(FSLR),mean(FSLR)-2*sd(FSLR),mean(FSLR)+2*sd(FSLR),
         FSLR[hoy])
  
  quantmod::getSymbols("FXI") 
  FXI <- tail(FXI$FXI.Close,n = 500)
  FXI <- c(min(FXI),max(FXI),mean(FXI)-2*sd(FXI),mean(FXI)+2*sd(FXI),
         FXI[hoy])
  
  quantmod::getSymbols("GE") 
  GE <- tail(GE$GE.Close,n = 500)
  GE <- c(min(GE),max(GE),mean(GE)-2*sd(GE),mean(GE)+2*sd(GE),
          GE[hoy])
  
  quantmod::getSymbols("GGB") 
  GGB <- tail(GGB$GGB.Close,n = 500)
  GGB <- c(min(GGB),max(GGB),mean(GGB)-2*sd(GGB),mean(GGB)+2*sd(GGB),
           GGB[hoy])
  
  quantmod::getSymbols("GLD") 
  GLD <- tail(GLD$GLD.Close,n = 500)
  GLD <- c(min(GLD),max(GLD),mean(GLD)-2*sd(GLD),mean(GLD)+2*sd(GLD),
         GLD[hoy])
  
  quantmod::getSymbols("GLOB") 
  GLOB <- tail(GLOB$GLOB.Close,n = 500)
  GLOB <- c(min(GLOB),max(GLOB),mean(GLOB)-2*sd(GLOB),mean(GLOB)+2*sd(GLOB),
           GLOB[hoy])
  
  quantmod::getSymbols("GM") 
  GM <- tail(GM$GM.Close,n = 500)
  GM <- c(min(GM),max(GM),mean(GM)-2*sd(GM),mean(GM)+2*sd(GM),
           GM[hoy])
  
  quantmod::getSymbols("GOOGL") 
  GOOGL <- tail(GOOGL$GOOGL.Close,n = 500)
  GOOGL <- c(min(GOOGL),max(GOOGL),mean(GOOGL)-2*sd(GOOGL),mean(GOOGL)+2*sd(GOOGL),
          GOOGL[hoy])
  
  quantmod::getSymbols("GRMN") 
  GRMN <- tail(GRMN$GRMN.Close,n = 500)
  GRMN <- c(min(GRMN),max(GRMN),mean(GRMN)-2*sd(GRMN),mean(GRMN)+2*sd(GRMN),
          GRMN[hoy])
  
  quantmod::getSymbols("GS") 
  GS <- tail(GS$GS.Close,n = 500)
  GS <- c(min(GS),max(GS),mean(GS)-2*sd(GS),mean(GS)+2*sd(GS),
          GS[hoy])
  
  quantmod::getSymbols("GSK") 
  GSK <- tail(GSK$GSK.Close,n = 500)
  GSK <- c(min(GSK),max(GSK),mean(GSK)-2*sd(GSK),mean(GSK)+2*sd(GSK),
          GSK[hoy])
  
  quantmod::getSymbols("HAL") 
  HAL <- tail(HAL$HAL.Close,n = 500)
  HAL <- c(min(HAL),max(HAL),mean(HAL)-2*sd(HAL),mean(HAL)+2*sd(HAL),
           HAL[hoy])
  
  quantmod::getSymbols("HDB") 
  HDB <- tail(HDB$HDB.Close,n = 500)
  HDB <- c(min(HDB),max(HDB),mean(HDB)-2*sd(HDB),mean(HDB)+2*sd(HDB),
           HDB[hoy])
  
  quantmod::getSymbols("HMC") 
  HMC <- tail(HMC$HMC.Close,n = 500)
  HMC <- c(min(HMC),max(HMC),mean(HMC)-2*sd(HMC),mean(HMC)+2*sd(HMC),
           HMC[hoy])
  
  quantmod::getSymbols("HOG") 
  HOG <- tail(HOG$HOG.Close,n = 500)
  HOG <- c(min(HOG),max(HOG),mean(HOG)-2*sd(HOG),mean(HOG)+2*sd(HOG),
           HOG[hoy])
  
  quantmod::getSymbols("HON") 
  HON <- tail(HON$HON.Close,n = 500)
  HON <- c(min(HON),max(HON),mean(HON)-2*sd(HON),mean(HON)+2*sd(HON),
           HON[hoy])
  
  quantmod::getSymbols("HOOD") 
  HOOD <- tail(HOOD$HOOD.Close,n = 500)
  HOOD <- c(min(HOOD),max(HOOD),mean(HOOD)-2*sd(HOOD),mean(HOOD)+2*sd(HOOD),
           HOOD[hoy])
  
  quantmod::getSymbols("HPQ") 
  HPQ <- tail(HPQ$HPQ.Close,n = 500)
  HPQ <- c(min(HPQ),max(HPQ),mean(HPQ)-2*sd(HPQ),mean(HPQ)+2*sd(HPQ),
           HPQ[hoy])
  
  quantmod::getSymbols("HSBC") 
  HSBC <- tail(HSBC$HSBC.Close,n = 500)
  HSBC <- c(min(HSBC),max(HSBC),mean(HSBC)-2*sd(HSBC),mean(HSBC)+2*sd(HSBC),
           HSBC[hoy])
  
  quantmod::getSymbols("HSY") 
  HSY <- tail(HSY$HSY.Close,n = 500)
  HSY <- c(min(HSY),max(HSY),mean(HSY)-2*sd(HSY),mean(HSY)+2*sd(HSY),
            HSY[hoy])
  
  quantmod::getSymbols("HUT") 
  HUT <- tail(HUT$HUT.Close,n = 500)
  HUT <- c(min(HUT),max(HUT),mean(HUT)-2*sd(HUT),mean(HUT)+2*sd(HUT),
           HUT[hoy])
  
  quantmod::getSymbols("IBB") 
  IBB <- tail(IBB$IBB.Close,n = 500)
  IBB <- c(min(IBB),max(IBB),mean(IBB)-2*sd(IBB),mean(IBB)+2*sd(IBB),
           IBB[hoy])
  
  quantmod::getSymbols("IBIT") 
  IBIT <- tail(IBIT$IBIT.Close,n = 500)
  IBIT <- c(min(IBIT),max(IBIT),mean(IBIT)-2*sd(IBIT),mean(IBIT)+2*sd(IBIT),
            IBIT[hoy])
  
  quantmod::getSymbols("IBM") 
  IBM <- tail(IBM$IBM.Close,n = 500)
  IBM <- c(min(IBM),max(IBM),mean(IBM)-2*sd(IBM),mean(IBM)+2*sd(IBM),
           IBM[hoy])
  
  quantmod::getSymbols("IEUR") 
  IEUR <- tail(IEUR$IEUR.Close,n = 500)
  IEUR <- c(min(IEUR),max(IEUR),mean(IEUR)-2*sd(IEUR),mean(IEUR)+2*sd(IEUR),
           IEUR[hoy])
  
  quantmod::getSymbols("ING") 
  ING <- tail(ING$ING.Close,n = 500)
  ING <- c(min(ING),max(ING),mean(ING)-2*sd(ING),mean(ING)+2*sd(ING),
           ING[hoy])
  
  quantmod::getSymbols("INTC") 
  INTC <- tail(INTC$INTC.Close,n = 500)
  INTC <- c(min(INTC),max(INTC),mean(INTC)-2*sd(INTC),mean(INTC)+2*sd(INTC),
            INTC[hoy])
  
  quantmod::getSymbols("ITUB") 
  ITUB <- tail(ITUB$ITUB.Close,n = 500)
  ITUB <- c(min(ITUB),max(ITUB),mean(ITUB)-2*sd(ITUB),mean(ITUB)+2*sd(ITUB),
            ITUB[hoy])
  
  quantmod::getSymbols("IVE") 
  IVE <- tail(IVE$IVE.Close,n = 500)
  IVE <- c(min(IVE),max(IVE),mean(IVE)-2*sd(IVE),mean(IVE)+2*sd(IVE),
           IVE[hoy])
  
  quantmod::getSymbols("IVW") 
  IVW <- tail(IVW$IVW.Close,n = 500)
  IVW <- c(min(IVW),max(IVW),mean(IVW)-2*sd(IVW),mean(IVW)+2*sd(IVW),
           IVW[hoy])
  
  quantmod::getSymbols("JCI") 
  JCI <- tail(JCI$JCI.Close,n = 500)
  JCI <- c(min(JCI),max(JCI),mean(JCI)-2*sd(JCI),mean(JCI)+2*sd(JCI),
            JCI[hoy])
  
  quantmod::getSymbols("JD") 
  JD <- tail(JD$JD.Close,n = 500)
  JD <- c(min(JD),max(JD),mean(JD)-2*sd(JD),mean(JD)+2*sd(JD),
            JD[hoy])
  
  quantmod::getSymbols("JMIA") 
  JMIA <- tail(JMIA$JMIA.Close,n = 500)
  JMIA <- c(min(JMIA),max(JMIA),mean(JMIA)-2*sd(JMIA),mean(JMIA)+2*sd(JMIA),
            JMIA[hoy])
  
  quantmod::getSymbols("JNJ") 
  JNJ <- tail(JNJ$JNJ.Close,n = 500)
  JNJ <- c(min(JNJ),max(JNJ),mean(JNJ)-2*sd(JNJ),mean(JNJ)+2*sd(JNJ),
          JNJ[hoy])
  
  quantmod::getSymbols("JPM") 
  JPM <- tail(JPM$JPM.Close,n = 500)
  JPM <- c(min(JPM),max(JPM),mean(JPM)-2*sd(JPM),mean(JPM)+2*sd(JPM),
          JPM[hoy])
  
  quantmod::getSymbols("KB") 
  KB <- tail(KB$KB.Close,n = 500)
  KB <- c(min(KB),max(KB),mean(KB)-2*sd(KB),mean(KB)+2*sd(KB),
           KB[hoy])
  
  quantmod::getSymbols("KEP") 
  KEP <- tail(KEP$KEP.Close,n = 500)
  KEP <- c(min(KEP),max(KEP),mean(KEP)-2*sd(KEP),mean(KEP)+2*sd(KEP),
          KEP[hoy])
  
  
  quantmod::getSymbols("KGC") 
  KGC <- tail(KGC$KGC.Close,n = 500)
  KGC <- c(min(KGC),max(KGC),mean(KGC)-2*sd(KGC),mean(KGC)+2*sd(KGC),
          KGC[hoy])
  
  
  quantmod::getSymbols("KMB") 
  KMB <- tail(KMB$KMB.Close,n = 500)
  KMB <- c(min(KMB),max(KMB),mean(KMB)-2*sd(KMB),mean(KMB)+2*sd(KMB),
          KMB[hoy])
  
  
  quantmod::getSymbols("KO") 
  KO <- tail(KO$KO.Close,n = 500)
  KO <- c(min(KO),max(KO),mean(KO)-2*sd(KO),mean(KO)+2*sd(KO),
          KO[hoy])
  
  quantmod::getSymbols("LAC") 
  LAC <- tail(LAC$LAC.Close,n = 500)
  LAC <- c(min(LAC),max(LAC),mean(LAC)-2*sd(LAC),mean(LAC)+2*sd(LAC),
          LAC[hoy])
  
  quantmod::getSymbols("LLY") 
  LLY <- tail(LLY$LLY.Close,n = 500)
  LLY <- c(min(LLY),max(LLY),mean(LLY)-2*sd(LLY),mean(LLY)+2*sd(LLY),
           LLY[hoy])
  
  quantmod::getSymbols("LMT") 
  LMT <- tail(LMT$LMT.Close,n = 500)
  LMT <- c(min(LMT),max(LMT),mean(LMT)-2*sd(LMT),mean(LMT)+2*sd(LMT),
           LMT[hoy])
  
  quantmod::getSymbols("LRCX") 
  LRCX <- tail(LRCX$LRCX.Close,n = 500)
  LRCX <- c(min(LRCX),max(LRCX),mean(LRCX)-2*sd(LRCX),mean(LRCX)+2*sd(LRCX),
           LRCX[hoy])
  
  quantmod::getSymbols("LVS") 
  LVS <- tail(LVS$LVS.Close,n = 500)
  LVS <- c(min(LVS),max(LVS),mean(LVS)-2*sd(LVS),mean(LVS)+2*sd(LVS),
           LVS[hoy])
  
  quantmod::getSymbols("LYG") 
  LYG <- tail(LYG$LYG.Close,n = 500)
  LYG <- c(min(LYG),max(LYG),mean(LYG)-2*sd(LYG),mean(LYG)+2*sd(LYG),
           LYG[hoy])
  
  quantmod::getSymbols("MA") 
  MA <- tail(MA$MA.Close,n = 500)
  MA <- c(min(MA),max(MA),mean(MA)-2*sd(MA),mean(MA)+2*sd(MA),
          MA[hoy])
  
  quantmod::getSymbols("MCD") 
  MCD <- tail(MCD$MCD.Close,n = 500)
  MCD <- c(min(MCD),max(MCD),mean(MCD)-2*sd(MCD),mean(MCD)+2*sd(MCD),
           MCD[hoy])
  
  quantmod::getSymbols("MELI") 
  MELI <- tail(MELI$MELI.Close,n = 500)
  MELI <- c(min(MELI),max(MELI),mean(MELI)-2*sd(MELI),mean(MELI)+2*sd(MELI),
           MELI[hoy])
  
  quantmod::getSymbols("META") 
  META <- tail(META$META.Close,n = 500)
  META <- c(min(META),max(META),mean(META)-2*sd(META),mean(META)+2*sd(META),
            META[hoy])
  
  quantmod::getSymbols("MMM") 
  MMM <- tail(MMM$MMM.Close,n = 500)
  MMM <- c(min(MMM),max(MMM),mean(MMM)-2*sd(MMM),mean(MMM)+2*sd(MMM),
          MMM[hoy])
  
  quantmod::getSymbols("MO") 
  MO <- tail(MO$MO.Close,n = 500)
  MO <- c(min(MO),max(MO),mean(MO)-2*sd(MO),mean(MO)+2*sd(MO),
          MO[hoy])
  
  quantmod::getSymbols("MSFT") 
  MSFT <- tail(MSFT$MSFT.Close,n = 500)
  MSFT <- c(min(MSFT),max(MSFT),mean(MSFT)-2*sd(MSFT),mean(MSFT)+2*sd(MSFT),
          MSFT[hoy])
  
  quantmod::getSymbols("MSI") 
  MSI <- tail(MSI$MSI.Close,n = 500)
  MSI <- c(min(MSI),max(MSI),mean(MSI)-2*sd(MSI),mean(MSI)+2*sd(MSI),
            MSI[hoy])
  
  quantmod::getSymbols("MSTR") 
  MSTR <- tail(MSTR$MSTR.Close,n = 500)
  MSTR <- c(min(MSTR),max(MSTR),mean(MSTR)-2*sd(MSTR),mean(MSTR)+2*sd(MSTR),
            MSTR[hoy])
  
  quantmod::getSymbols("MU") 
  MU <- tail(MU$MU.Close,n = 500)
  MU <- c(min(MU),max(MU),mean(MU)-2*sd(MU),mean(MU)+2*sd(MU),
            MU[hoy])
  
  quantmod::getSymbols("NEM") 
  NEM <- tail(NEM$NEM.Close,n = 500)
  NEM <- c(min(NEM),max(NEM),mean(NEM)-2*sd(NEM),mean(NEM)+2*sd(NEM),
           NEM[hoy])
  
  quantmod::getSymbols("NFLX") 
  NFLX <- tail(NFLX$NFLX.Close,n = 500)
  NFLX <- c(min(NFLX),max(NFLX),mean(NFLX)-2*sd(NFLX),mean(NFLX)+2*sd(NFLX),
           NFLX[hoy])
  
  quantmod::getSymbols("NG") 
  NG <- tail(NG$NG.Close,n = 500)
  NG <- c(min(NG),max(NG),mean(NG)-2*sd(NG),mean(NG)+2*sd(NG),
          NG[hoy])
  
  quantmod::getSymbols("NGG") 
  NGG <- tail(NGG$NGG.Close,n = 500)
  NGG <- c(min(NGG),max(NGG),mean(NGG)-2*sd(NGG),mean(NGG)+2*sd(NGG),
           NGG[hoy])
  
  quantmod::getSymbols("NIO") 
  NIO <- tail(NIO$NIO.Close,n = 500)
  NIO <- c(min(NIO),max(NIO),mean(NIO)-2*sd(NIO),mean(NIO)+2*sd(NIO),
           NIO[hoy])
  
  quantmod::getSymbols("NKE") 
  NKE <- tail(NKE$NKE.Close,n = 500)
  NKE <- c(min(NKE),max(NKE),mean(NKE)-2*sd(NKE),mean(NKE)+2*sd(NKE),
           NKE[hoy])
  
  quantmod::getSymbols("NMR") 
  NMR <- tail(NMR$NMR.Close,n = 500)
  NMR <- c(min(NMR),max(NMR),mean(NMR)-2*sd(NMR),mean(NMR)+2*sd(NMR),
           NMR[hoy])
  
  quantmod::getSymbols("NU") 
  NU <- tail(NU$NU.Close,n = 500)
  NU <- c(min(NU),max(NU),mean(NU)-2*sd(NU),mean(NU)+2*sd(NU),
            NU[hoy])
  
  quantmod::getSymbols("NVDA") 
  NVDA <- tail(NVDA$NVDA.Close,n = 500)
  NVDA <- c(min(NVDA),max(NVDA),mean(NVDA)-2*sd(NVDA),mean(NVDA)+2*sd(NVDA),
          NVDA[hoy])
  
  quantmod::getSymbols("ORCL") 
  ORCL <- tail(ORCL$ORCL.Close,n = 500)
  ORCL <- c(min(ORCL),max(ORCL),mean(ORCL)-2*sd(ORCL),mean(ORCL)+2*sd(ORCL),
           ORCL[hoy])
  
  quantmod::getSymbols("OXY") 
  OXY <- tail(OXY$OXY.Close,n = 500)
  OXY <- c(min(OXY),max(OXY),mean(OXY)-2*sd(OXY),mean(OXY)+2*sd(OXY),
            OXY[hoy])
  
  quantmod::getSymbols("PAAS") 
  PAAS <- tail(PAAS$PAAS.Close,n = 500)
  PAAS <- c(min(PAAS),max(PAAS),mean(PAAS)-2*sd(PAAS),mean(PAAS)+2*sd(PAAS),
            PAAS[hoy])
  
  quantmod::getSymbols("PAC") 
  PAC <- tail(PAC$PAC.Close,n = 500)
  PAC <- c(min(PAC),max(PAC),mean(PAC)-2*sd(PAC),mean(PAC)+2*sd(PAC),
           PAC[hoy])
  
  quantmod::getSymbols("PAGS") 
  PAGS <- tail(PAGS$PAGS.Close,n = 500)
  PAGS <- c(min(PAGS),max(PAGS),mean(PAGS)-2*sd(PAGS),mean(PAGS)+2*sd(PAGS),
            PAGS[hoy])
  
  quantmod::getSymbols("PANW") 
  PANW <- tail(PANW$PANW.Close,n = 500)
  PANW <- c(min(PANW),max(PANW),mean(PANW)-2*sd(PANW),mean(PANW)+2*sd(PANW),
            PANW[hoy])
  
  quantmod::getSymbols("PBI") 
  PBI <- tail(PBI$PBI.Close,n = 500)
  PBI <- c(min(PBI),max(PBI),mean(PBI)-2*sd(PBI),mean(PBI)+2*sd(PBI),
           PBI[hoy])
  
  quantmod::getSymbols("PBR") 
  PBR <- tail(PBR$PBR.Close,n = 500)
  PBR <- c(min(PBR),max(PBR),mean(PBR)-2*sd(PBR),mean(PBR)+2*sd(PBR),
           PBR[hoy])
  
  quantmod::getSymbols("PEP") 
  PEP <- tail(PEP$PEP.Close,n = 500)
  PEP <- c(min(PEP),max(PEP),mean(PEP)-2*sd(PEP),mean(PEP)+2*sd(PEP),
           PEP[hoy])
  
  quantmod::getSymbols("PFE") 
  PFE <- tail(PFE$PFE.Close,n = 500)
  PFE <- c(min(PFE),max(PFE),mean(PFE)-2*sd(PFE),mean(PFE)+2*sd(PFE),
           PFE[hoy])
  
  quantmod::getSymbols("PG") 
  PG <- tail(PG$PG.Close,n = 500)
  PG <- c(min(PG),max(PG),mean(PG)-2*sd(PG),mean(PG)+2*sd(PG),
          PG[hoy])
  
  quantmod::getSymbols("PLTR") 
  PLTR <- tail(PLTR$PLTR.Close,n = 500)
  PLTR <- c(min(PLTR),max(PLTR),mean(PLTR)-2*sd(PLTR),mean(PLTR)+2*sd(PLTR),
          PLTR[hoy])
  
  quantmod::getSymbols("PYPL") 
  PYPL <- tail(PYPL$PYPL.Close,n = 500)
  PYPL <- c(min(PYPL),max(PYPL),mean(PYPL)-2*sd(PYPL),mean(PYPL)+2*sd(PYPL),
           PYPL[hoy])
  
  quantmod::getSymbols("QCOM") 
  QCOM <- tail(QCOM$QCOM.Close,n = 500)
  QCOM <- c(min(QCOM),max(QCOM),mean(QCOM)-2*sd(QCOM),mean(QCOM)+2*sd(QCOM),
          QCOM[hoy])
  
  quantmod::getSymbols("QQQ") 
  QQQ <- tail(QQQ$QQQ.Close,n = 500)
  QQQ <- c(min(QQQ),max(QQQ),mean(QQQ)-2*sd(QQQ),mean(QQQ)+2*sd(QQQ),
          QQQ[hoy])
  
  quantmod::getSymbols("RBLX") 
  RBLX <- tail(RBLX$RBLX.Close,n = 500)
  RBLX <- c(min(RBLX),max(RBLX),mean(RBLX)-2*sd(RBLX),mean(RBLX)+2*sd(RBLX),
          RBLX[hoy])
  
  quantmod::getSymbols("RGTI") 
  RGTI <- tail(RGTI$RGTI.Close,n = 500)
  RGTI <- c(min(RGTI),max(RGTI),mean(RGTI)-2*sd(RGTI),mean(RGTI)+2*sd(RGTI),
          RGTI[hoy])
  
  quantmod::getSymbols("RIO") 
  RIO <- tail(RIO$RIO.Close,n = 500)
  RIO <- c(min(RIO),max(RIO),mean(RIO)-2*sd(RIO),mean(RIO)+2*sd(RIO),
          RIO[hoy])
  
  quantmod::getSymbols("ROST") 
  ROST <- tail(ROST$ROST.Close,n = 500)
  ROST <- c(min(ROST),max(ROST),mean(ROST)-2*sd(ROST),mean(ROST)+2*sd(ROST),
           ROST[hoy])
  
  quantmod::getSymbols("RTX") 
  RTX <- tail(RTX$RTX.Close,n = 500)
  RTX <- c(min(RTX),max(RTX),mean(RTX)-2*sd(RTX),mean(RTX)+2*sd(RTX),
           RTX[hoy])
  
  quantmod::getSymbols("SAP") 
  SAP <- tail(SAP$SAP.Close,n = 500)
  SAP <- c(min(SAP),max(SAP),mean(SAP)-2*sd(SAP),mean(SAP)+2*sd(SAP),
           SAP[hoy])
  
  quantmod::getSymbols("SATL") 
  SATL <- tail(SATL$SATL.Close,n = 500)
  SATL <- c(min(SATL),max(SATL),mean(SATL)-2*sd(SATL),mean(SATL)+2*sd(SATL),
            SATL[hoy])
  
  quantmod::getSymbols("SBUX") 
  SBUX <- tail(SBUX$SBUX.Close,n = 500)
  SBUX <- c(min(SBUX),max(SBUX),mean(SBUX)-2*sd(SBUX),mean(SBUX)+2*sd(SBUX),
            SBUX[hoy])
  
  quantmod::getSymbols("SH") 
  SH <- tail(SH$SH.Close,n = 500)
  SH <- c(min(SH),max(SH),mean(SH)-2*sd(SH),mean(SH)+2*sd(SH),
           SH[hoy])
  
  quantmod::getSymbols("SHOP") 
  SHOP <- tail(SHOP$SHOP.Close,n = 500)
  SHOP <- c(min(SHOP),max(SHOP),mean(SHOP)-2*sd(SHOP),mean(SHOP)+2*sd(SHOP),
          SHOP[hoy])
  
  quantmod::getSymbols("SONY") 
  SONY <- tail(SONY$SONY.Close,n = 500)
  SONY <- c(min(SONY),max(SONY),mean(SONY)-2*sd(SONY),mean(SONY)+2*sd(SONY),
            SONY[hoy])
  
  quantmod::getSymbols("SPGI") 
  SPGI <- tail(SPGI$SPGI.Close,n = 500)
  SPGI <- c(min(SPGI),max(SPGI),mean(SPGI)-2*sd(SPGI),mean(SPGI)+2*sd(SPGI),
          SPGI[hoy])
  
  quantmod::getSymbols("SPOT") 
  SPOT <- tail(SPOT$SPOT.Close,n = 500)
  SPOT <- c(min(SPOT),max(SPOT),mean(SPOT)-2*sd(SPOT),mean(SPOT)+2*sd(SPOT),
           SPOT[hoy])
  
  quantmod::getSymbols("SPY") 
  SPY <- tail(SPY$SPY.Close,n = 500)
  SPY <- c(min(SPY),max(SPY),mean(SPY)-2*sd(SPY),mean(SPY)+2*sd(SPY),
           SPY[hoy])
  
  quantmod::getSymbols("T") 
  T <- tail(T$T.Close,n = 500)
  T <- c(min(T),max(T),mean(T)-2*sd(T),mean(T)+2*sd(T),
           T[hoy])
  
  quantmod::getSymbols("TCOM") 
  TCOM <- tail(TCOM$TCOM.Close,n = 500)
  TCOM <- c(min(TCOM),max(TCOM),mean(TCOM)-2*sd(TCOM),mean(TCOM)+2*sd(TCOM),
            TCOM[hoy])
  
  quantmod::getSymbols("TEAM") 
  TEAM <- tail(TEAM$TEAM.Close,n = 500)
  TEAM <- c(min(TEAM),max(TEAM),mean(TEAM)-2*sd(TEAM),mean(TEAM)+2*sd(TEAM),
            TEAM[hoy])
  
  quantmod::getSymbols("TEN") 
  TEN <- tail(TEN$TEN.Close,n = 500)
  TEN <- c(min(TEN),max(TEN),mean(TEN)-2*sd(TEN),mean(TEN)+2*sd(TEN),
         TEN[hoy])
  
  quantmod::getSymbols("TGT") 
  TGT <- tail(TGT$TGT.Close,n = 500)
  TGT <- c(min(TGT),max(TGT),mean(TGT)-2*sd(TGT),mean(TGT)+2*sd(TGT),
           TGT[hoy])
  
  quantmod::getSymbols("TM") 
  TM <- tail(TM$TM.Close,n = 500)
  TM <- c(min(TM),max(TM),mean(TM)-2*sd(TM),mean(TM)+2*sd(TM),
          TM[hoy])
  
  quantmod::getSymbols("TRIP") 
  TRIP <- tail(TRIP$TRIP.Close,n = 500)
  TRIP <- c(min(TRIP),max(TRIP),mean(TRIP)-2*sd(TRIP),mean(TRIP)+2*sd(TRIP),
            TRIP[hoy])
  
  quantmod::getSymbols("TSLA") 
  TSLA <- tail(TSLA$TSLA.Close,n = 500)
  TSLA <- c(min(TSLA),max(TSLA),mean(TSLA)-2*sd(TSLA),mean(TSLA)+2*sd(TSLA),
         TSLA[hoy])
  
  quantmod::getSymbols("TSM") 
  TSM <- tail(TSM$TSM.Close,n = 500)
  TSM <- c(min(TSM),max(TSM),mean(TSM)-2*sd(TSM),mean(TSM)+2*sd(TSM),
         TSM[hoy])
  
  quantmod::getSymbols("TWLO") 
  TWLO <- tail(TWLO$TWLO.Close,n = 500)
  TWLO <- c(min(TWLO),max(TWLO),mean(TWLO)-2*sd(TWLO),mean(TWLO)+2*sd(TWLO),
           TWLO[hoy])
  
  quantmod::getSymbols("UAL") 
  UAL <- tail(UAL$UAL.Close,n = 500)
  UAL <- c(min(UAL),max(UAL),mean(UAL)-2*sd(UAL),mean(UAL)+2*sd(UAL),
           UAL[hoy])
  
  quantmod::getSymbols("UBER") 
  UBER <- tail(UBER$UBER.Close,n = 500)
  UBER <- c(min(UBER),max(UBER),mean(UBER)-2*sd(UBER),mean(UBER)+2*sd(UBER),
         UBER[hoy])
  
  quantmod::getSymbols("UGP") 
  UGP <- tail(UGP$UGP.Close,n = 500)
  UGP <- c(min(UGP),max(UGP),mean(UGP)-2*sd(UGP),mean(UGP)+2*sd(UGP),
           UGP[hoy])
  
  quantmod::getSymbols("UL") 
  UL <- tail(UL$UL.Close,n = 500)
  UL <- c(min(UL),max(UL),mean(UL)-2*sd(UL),mean(UL)+2*sd(UL),
          UL[hoy])
  
  quantmod::getSymbols("UNH") 
  UNH <- tail(UNH$UNH.Close,n = 500)
  UNH <- c(min(UNH),max(UNH),mean(UNH)-2*sd(UNH),mean(UNH)+2*sd(UNH),
           UNH[hoy])
  
  quantmod::getSymbols("UNP") 
  UNP <- tail(UNP$UNP.Close,n = 500)
  UNP <- c(min(UNP),max(UNP),mean(UNP)-2*sd(UNP),mean(UNP)+2*sd(UNP),
           UNP[hoy])
  
  quantmod::getSymbols("URBN") 
  URBN <- tail(URBN$URBN.Close,n = 500)
  URBN <- c(min(URBN),max(URBN),mean(URBN)-2*sd(URBN),mean(URBN)+2*sd(URBN),
            URBN[hoy])
  
  quantmod::getSymbols("USB") 
  USB <- tail(USB$USB.Close,n = 500)
  USB <- c(min(USB),max(USB),mean(USB)-2*sd(USB),mean(USB)+2*sd(USB),
           USB[hoy])
  
  quantmod::getSymbols("V") 
  V <- tail(V$V.Close,n = 500)
  V <- c(min(V),max(V),mean(V)-2*sd(V),mean(V)+2*sd(V),
         V[hoy])
  
  quantmod::getSymbols("VALE") 
  VALE <- tail(VALE$VALE.Close,n = 500)
  VALE <- c(min(VALE),max(VALE),mean(VALE)-2*sd(VALE),mean(VALE)+2*sd(VALE),
         VALE[hoy])
  
  quantmod::getSymbols("VEA") 
  VEA <- tail(VEA$VEA.Close,n = 500)
  VEA <- c(min(VEA),max(VEA),mean(VEA)-2*sd(VEA),mean(VEA)+2*sd(VEA),
         VEA[hoy])
  
  quantmod::getSymbols("VIST") 
  VIST <- tail(VIST$VIST.Close,n = 500)
  VIST <- c(min(VIST),max(VIST),mean(VIST)-2*sd(VIST),mean(VIST)+2*sd(VIST),
         VIST[hoy])
  
  quantmod::getSymbols("VIV") 
  VIV <- tail(VIV$VIV.Close,n = 500)
  VIV <- c(min(VIV),max(VIV),mean(VIV)-2*sd(VIV),mean(VIV)+2*sd(VIV),
           VIV[hoy])
  
  quantmod::getSymbols("VOD") 
  VOD <- tail(VOD$VOD.Close,n = 500)
  VOD <- c(min(VOD),max(VOD),mean(VOD)-2*sd(VOD),mean(VOD)+2*sd(VOD),
           VOD[hoy])
  
  quantmod::getSymbols("VRSN") 
  VRSN <- tail(VRSN$VRSN.Close,n = 500)
  VRSN <- c(min(VRSN),max(VRSN),mean(VRSN)-2*sd(VRSN),mean(VRSN)+2*sd(VRSN),
            VRSN[hoy])
  
  quantmod::getSymbols("VRTX") 
  VRTX <- tail(VRTX$VRTX.Close,n = 500)
  VRTX <- c(min(VRTX),max(VRTX),mean(VRTX)-2*sd(VRTX),mean(VRTX)+2*sd(VRTX),
            VRTX[hoy])
  
  quantmod::getSymbols("VST") 
  VST <- tail(VST$VST.Close,n = 500)
  VST <- c(min(VST),max(VST),mean(VST)-2*sd(VST),mean(VST)+2*sd(VST),
           VST[hoy])
  
  quantmod::getSymbols("VZ") 
  VZ <- tail(VZ$VZ.Close,n = 500)
  VZ <- c(min(VZ),max(VZ),mean(VZ)-2*sd(VZ),mean(VZ)+2*sd(VZ),
          VZ[hoy])
  
  quantmod::getSymbols("WFC") 
  WFC <- tail(WFC$WFC.Close,n = 500)
  WFC <- c(min(WFC),max(WFC),mean(WFC)-2*sd(WFC),mean(WFC)+2*sd(WFC),
           WFC[hoy])
  
  quantmod::getSymbols("WMT") 
  WMT <- tail(WMT$WMT.Close,n = 500)
  WMT <- c(min(WMT),max(WMT),mean(WMT)-2*sd(WMT),mean(WMT)+2*sd(WMT),
         WMT[hoy])
  
  quantmod::getSymbols("XP") 
  XP <- tail(XP$XP.Close,n = 500)
  XP <- c(min(XP),max(XP),mean(XP)-2*sd(XP),mean(XP)+2*sd(XP),
          XP[hoy])
  
  quantmod::getSymbols("XYZ") 
  XYZ <- tail(XYZ$XYZ.Close,n = 500)
  XYZ <- c(min(XYZ),max(XYZ),mean(XYZ)-2*sd(XYZ),mean(XYZ)+2*sd(XYZ),
         XYZ[hoy])
  
  quantmod::getSymbols("YELP") 
  YELP <- tail(YELP$YELP.Close,n = 500)
  YELP <- c(min(YELP),max(YELP),mean(YELP)-2*sd(YELP),mean(YELP)+2*sd(YELP),
            YELP[hoy])
  
  quantmod::getSymbols("ZM") 
  ZM <- tail(ZM$ZM.Close,n = 500)
  ZM <- c(min(ZM),max(ZM),mean(ZM)-2*sd(ZM),mean(ZM)+2*sd(ZM),
           ZM[hoy])
  
  
  planilla <- as.data.frame(rbind(AAL,AAPL,ABNB,ACN,ADBE,AIG,AMD,AMZN,ANF,ARKK,AXP,
                                B,BA,BABA,BAC,BB,BCS,BIDU,BIIB,BIOX,BITF,
                                C,CAT,CL,CLS,COIN,COST,CRM,CSCO,CVX,CX,
                                DD,DE,DECK,DEO,DIA,DOCU,DOW,
                                E,EA,EBAY,EEM,EFX,ERIC,ETHA,EWZ,
                                F,FCX,FDX,FMCC,FMX,FNMA,FSLR,FXI,
                                GE,GGB,GLD,GLOB,GM,GOOGL,GRMN,GS,GSK,
                                HAL,HDB,HMC,HOG,HON,HOOD,HPQ,HSBC,HSY,HUT,
                                IBB,IBIT,IBM,IEUR,ING,INTC,ITUB,IVE,IVW,
                                JCI,JD,JMIA,JNJ,JPM,KB,KEP,KGC,KMB,KO,
                                LAC,LLY,LMT,LRCX,LVS,LYG,
                                MA,MCD,MELI,META,MMM,MO,MSFT,MSI,MSTR,MU,
                                NEM,NFLX,NG,NGG,NIO,NKE,NMR,NU,NVDA,
                                ORCL,OXY,
                                PAAS,PAC,PAGS,PANW,PBI,PBR,PEP,PFE,PG,PLTR,PYPL,
                                QCOM,QQQ,RGTI,RIO,ROST,RTX,
                                SAP,SATL,SBUX,SH,SHOP,SONY,SPGI,SPOT,SPY,
                                T,TCOM,TEN,TGT,TM,TRIP,TSLA,TSM,TWLO,
                                UBER,UAL,UGP,UL,UNH,UNP,URBN,USB,
                                V,VALE,VEA,VIST,VIV,VOD,VRSN,VRTX,VST,VZ,
                                WFC,WMT,XP,XYZ,YELP,ZM)) %>% 
  mutate(Min=V1,Max=V2,LI=V3,LS=V4,Hoy=V5) %>% select(-V1,-V2,-V3,-V4,-V5)
  return(planilla)
}

planilla = actualizar_precios(hoy)


filter(planilla,Hoy<LI)
filter(planilla,Hoy<0.95*LI)
  
filter(planilla,Hoy>LS)
