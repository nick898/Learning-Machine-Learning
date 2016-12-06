AgeEqPlayer = function(plid_age, df){
  # Given a player's age and a data frame of player season data this function computes
  # the career statistics of all players of the same age
  
  df = subset(df, age <= plid_age)
  Pid_Unique = unique(df$playerID)
  R = length(Pid_Unique)
  pb <- txtProgressBar(min = 0, max = R, style = 3)
  PermDF = matrix(nrow = R, ncol = 15)
  
  
  for (i in 1:R){
    
    pid = Pid_Unique[i]
    TempDF = subset(df, playerID == pid)
    W = sum(TempDF$W,na.rm = TRUE)
    L = sum(TempDF$L,na.rm = TRUE)
    G = sum(TempDF$G,na.rm = TRUE)
    GS = sum(TempDF$GS,na.rm = TRUE)
    CG = sum(TempDF$CG,na.rm = TRUE)
    SHO = sum(TempDF$SHO,na.rm = TRUE)
    SV = sum(TempDF$SV,na.rm = TRUE)
    IPouts = sum(TempDF$IPouts,na.rm = TRUE)
    H = sum(TempDF$H,na.rm = TRUE)
    BFP = sum(TempDF$BFP,na.rm = TRUE)
    ER = sum(TempDF$ER,na.rm = TRUE)
    HR = sum(TempDF$HR,na.rm = TRUE)
    BB = sum(TempDF$BB,na.rm = TRUE)
    SO = sum(TempDF$SO,na.rm = TRUE)
    BAOpp =round(H/BFP,3)
    ERA =round(ER/(IPouts/3)*9,2)
    IBB = sum(TempDF$IBB,na.rm = TRUE)
    WP = sum(TempDF$WP,na.rm = TRUE)
    HBP = sum(TempDF$HBP,na.rm = TRUE)
    BK = sum(TempDF$BK,na.rm = TRUE)
    GF = sum(TempDF$GF,na.rm = TRUE)
    R = sum(TempDF$R,na.rm = TRUE)
    SH = sum(TempDF$SH,na.rm = TRUE)
    SF = sum(TempDF$SF,na.rm = TRUE)
    GIDP = sum(TempDF$GIDP,na.rm = TRUE)
    TempDF2 = data.frame(playerID = pid, W = W, L = L, G = G, GS = GS, CG = CG, sHO = SHO, SV = SV, IPouts = IPouts, H = H, ER = ER, HR = HR, BB = BB, SO = SO, BAOpp = BAOpp, ERA = ERA, IBB = IBB, WP = WP, HBP = HBP, BK = BK, BFP = BFP, GF = GF, R = R, SH = SH, SF = SF, GIDP = GIDP)
    setTxtProgressBar(pb, i)
    
    if (i == 1){
      PermDF = TempDF2
    }else{
      PermDF = rbind(PermDF,TempDF2)
    }
    
  }
  PermDF = subset(PermDF, G > 0)
  rownames(PermDF) = PermDF$playerID
  
  close(pb) 
  return(PermDF)
}