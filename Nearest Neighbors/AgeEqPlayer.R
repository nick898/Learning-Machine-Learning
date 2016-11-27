AgeEqPlayer = function(plid_age, df){
  
  df = subset(df, age <= plid_age)
  Pid_Unique = unique(df$playerID)
  R = length(Pid_Unique)
  pb <- txtProgressBar(min = 0, max = R, style = 3)
  PermDF = matrix(nrow = R, ncol = 15)
  
  
  for (i in 1:R){
    
    pid = Pid_Unique[i]
    TempDF = subset(df, playerID == pid)
    G = sum(TempDF$G,na.rm = TRUE)
    AB = sum(TempDF$AB,na.rm = TRUE)
    R = sum(TempDF$R,na.rm = TRUE)
    H = sum(TempDF$H,na.rm = TRUE)
    X2B = sum(TempDF$X2B,na.rm = TRUE)
    X3B = sum(TempDF$X3B,na.rm = TRUE)
    HR = sum(TempDF$HR,na.rm = TRUE)
    RBI = sum(TempDF$RBI,na.rm = TRUE)
    SB = sum(TempDF$SB,na.rm = TRUE)
    BB = sum(TempDF$BB,na.rm = TRUE)
    SO = sum(TempDF$SO,na.rm = TRUE)
    X1B = H - X2B - X3B - HR
    AVG = round(H/AB,3)
    SLG = round((X1B + 2*X2B + 3*X3B + 4*HR)/AB,3)
    TempDF2 = data.frame(playerID = pid, G = G, AB = AB, R = R, H = H, x1B = X1B, X2B = X2B, X3B = X3B, HR = HR, RBI = RBI, SB = SB, BB = BB, SO = SO, AVG = AVG, SLG = SLG)
    setTxtProgressBar(pb, i)
    
    if (i == 1){
      PermDF = TempDF2
    }else{
      PermDF = rbind(PermDF,TempDF2)
    }

  }
  PermDF = subset(PermDF, AB > 0)
  rownames(PermDF) = PermDF$playerID
    
  close(pb) 
  return(PermDF)
}