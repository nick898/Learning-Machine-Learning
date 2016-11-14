ComputeOPS = function(df){
  
  AB = sum(df$AB, na.rm = TRUE)
  H = sum(df$H, na.rm = TRUE)
  X2B = sum(df$X2B,na.rm = TRUE)
  X3B = sum(df$X3B,na.rm = TRUE)
  HR = sum(df$HR,na.rm = TRUE)
  X1B = H - X2B - X3B - HR
  TB = X1B + 2*X2B + 3*X3B + 4*HR
  BB = sum(df$BB, na.rm = TRUE)
  HBP = sum(df$HBP, na.rm = TRUE)
  SF = sum(df$SF, na.rm = TRUE)

  SLG = TB/AB
  
  OBP = (H + BB + HBP)/(AB + BB + HBP + SF)
  
  #OPS = (AB*(H + BB + HBP) + TB*(AB + BB + SF + HBP))/(AB*(AB + BB + SF + HBP))
  OPS = SLG + OBP
  
  OPS
  
    
}