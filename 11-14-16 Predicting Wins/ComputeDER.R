ComputeDER = function(df){
  
  AB = sum(df$BFP, na.rm = TRUE)
  BB = sum(df$BB, na.rm = TRUE)
  HBP = sum(df$HBP, na.rm = TRUE)
  SH = sum(df$SH, na.rm = TRUE)
  SF = sum(df$SF, na.rm = TRUE)
  PA = AB + BB + HBP + SH + SF
  
  H = sum(df$H, na.rm = TRUE)
  HR = sum(df$HR,na.rm = TRUE)
  SO = sum(df$SO, na.rm = TRUE)
  RA = sum(df$R, na.rm = TRUE)
  
  DER = 1 - ((H - HR)/(AB - BB - SO - HBP - HR))

  c(DER = DER,RA = RA)
  }