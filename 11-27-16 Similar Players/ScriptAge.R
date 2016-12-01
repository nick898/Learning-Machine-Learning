R = nrow(T)

AgeV = rep(0,R)

for (i in 1:R){
  
  AgeV[i] = age(as.Date(paste(T$birthYear[i],T$birthMonth[i],T$birthDay[i],sep = "-")),as.Date(paste(T$yearID[i],"07","01",sep = "-")))
  
}