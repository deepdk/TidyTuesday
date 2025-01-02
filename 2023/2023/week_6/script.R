library(quantmod)

# get the Amazon Stock data
amazon<-getSymbols("AMZN", 
           from = "2010-01-01", 
           to = "2022-12-29",
           periodicity = "daily",
           auto.assign=FALSE)
chartSeries(amazon, name="Amazon Stock Prices",up.col = "black", dn.col = "#FF8C00",theme="white")+
  theme_minimal()
  
chartSeries(amazon,up.col = "black", dn.col = "#FF8C00", subset = "last 3 months",theme="white")  

# Added the title using figma
