#removing NA values from a data frame
head(airquality,20)
mean(airquality$Ozone)
# removing NA values and calculating mean
good<-complete.cases(airquality)
mean(airquality[good,"Ozone"])

