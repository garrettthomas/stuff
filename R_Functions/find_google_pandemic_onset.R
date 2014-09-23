gft_ili_ratio <- read.csv("~/Desktop/gft_ili_percent.csv", check.names = FALSE, stringsAsFactors = FALSE)
cities = read.csv(file = "~/GFTvsILINET/DATA/google_city_data.csv", header = TRUE, check.names = FALSE, stringsAsFactors=FALSE)
cities[,1] <- as.Date(cities[,1], "%m/%d/%y")

gft_ili_ratio[,1] <- as.Date(gft_ili_ratio[,1], "%m/%d/%y")
low_ili_index <- which(gft_ili_ratio[,2] < 2 & gft_ili_ratio[,1] < as.Date("3/1/10", "%m/%d/%y") & gft_ili_ratio[,1] > as.Date("3/1/06", "%m/%d/%y"))
low_dates <- gft_ili_ratio[low_ili_index,1]
low_numeric <- as.numeric(low_dates)
time_index <-  which(gft_ili_ratio[,1] < as.Date("3/1/10", "%m/%d/%y") & gft_ili_ratio[,1] > as.Date("3/1/06", "%m/%d/%y"))
time_dates <- gft_ili_ratio[time_index,1]
time_numeric <- as.numeric(time_dates)
time_index_2009 <- which(gft_ili_ratio[,1] < as.Date("1/1/10", "%m/%d/%y") & gft_ili_ratio[,1] > as.Date("8/1/09", "%m/%d/%y"))
time_dates_2009 <- gft_ili_ratio[time_index_2009,1]
time_numeric_2009 <- as.numeric(time_dates_2009)
frequency <- (2*pi)/365
model <- lm(gft_ili_ratio[low_ili_index,2] ~ cos(frequency*low_numeric) + sin(frequency*low_numeric))
phase <- acos(model$coefficients[[3]]/sqrt(model$coefficients[[3]]^2 + model$coefficients[[2]]^2))

#plot(x=gft_ili_ratio[time_index,1],y=gft_ili_ratio[time_index,2], pch = 20)
#predicted <- model$coefficients[[1]] + model$coefficients[[2]]*cos(((2*pi)/365)*time_values) + 
  #model$coefficients[[3]]*sin(((2*pi)/365)*time_values)
#lines(time_values,predicted)
onset_dates <- data.frame(name = names(cities)[2:ncol(cities)], onset_date = as.Date(NA), stringsAsFactors = FALSE)
for ( i in 54:150) {
  
 # gft_ili_ratio[time_index,i]
for ( qq in time_index ) {
  ave <- ((gft_ili_ratio[time_index,i]+gft_ili_ratio[time_index-1,i]+gft_ili_ratio[time_index+1,i])/3)
}
  plot(x=time_dates,y=ave,
       type='l', main = names(gft_ili_ratio)[i])
low_first <- low_numeric
low_second <- low_numeric^2
low_third <- low_numeric^3
low_fourth <- low_numeric^4

time_first <- time_numeric
time_second <- time_numeric^2
time_third <- time_numeric^3
time_fourth <- time_numeric^4


model <- lm(gft_ili_ratio[low_ili_index,i] ~ low_first + low_second + low_third + low_fourth + sin(frequency*low_first + phase))
predicted <- model$coefficients[[1]] + model$coefficients[[2]]*time_first + 
  model$coefficients[[3]]*time_second + model$coefficients[[4]]*time_third +
  model$coefficients[[5]]*time_fourth + model$coefficients[[6]]*sin(frequency * time_first + phase)
threshold <- max(predicted[180:length(predicted)])*1.02
lines(x=time_dates,y=predicted,type='l')
abline(h = threshold, col = "blue")
for ( j in time_dates_2009 ) {
 #print(j)
  ili_index <- which(time_dates == j )
  google_cities_index <- which(cities[,1] == j)
  date_index <- which(gft_ili_ratio[,1] == j)
  #names <- names(cities)[2:ncol(cities)]
  #for ( z in 1:length(names)) {
  #  names[z] <- substring(names[z], 1, nchar(names[z])-3)
 # }
  
  if ( ave[ili_index - 2] > threshold & 
         ave[ili_index - 1] > threshold & ave[ili_index] > threshold) {
    n <- (100*cities[google_cities_index, i - 52]/ave[ili_index])
    binom_one <- binom.test( cities[google_cities_index, i - 52], round(n),
                             threshold/100)
    binom_two <- binom.test( cities[google_cities_index-1, i - 52], round(100*cities[google_cities_index-1, i - 52]/ave[ili_index-1]),
                             threshold/100)
    binom_three <- binom.test( cities[google_cities_index-2, i - 52], round(100*cities[google_cities_index-2, i - 52]/ave[ili_index-2]),
                               threshold/100)
    if ( binom_one$p.value < 0.01 & binom_two$p.value < 0.01 & binom_three$p.value < 0.01 ) {
      onset_dates[(i-53),2] <- gft_ili_ratio[date_index-2,1]
      
      break
    }
  }
}
if(!is.na(onset_dates[i-53,2])) {
points(onset_dates[i-53,2], ave[ili_index-2], col = "red")
}
#lines(x=time_dates,y=predicted,type='l')
}
  
  