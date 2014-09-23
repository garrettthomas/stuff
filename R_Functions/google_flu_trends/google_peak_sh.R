google_peak_sh <- function(year=6, plot = FALSE) {
  source("~/garrett/R_Functions/google_flu_trends/find_google_peak_week.R")
  source("~/garrett/R_Functions/find_hhs_region.R")
  require(maps)
  require(Hmisc)
  
  
  years = c("2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014" )
  a <- find_google_peak_week(year-1)
  names(a) <- gsub("\\.", " ", names(a))
  a <- a[,order(names(a))]
data(us.cities)
par(mfrow = c(1, 1))
b <- subset(us.cities, is.element(name,names(a)))
b <- cbind(b, peak_date = as.Date(unname(unlist(a)), "%m/%d/%y"))
b <- na.omit(b)
remove_rows <- which(b$country.etc=="AK" | b$country.etc=="HI")
b <- b[-1*remove_rows,]
b <- cbind(b, color = find_hhs_region(b[ ,2]))
b <- cbind(b, year = as.character(format(b$peak_date, "%Y")))
b <- cbind(b, days = as.integer(b$peak_date - as.Date(paste("01/01/", b$year, sep = ""), "%m/%d/%Y") + 1))
b <- cbind(b, lon_approx = NA, lat_approx = NA)

rdata_file <- paste("~/Dropbox/LEPR01/humidity_data/SH-NLDAS-2-", years[year], ".RData", sep="")
load(rdata_file)

SH <- w[[3]]

for ( i in 1:nrow(b)) {
  lon_approx <- which.min(abs(w[[1]] - b$long[i]))
  b$lon_approx[i] <- w[[1]][lon_approx]
                          
  lat_approx <- which.min(abs(w[[2]] - b$lat[i]))
  b$lat_approx[i] <- w[[2]][lat_approx]
}

b <- cbind(b, sh = NA)
index_first_year <- which(b$year==years[year])
for ( i in index_first_year ) {
  lon <- which(w$lon == b$lon_approx[i])
  lat <- which(w$lat == b$lat_approx[i])
  days <- which(w$days == b$days[i])
  b[i,13] <- sum(SH[lon, lat, days - c(7,8,9,10)])/4
}

index_first_year_modified <- which(b$days < 8)
for ( i in index_first_year_modified ) {
  lon <- which(w$lon == b$lon_approx[i])
  lat <- which(w$lat == b$lat_approx[i])
  days <- which(w$days == b$days[i] - 7 + dim(SH)[3] )
  b[i,13] <- sum(SH[lon, lat, days - c(0,1,2,3)])/4
}

index_over_lap <- which( 7 < b$days & b$days < 11 )

if (length(index_over_lap) == 0 ) {
  rdata_file <- paste("~/Dropbox/LEPR01/humidity_data/SH-NLDAS-2-", years[year+1], ".RData", sep="")
  load(rdata_file)
}
if (length(index_over_lap) != 0 ) {
  humidity_readings <- data.frame(name = b$name[index_over_lap], read1 = NA, read2 = NA,read3 = NA,read4 = NA)

  for ( i in index_over_lap ) {
    max_day_back <- b$days[i] - 10
    days_back <- seq(from = max_day_back, to = 0, by = 1)
    lon <- which(w$lon == b$lon_approx[i])
    lat <- which(w$lat == b$lat_approx[i])
    days <- which(w$days == max_day_back + dim(SH)[3])
    humidity_readings[which(humidity_readings$name==b$name[i]),2:(length(days_back)+1)] <- SH[lon, lat, days + days_back]
  }
  rdata_file <- paste("~/Dropbox/LEPR01/humidity_data/SH-NLDAS-2-", years[year+1], ".RData", sep="")
  load(rdata_file)
  SH <- w[[3]]
  for ( i in index_over_lap ) {
    max_day_foward <- b$days[i] - 7
    lon <- which(w$lon == b$lon_approx[i])
    lat <- which(w$lat == b$lat_approx[i])
    days_forward <- seq(max_day_foward,0, by=-1)
    days <- which(w$days == max_day_foward)
    humidity_readings[which(humidity_readings$name==b$name[i]),(6-length(days_forward)):5] <- SH[lon, lat, days - days_forward]
  }
  for ( i in index_over_lap) {
    b[i,13] <- sum(humidity_readings[which(humidity_readings$name==b$name[i]),2:5])/4
  }

}

index_second_year <- which(b$year==years[year+1] & b$days > 10)



#index_second_year <- subset( index_second_year, !is.element(index_second_year, index_over_lap))
for ( i in index_second_year ) {
  lon <- which(w$lon == b$lon_approx[i])
  lat <- which(w$lat == b$lat_approx[i])
  days <- which(w$days == b$days[i])
  b[i,13] <- sum(SH[lon, lat, days - c(7,8,9,10)])/4
}

answer <- data.frame(name = b$name, peak_date = b$peak_date, sh = b$sh, color = b$color)


if ( plot ) {
par(pty = "s")
plot(x = b$peak_date, y = b$sh , ylab = "Specific Humidity", xlab = "Date of Peak Week", 
    col = b$color, pch = 20)
abline(lm(formula = sh ~ peak_date, data = b))
abline(lm(formula = sh ~ peak_date, data = b[which(b$color=="red"), ]), col = "red")
corr_black <- rcorr(x = as.numeric(as.Date(b$peak_date,"%m/%d/%y")), y = b$sh, type="pearson")
corr_red <- rcorr(x = as.numeric(as.Date(b$peak_date[which(b$color=="red")],"%m/%d/%y")), 
                  y = b$sh[which(b$color=="red")], type="pearson")
title <- paste("Peak Date of", years[year], "-", years[year+1], "Against Specific Humidity
Black:", round(corr_black$r[2],2),", p ~", signif(corr_black$P[2],1), "      Red:", round(corr_red$r[2],2),
               ", p ~", signif(corr_red$P[2],1))
title(main=title)
}
return(answer)
}