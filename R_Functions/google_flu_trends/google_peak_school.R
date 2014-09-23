google_peak_school <- function(year=6, plot = FALSE) {
  google_years <- c("2004-2005", "2005-2006", "2006-2007", "2007-2008", "2008-2009", 
                    "2009-2010", "2010-2011", "2011-2012", "2012-2013", "2013-2014")
source("~/garrett/R_Functions/google_flu_trends/find_google_peak_week.R")
source("~/garrett/R_Functions/get_median_school_start_dates.R")
source("~/garrett/R_Functions/find_hhs_region.R")
require(Hmisc)
data$state <- gsub("_", " ", data$state)
data <- cbind(data, state_abb=NA)
for( i in 1:nrow(data)) {
  index <- grep(data$state[i],state.name,ignore.case=TRUE)
  if ( length(index) > 1 ) {
    first_length <- nchar(state.name[index[1]])
    second_length <- nchar(state.name[index[2]])
    if ( first_length > second_length )
      index <- index[2]
    else index <- index[1]
  }
  state_abb <- state.abb[index]
  if ( length(state_abb)==0 & data$state[i]=="district columbia") state_abb <- "DC" 
  data$state_abb[i] <- state_abb
}
color <- find_hhs_region(data$state_abb)
data <- cbind(data, color=color, stringsAsFactors = FALSE)
a <- find_google_peak_week(year-1)
names(a) <- gsub("\\.", " ", names(a))
b <- unlist(unname(a))
peak_school <- data.frame(name = names(a), peak_date = b, school_start = as.Date(NA), color = NA, stringsAsFactors = FALSE)
remove_rows <- integer(0)
for ( i in 1:nrow(peak_school)) {
state_peak <- substr(peak_school$name[i],(nchar(peak_school$name[i])-1),nchar(peak_school$name[i]))
if ( state_peak == "AK" | state_peak == "HI" ) remove_rows <- append(remove_rows, i )
else {
index <- which(data$state_abb == state_peak)
peak_school$school_start[i] <- as.Date(data$start_date[index])
peak_school$color[i] <- as.character(data$color[index])
}

peak_school$peak_date <- as.Date(peak_school$peak_date, "%m/%d/%y")
}
if ( length(remove_rows) != 0 ) peak_school <- peak_school[-1*remove_rows,]

if (plot) {
par(pty = "s")
plot(y = peak_school$peak_date, x = peak_school$school_start, ylab = "Date of Peak Week", xlab = "School Start Date", 
      col = peak_school$color, pch = 20)
abline(lm(formula = peak_date ~ school_start, data = peak_school))
abline(lm(formula = peak_date ~ school_start, data = peak_school[which(peak_school$color=="red"), ]), col = "red")
corr_black <- rcorr(x = as.numeric(peak_school$school_start), y = as.numeric(peak_school$peak_date), type="pearson")
corr_red <- rcorr(x = as.numeric(peak_school$school_start[which(peak_school$color=="red")]), 
                  y = as.numeric(peak_school$peak_date[which(peak_school$color=="red")]), type="pearson")
title <- paste("Peak Date of", google_years[year], "Against School Start Date
Black:", round(corr_black$r[2],2),", p ~", signif(corr_black$P[2],1), "      Red:", round(corr_red$r[2],2),
               ", p ~", signif(corr_red$P[2],1))
title(main=title)
}

return(peak_school)
}