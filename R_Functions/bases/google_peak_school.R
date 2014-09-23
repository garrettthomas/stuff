google_peak_school <- function(year = 6, plot = FALSE) {
require(mapdata)
require(ggmap)
require(Imap)
require(Hmisc)
source("~/GFTvsILINET/R funtions/find_peak_week_bases.R")
source("~/GFTvsILINET/R funtions/find_hhs_region.R")
data(us.cities)
par(mfrow = c(1, 1))
a <- find_peak_week_bases()
# a <- data.frame(zip = as.integer(names(data)), peak_date = as.Date(unlist(unname(data[10,]))))
a[, c(3:5)] <- NA
names(a)[c(3:5)] <- c("long", "lat", "state")
location_data_frame <- read.csv("~/Dropbox/LEPR/data/AFHSC-top300-zip5.csv")[, c(4, 8, 9)]
remove_rows <- integer(0)
for (i in 1:nrow(a)) {
    index <- which(location_data_frame$zip5 == a[i, 1])
    
    lon_lat <- location_data_frame[index, c(2, 3)]
    if (nrow(lon_lat) == 0) 
        remove_rows <- append(remove_rows, i)
    if (nrow(lon_lat) != 0) {
        state <- map.where("state", lon_lat)
        if (is.na(state)) 
            remove_rows <- append(remove_rows, i) else {
            if (state == "district of columbia") 
                state_name <- "DC" else {
                state <- gsub(":main", "", state)
                state <- gsub(":south", "", state)
                state_name <- state.abb[grep(state, state.name, ignore.case = TRUE)]
                length(state_name) <- 1
            }
            a[i, c(3, 4)] <- lon_lat
            a[i, 5] <- state_name
        }
    }
}
b <- a[-1 * remove_rows, ]

zip_school <- read.csv("~/Dropbox/LEPR/data/AFHSC-top300-zip5.csv", )[, c(4, 23)]
new_zip_school <- data.frame(zip5 = integer(0), school_open = numeric(0), peak_date = numeric(0), state = character(0))
for (i in 1:nrow(zip_school)) {
    if (length(b[b$zip == zip_school[i, 1], 2]) != 0 && !is.na(zip_school[i, 2]) && zip_school[i, 2] != "") {
        school_date <- paste(zip_school[i, 2], "2009", sep = "-")
        new_school_date <- as.Date(school_date, "%d-%b-%Y")
        peak_date <- b[b$zip == zip_school[i, 1], 2]
        new_peak_date <- as.Date(peak_date, "%m/%d/%y")
        state <- b[b$zip == zip_school[i, 1], 5]
        new_zip_school <- rbind(new_zip_school, data.frame(zip5 = zip_school[i, 1], school_open = new_school_date, peak_date = new_peak_date, state = state))
    }
}

color <- find_hhs_region(new_zip_school$state)
par(pty = "s")
plot(x = as.Date(new_zip_school$peak_date), y = as.Date(new_zip_school$school_open), pch = 20, col = color, ylab = "School Open Date", xlab = "Peak Date")

all_lm <- lm(formula = as.numeric(school_open) ~ as.numeric(peak_date), data = new_zip_school)
east_lm <- lm(formula = as.numeric(school_open) ~ as.numeric(peak_date), data = new_zip_school[which(color == "red"), ])

corr_b <- rcorr(as.numeric(new_zip_school$school_open), as.numeric(new_zip_school$peak_date), type = "pearson")
corr_r <- rcorr(as.numeric(new_zip_school[which(color == "red"), 2]), as.numeric(new_zip_school[which(color == "red"), 3]), type = "pearson")

par(pty = "s")
abline(all_lm)
abline(east_lm, col = "red")

title <- paste("Peak Date of 2009 - 2010 Influenza Wave Against School Start Date\nBlack:", round(corr_b$r[2], 2), ", p ~", signif(corr_b$P[2], 
    1), "   Red:", round(corr_r$r[2], 2), ", p ~", signif(corr_r$P[2], 1))
title(main = title)
legend("topleft", c("All US", "East US"), lwd = c(2, 2), col = c("black", "red"))
}
