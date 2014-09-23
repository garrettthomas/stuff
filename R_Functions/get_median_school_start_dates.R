setwd("~/Dropbox/LEPR/garrett/")
files <- list.files(pattern = ".csv")
data <- data.frame(matrix(nrow = 0, ncol = 2))
for (file in files) {
    start_dates <- read.csv(file)
    start_dates$start_date <- as.Date(start_dates$start_date, "%m/%d/%y")
    median_start_date <- median(start_dates$start_date, na.rm = TRUE)
    location <- gregexpr(pattern = "_", file)
    state <- substring(file, 1, location[[1]][1] - 1)
    if (is.element(state, c("new", "west", "north", "rhode", "south", "district"))) {
        state <- substring(file, 1, location[[1]][2] - 1)
    }
    data <- rbind(data, data.frame(state = state, start_date = median_start_date))
} 
