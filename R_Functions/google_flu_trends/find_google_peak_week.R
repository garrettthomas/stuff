find_google_peak_week <- function(which_years=c(0:9)) {
    cities = read.csv(file = "~/GFTvsILINET/DATA/google_city_data.csv", header = TRUE)
    data <- data.frame(matrix(nrow = length(which_years), ncol = 97))
    colnames(data) <- colnames(cities[, 2:98])
    for (j in which_years) {
        a = 42 + 52 * j
        b = 94 + 52 * j
        
        for (k in 2:98) {
            index <- which.max(cities[a:b, k])
            if (length(index) == 0) 
                data[which(which_years == j), k - 1] <- NA 
            else data[which(which_years == j), k - 1] <- as.character(cities$Date[a-1+index])
        }
        
        
    }
    
    return(data)
}

 
