google_peak_distance <- function(year=6, plot = FALSE) {
require(mapdata)
require(ggmap)
require(Imap)
source("~/garrett/R_Functions/google_flu_trends/find_google_peak_week.R")
source("~/garrett/R_Functions/find_hhs_region.R")
require("Hmisc")

data(us.cities)
par(mfrow = c(1, 1))
a <- find_google_peak_week(year-1)
names(a) <- gsub("\\.", " ", names(a))
a <- a[, order(names(a))]
google_years <- c("2004-2005", "2005-2006", "2006-2007", "2007-2008", "2008-2009", "2009-2010", "2010-2011", "2011-2012", "2012-2013", "2013-2014")

    b <- subset(us.cities, is.element(name, names(a)))
    b <- cbind(b, peak_date = as.Date(unname(unlist(a)), "%m/%d/%y"))
    b <- na.omit(b)
    remove_rows <- which(b$country.etc == "AK" | b$country.etc == "HI")
    b <- b[-1 * remove_rows, ]
    b <- cbind(b, color = find_hhs_region(b[, 2]))
    cor_black <- 0
    cor_red <- 0
    for (i in 1:nrow(b)) {
        q <- b[i, c(5, 4)]
        temp_data_frame <- data.frame(distance = NA, peak_date = as.Date(b$peak_date))
        for (j in 1:nrow(b)) {
            temp_data_frame[j, 1] <- gdist(lon.1 = as.numeric(b[j, 5]), lat.1 = as.numeric(b[j, 4]), lon.2 = as.numeric(q[1]), lat.2 = as.numeric(q[2]), 
                units = "km")
        }
        cor1 <- cor(as.numeric(temp_data_frame[, 2]), temp_data_frame[, 1], method = "pearson")
        cor2 <- cor(as.numeric(temp_data_frame[which(b$color == "red"), 2]), temp_data_frame[which(b$color == "red"), 1], method = "pearson")
        if (cor1 > cor_black) {
            cor_black <- cor1
            distance_data_frame <- temp_data_frame
            b_name <- b$name[i]
        }
        if (cor2 > cor_red) {
            cor_red <- cor2
            red_data_frame <- temp_data_frame
            r_name <- b$name[i]
        }
    }
black_center <- rep(0,nrow(b))
black_center[which(b$name==b_name)] <- 1

red_center <- rep(0,nrow(b))
red_center[which(b$name==r_name)] <- 1

answer <- data.frame(name = b$name, peak_date = b$peak_date, black_center = black_center, 
                     black_distance=distance_data_frame$distance, red_center=red_center,
                     red_distance=red_data_frame$distance, color=b$color)
    if (plot) {
    ########### plot which maximizes black correlation
    
    all_lm <- lm(formula = distance ~ peak_date, data = distance_data_frame)
    east_lm <- lm(formula = distance ~ peak_date, data = distance_data_frame[which(b$color == "red"), ])
    
    par(pty = "s")
    speed_black <- round(all_lm$coefficients[[2]]/24, 2)
    speed_red <- round(east_lm$coefficients[[2]]/24, 2)
    
    y_label <- paste("Distance from", b_name, "(km)")
    corr_bb <- rcorr(as.numeric(distance_data_frame[, 2]), distance_data_frame[, 1], type = "pearson")
    corr_br <- rcorr(as.numeric(distance_data_frame[which(b$color == "red"), 2]), distance_data_frame[which(b$color == "red"), 1], type = "pearson")
    title <- paste("Peak Date of", google_years[year], "Influenza Wave Against Distance from", b_name, "\nBlack:", round(corr_bb$r[2], 2), ", p ~", 
        signif(corr_bb$P[2], 1), ", speed ~", speed_black, "km/hr  Red:", round(corr_br$r[2], 2), ", p ~", signif(corr_br$P[2], 1), ", speed ~", 
        speed_red, "km/hr")
    plot(x = as.Date(distance_data_frame[, 2]), y = distance_data_frame[, 1], xlab = "Date of Peak Week", ylab = y_label, col = b$color, pch = 20)
    title(main = title, cex = 0.7)
    abline(all_lm)
    abline(east_lm, col = "red")
    
    legend("topleft", c("All US", "East US"), lwd = c(2, 2), col = c("black", "red"))
    
    
    ########### plot which maximizes red correlation
    
    
    all_lm_red <- lm(formula = distance ~ peak_date, data = red_data_frame)
    east_lm_red <- lm(formula = distance ~ peak_date, data = red_data_frame[which(b$color == "red"), ])
    
    speed_rb <- round(all_lm_red$coefficients[[2]]/24, 2)
    speed_rr <- round(east_lm_red$coefficients[[2]]/24, 2)
    
    par(pty = "s")
    y_label <- paste("Distance from", r_name, "(km)")
    cor_red_black <- cor(as.numeric(red_data_frame[, 2]), red_data_frame[, 1], method = "pearson")
    corr_rb <- rcorr(as.numeric(red_data_frame[, 2]), red_data_frame[, 1], type = "pearson")
    corr_rr <- rcorr(as.numeric(red_data_frame[which(b$color == "red"), 2]), red_data_frame[which(b$color == "red"), 1], type = "pearson")
    title <- paste("Peak Date of", google_years[year], "Influenza Wave Against Distance from", r_name, "\nBlack:", round(corr_rb$r[2], 2), ", p ~", 
        signif(corr_rb$P[2], 1), ", speed ~", speed_rb, "km/hr  Red:", round(corr_rr$r[2], 2), ", p ~", signif(corr_rr$P[2], 1), ", speed ~", speed_rr, 
        "km/hr")
    plot(x = as.Date(red_data_frame[, 2]), y = red_data_frame[, 1], xlab = "Date of Peak Week", ylab = y_label, col = b$color, pch = 20)
    
    title(main = title, cex = 0.7)
    abline(all_lm_red)
    abline(east_lm_red, col = "red")
    legend("topleft", c("All US", "East US"), lwd = c(2, 2), col = c("black", "red"))
    }
return(answer)
}