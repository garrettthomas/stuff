source("~/garrett/R_Functions/google_flu_trends/google_peak_distance.R")
source("~/garrett/R_Functions/google_flu_trends/google_peak_sh.R")
source("~/garrett/R_Functions/google_flu_trends/google_peak_school.R")

distance <- google_peak_distance()
sh <- google_peak_sh()
school <- google_peak_school()

mult_regression <- data.frame( name = distance$name, peak_date = distance$peak_date, 
                               distance_black = distance$black_distance, distance_red = distance$red_distance, 
                               sh = NA, school = as.Date(NA), color = distance$color)

for( i in 1:nrow(mult_regression) ) {
  
  sh_index <- which(sh$name==mult_regression$name[i])
  mult_regression$sh[i] <- sh$sh[sh_index]
  
  school_index <- which(school$name==mult_regression$name[i])
  mult_regression$school[i] <- school$school_start[school_index]
}
model_distance <- lm(as.numeric(peak_date) ~ distance_black, data = mult_regression)
model_sh <- lm(as.numeric(peak_date) ~ sh, data = mult_regression)
model_school <- lm(as.numeric(peak_date) ~ school, data = mult_regression)

model_distance_sh <- lm(as.numeric(peak_date) ~ distance_black + sh, data = mult_regression)
model_sh_school <- lm(as.numeric(peak_date) ~ sh + school, data = mult_regression)
model_school_distance <- lm(as.numeric(peak_date) ~ school + distance_black, data = mult_regression)


model <- lm(as.numeric(peak_date) ~ distance_black + as.numeric(school) + sh, data = mult_regression)
