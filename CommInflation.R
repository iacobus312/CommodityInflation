library(midasr)
library(readxl)
library(dplyr)

pce_monthly <- read_excel("pce data.xlsx")
daily_data <- read_excel("relevant data.xlsx", sheet = "commodity index data")

names(pce_monthly)[names(pce_monthly) == "Last Price"] <- "pce"
names(daily_data)[names(daily_data) == "Last Price"] <- "commodity_index"

date_strings <- daily_data$Date

y <- as.numeric(pce_monthly$pce)
x <- as.numeric(daily_data$commodity_index)

freq_ratio <- floor(length(x) / length(y))

x_adjusted_length <- floor(length(x) / freq_ratio) * freq_ratio
x <- x[1:x_adjusted_length]
kept_indices <- 1:x_adjusted_length

needed_y_length <- length(x) / freq_ratio
if (length(y) > needed_y_length) {
  y <- y[1:needed_y_length]
} else if (length(y) < needed_y_length) {
  x_length <- length(y) * freq_ratio
  x <- x[1:x_length]
  kept_indices <- 1:x_length
}

midas_data <- list(y = y, x = x)

formula_u <- y ~ mls(x, 0:(freq_ratio-1), freq_ratio)
umidas <- midas_u(formula_u, midas_data)

model_summary <- summary(umidas)
print(model_summary)
cat("\nR-squared:", model_summary$r.squared, "\n")
cat("Adjusted R-squared:", model_summary$adj.r.squared, "\n")

coefs <- coef(umidas)
daily_estimates <- rep(NA, length(x))

for (i in freq_ratio:length(x)) {
  window_data <- x[(i-freq_ratio+1):i]
  estimate <- coefs[1]
  for (j in 1:freq_ratio) {
    coef_index <- j + 1
    if (coef_index <= length(coefs)) {
      estimate <- estimate + coefs[coef_index] * window_data[j]
    }
  }
  daily_estimates[i] <- estimate
}

daily_estimates <- pmin(daily_estimates, 5)

output <- data.frame(
  Date = date_strings[kept_indices],
  commodity_index = x,
  pce_daily = daily_estimates
)

output <- output[!is.na(output$pce_daily), ]

write.csv(output, "daily_pce_estimates.csv", row.names = FALSE)