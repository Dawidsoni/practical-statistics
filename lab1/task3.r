get_statistics <- function(name, values) {
  hist(values, main=name)
  p_min <- min(values)
  p_max <- max(values)
  p_mean <- mean(values)
  p_var <- var(values)
  p_std <- sd(values)
  p_1st_q <- quantile(values, 0.25)  
  p_3rd_q <- quantile(values, 0.75)
  var_coef <- sd(values) / mean(values)
  stats <- c("min", "max", "mean", "var", "std", "1st Q", "3rd Q", "VarCoef")
  values <- c(p_min, p_max, p_mean, p_var, p_std, p_1st_q, p_3rd_q, var_coef)
  df <- data.frame(stats, values)
  colnames(df) <- c("Property", name)
  return(df)
}


get_outliers <- function(values) {
  q1 <- unname(quantile(values, 0.25))
  q3 <- unname(quantile(values, 0.75))
  iqr <- q3 - q1
  non_outliers_range_start <- q1 - 1.5 * iqr 
  non_outliers_range_end <- q3 + 1.5 * iqr
  return(values[values < non_outliers_range_start | values > non_outliers_range_end])
}


income_df <- read.table(
  "~/Desktop/PSP/lab1/income.dat",
  col.names=c("id", "age", "education_level", "sex", "earnings", "emloyment_sector")
)
par(mfrow=c(1, 3))
print(get_statistics("Earnings", income_df$earnings), digits=2)
print(get_statistics("Men earnings", income_df[income_df$sex == 1, ]$earnings), digits=2)
print(get_statistics("Women earnings", income_df[income_df$sex == 2, ]$earnings), digits=2)
print(get_outliers(income_df$earnings))

