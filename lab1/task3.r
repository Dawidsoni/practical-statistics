get_statistics <- function(name, values) {
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


get_earnings_outliers <- function(df) {
  q1 <- unname(quantile(df$earnings, 0.25))
  q3 <- unname(quantile(df$earnings, 0.75))
  iqr <- q3 - q1
  non_outliers_range_start <- q1 - 1.5 * iqr 
  non_outliers_range_end <- q3 + 1.5 * iqr
  outliers_df <- df[df$earnings < non_outliers_range_start | df$earnings > non_outliers_range_end, ]
  return(outliers_df)
}


income_df <- read.table(
  "http://www.math.uni.wroc.pl/~elsner/dydaktyka/dane/income.dat",
  col.names=c("id", "age", "education_level", "sex", "earnings", "emloyment_sector")
)
income_df["sex"] <- factor(income_df[, "sex"], labels=c("Male", "Female"))
men_income_df <- income_df[income_df$sex == "Male", ]
women_income_df <- income_df[income_df$sex == "Female", ]

statistics <- get_statistics("Earnings", income_df$earnings)
statistics <- merge(statistics, get_statistics("Men earnings", men_income_df$earnings))
statistics <- merge(statistics, get_statistics("Women earnings", women_income_df$earnings))
print(statistics, digits=2)
print(ggplot(income_df, aes(earnings)) + geom_histogram(fill="lightblue", color="black", bins=20))
print(ggplot(income_df, aes(earnings, fill=sex)) 
      + geom_histogram(binwidth=2e4, position=position_dodge(1e4)))

outliers_df <- get_earnings_outliers(income_df)
cat("Number of outliers: ", dim(outliers_df)[1])
print(ggplot(outliers_df, aes(earnings)) 
      + geom_histogram(fill="lightblue", color="black", bins=20)
      + ggtitle("Outliers earnings"))

rm(statistics, income_df, men_income_df, women_income_df)
rm(get_earnings_outliers, get_statistics, outliers_df)

"
Earnings: While most of earnings are concentrated around the mean, there are several outliers (around 5.5%)
with way higher earnings compared to others. The distribution is close to the exponential distribution (but it 
doesn't decrease directly from 0 like a exponential distribution, but it starts increasing at some low values).

Earnings (men vs women): Distribution of men earnings has higher variance than the distribution of women
earnings. A lot of outliers are men with significantly higher earnings than others. 

Outliers: there are 3163 outliers, which is around 5.5% of samples. These outliers have higher value than the
mean value.
"
