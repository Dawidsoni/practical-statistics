library(ggplot2)


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


print_quantitive_statistics <- function(statistics_name, grades_df) {
  statistics <- get_statistics("IQ", grades_df$iq)
  statistics <- merge(statistics, get_statistics("Grades mean", grades_df$grades_mean), by="Property")
  statistics <- merge(statistics, get_statistics("Test result", grades_df$test_result), by="Property")
  print(statistics_name)
  print(statistics, digits = 2)
  cat("\n\n")
}


plot_quantitive_columns <- function(grades_df) {
  print(ggplot(grades_df, aes(x=iq)) + geom_histogram(fill="lightblue", color="black", binwidth=5.0))
  print(ggplot(grades_df, aes(x=grades_mean)) + geom_histogram(fill="lightblue", color="black", binwidth=1.0))
  print(ggplot(grades_df, aes(x=test_result)) + geom_histogram(fill="lightblue", color="black", binwidth=5.0))
}


plot_compared_quantitive_columns <- function(grades_df) {
  print(ggplot(grades_df, aes(iq, fill=sex)) 
        + geom_histogram(binwidth=5.0, position=position_dodge(2.5)))
  print(ggplot(grades_df, aes(grades_mean, fill=sex)) 
        + geom_histogram(binwidth=1.0, position=position_dodge(0.5)))
  print(ggplot(grades_df, aes(test_result, fill=sex))
        + geom_histogram(binwidth=5.0, position=position_dodge(2.5)))
}


grades_df <- read.table(
  "http://www.math.uni.wroc.pl/~elsner/dydaktyka/dane/grades.txt",
  col.names=c("id", "grades_mean", "iq", "sex", "test_result")
)
grades_df["sex"] <- factor(grades_df[, "sex"], labels=c("Female", "Male"))
print_quantitive_statistics("All people", grades_df)
plot_quantitive_columns(grades_df)
print_quantitive_statistics("Men only", grades_df[grades_df$sex == "Male", ])
print_quantitive_statistics("Women only", grades_df[grades_df$sex == "Female", ])
plot_compared_quantitive_columns(grades_df)
rm(grades_df, get_statistics, print_quantitive_statistics)
rm(plot_quantitive_columns, plot_compared_quantitive_columns)

"
IQ: the distribution is centered around the value of 105 and it's close to the normal distribution (but it's
not ideally symetric). There are a few people with very low/high IQ compared to others.

IQ (male vs. female): both distributions have similar mean and variance. There are more men than women with
extremely high IQ, but it can be caused by the fact that there are more men than women in the experiment.
There is also a bit more women with low IQ compared to men, but it can be a coincidence as the sample size is
very low.

Grades mean: the distribution is cented around the value of 8. The number of people with lower/higher grades
mean decreases exponentially. It is similar to the beta disitrbution (but it's more like a mirror image).

Grades mean (male vs. female): distribution of men grades has slightly higher variance, which means that they
achieve the highest and the lowest possible results. Women grades concrentrates more near a mean. 

Test result: the distribution is similar to the grades mean distribution, except that the values are in 
diffrent range. The center of the distribution is in near the value of 65.

Test result (male vs. female): distribution of men test results has a bit higher mean than for women. There
are two men outliers with very high test result, but as the number of women participating in test is much
lower, it can be only coincidence.
"