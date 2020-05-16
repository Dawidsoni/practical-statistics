library(ggplot2)


get_interval_ratio <- function(values, from_value, to_value) {
  return(length(values[values >= from_value & values <= to_value]) / length(values))
}


get_interval_ratios_based_on_deviations <- function(values) {
  values_mean <- mean(values)
  values_std <- sd(values)
  std1_ratio <- get_interval_ratio(values, values_mean - values_std, values_mean + values_std)
  std2_ratio <- get_interval_ratio(values, values_mean - 2 * values_std, values_mean + 2 * values_std)
  std3_ratio <- get_interval_ratio(values, values_mean - 3 * values_std, values_mean + 3 * values_std)
  df <- data.frame(c("std1_ratio", "std2_ratio", "std3_ratio"), c(std1_ratio, std2_ratio, std3_ratio))
  colnames(df) <- c("Statistic", "Ratio")
  df[, "Ratio"] = round(df[, "Ratio"], digits=3)
  return(df)
}


grades_df <- read.table(
  "http://www.math.uni.wroc.pl/~elsner/dydaktyka/dane/grades.txt",
  col.names=c("id", "grades_mean", "iq", "sex", "test_result")
)
ratios_df <- get_interval_ratios_based_on_deviations(grades_df$iq)
print(ratios_df)
print(ggplot(grades_df, aes(sample=iq))
      + ggtitle("QQ-plot of IQ vs. standard normal distribution")
      + theme(plot.title = element_text(hjust = 0.5))
      + geom_qq(colour="blue")
      + geom_qq_line(colour="green"))
rm(list = ls())

"
Based on the analysis, we can say that IQ follows a normal distribution.

The ratios of points in the intervals [m - s, m + s], [m - 2 * s, m + 2 * s], [m - 3 * s, m + 3 * s]
are close to the ratios of the normal distribution (68.3% vs. 70.5%, 93.6% vs. 95.5%, 100% vs. 99.7%).

Points of QQ-plot are very close to a line, which passes through quantiles. The only area, where points differ
with the line, are points below [m - s]. But it only accounts for 12 points of our data, so they are not
exptected to perfectly match the line.
"