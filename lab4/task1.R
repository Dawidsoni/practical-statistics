library(ggplot2)
library(cowplot)


get_diff_quantile <- function (se1, se2, count1, count2, confidence) {
  df <- round((se1 ^ 2 + se2 ^ 2)^2 / ((se1 ^ 4 / (count1 - 1)) + (se2 ^ 4 / (count2 - 1))))
  return(abs(qt((1 - confidence) / 2, df = df)))
}


get_pooled_confidence_interval <- function (values1, values2, confidence = 0.95) {
  diff_mean <- mean(values2) - mean(values1)
  se1 <- sd(values1) / sqrt(length(values1))
  se2 <- sd(values2) / sqrt(length(values2))
  diff_se <- sqrt(se1 ^ 2 + se2 ^ 2)
  quantile <- get_diff_quantile(se1, se2, length(values1), length(values2), confidence)
  return(c(diff_mean - diff_se * quantile, diff_mean + diff_se * quantile))
}


get_unpooled_confidence_inteval <- function (values1, values2, confidence = 0.95) {
  diff_mean <- mean(values2) - mean(values1)
  se1 <- sd(values1) / sqrt(length(values1))
  se2 <- sd(values2) / sqrt(length(values2))
  ss1 <- sum((values1 - mean(values1)) ^ 2)
  ss2 <- sum((values2 - mean(values2)) ^ 2)
  sc <- (ss1 + ss2) / (length(values1) + length(values2) - 2)
  diff_se <- sqrt(sc * (1 / length(values1) + 1 / length(values2)))
  quantile <- get_diff_quantile(se1, se2, length(values1), length(values2), confidence)
  return(c(diff_mean - diff_se * quantile, diff_mean + diff_se * quantile))
}


show_lengths_statistics <- function (intervals_df, distribution_title) {
  pooled_lengths_plot <- ggplot(intervals_df, aes(x = pooled_lengths)) +
    geom_histogram(fill = "lightblue", color = "black", bins = 20) +
    ggtitle(paste0("Histogram of pooled interval lengths (", distribution_title,")")) +
    theme(plot.title = element_text(hjust = 0.5))
  unpooled_lengths_plot <- ggplot(intervals_df, aes(x = unpooled_lengths)) +
    geom_histogram(fill = "lightblue", color = "black", bins = 20) +
    ggtitle(paste0("Histogram of unpooled interval lengths (", distribution_title,")")) +
    theme(plot.title = element_text(hjust = 0.5))
  print(plot_grid(pooled_lengths_plot, unpooled_lengths_plot))
}


show_probability_statistics <- function (intervals_df, title, mean_diff) {
  pooled_probability <- mean(intervals_df$pooled_starts <= mean_diff & intervals_df$pooled_ends >= mean_diff)
  unpooled_probability <- mean(intervals_df$unpooled_starts <= mean_diff & intervals_df$unpooled_ends >= mean_diff)
  print(paste0(title, " - pooled probability: ", pooled_probability))
  print(paste0(title, " - unpooled probability: ", unpooled_probability))
}


compare_confidence_intervals <- function (sample_func1, sample_func2, title, mean_diff, iterations_count=1000) {
  pooled_starts <- rep(NA, iterations_count)
  pooled_ends <- rep(NA, iterations_count)
  unpooled_starts <- rep(NA, iterations_count)
  unpooled_ends <- rep(NA, iterations_count)
  for (i in 1:iterations_count) {
    values1 <- sample_func1()
    values2 <- sample_func2()
    pooled_interval <- get_pooled_confidence_interval(values1, values2)
    pooled_starts[i] <- pooled_interval[1]
    pooled_ends[i] <- pooled_interval[2]
    unpooled_interval <- get_unpooled_confidence_inteval(values1, values2)
    unpooled_starts[i] <- unpooled_interval[1]
    unpooled_ends[i] <- unpooled_interval[2]
  }
  intervals_df <- data.frame(pooled_starts, pooled_ends, unpooled_starts, unpooled_ends)
  intervals_df$pooled_lengths <- intervals_df$pooled_ends - intervals_df$pooled_starts
  intervals_df$unpooled_lengths <- intervals_df$unpooled_ends - intervals_df$unpooled_starts
  show_lengths_statistics(intervals_df, title)
  show_probability_statistics(intervals_df, title, mean_diff)
}


compare_confidence_intervals(
  function() rnorm(5),
  function() rnorm(10, mean = 20, sd = 10),
  title = "5 samples from N(0, 1) and 10 samples from N(20, 10)",
  mean_diff = 20
)
compare_confidence_intervals(
  function() rnorm(5),
  function() rnorm(10, mean = 20, sd = 1),
  title = "5 samples from N(0, 1) and 10 samples from N(20, 1)",
  mean_diff = 20
)
compare_confidence_intervals(
  function () rnorm(10),
  function () rnorm(10, mean = 20, sd = 10),
  title = "10 samples from N(0, 1) and 10 samples from N(20, 10)",
  mean_diff = 20
)

rm(list = ls())

"
The first outcome from the experiments is that if the number of samples is equal in both
groups, then both pooled and unpooled methods give the same intervals. Indeed, histograms
of pooled and unpooled intervals look the same in this case (10 samples from N(0, 1) and
10 samples from N(20, 10)). It implies that the probabilities of the true mean difference
of means being in the confidence interval, are the same in both pooled and unpooled methods.

The other outcome is that if the number of samples is less in one group, then intervals
in the pooled method are longer compared to unpooled method. For instance, for the first
case (5 samples from N(0, 1) and 10 samples from N(20, 10)), the average interval length
is about 13 when pooled method is used and 18 when unpooled method is used. It can be also
seen from the histogram that the standard deviation of interval lengths is higher when
the unpooled method is used. Naturally, it follows that the probability of the true mean
difference being in the confidence interval, is a bit higher when the unpooled method is
used (if the number of samples in one group is smaller).

Another outcome is that the probabilities of the true mean difference being in the
confidence interval, are close to 0.95. This is expected, because our confidence interval
is constructed to contain real value with 95% confidence. If unpooled is used and the
number of samples is not equal in both groups, then the probability concrentrates in the
interval of [0.96, 0.99], depending on the standard deviations of groups and the relative
difference in the number of samples.
"
