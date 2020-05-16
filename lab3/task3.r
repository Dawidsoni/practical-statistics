library(cowplot)
library(ggplot2)


get_earnings_df <- function(income_df) {
  earnings_id <- income_df$earnings
  earnings_sqrt <- earnings_id
  earnings_sqrt[earnings_sqrt > 0] <- sqrt(earnings_sqrt[earnings_sqrt > 0])
  earnings_sqrt[earnings_sqrt < 0] <- -sqrt(-earnings_sqrt[earnings_sqrt < 0])
  earnings_df <- data.frame(earnings_id, earnings_sqrt)
  return(earnings_df)
}


plot_earnings_sqrt <- function(earnings_df) {
  plot_of_earnings_sqrt <- ggplot(earnings_df, aes(x = earnings_sqrt)) +
  geom_histogram(fill = "lightblue", color = "black", binwidth = 10.0) +
  ggtitle("Histogram of sqrt(earnings)") +
  theme(plot.title = element_text(hjust = 0.5))
  return(plot_of_earnings_sqrt)
}


plot_eanings_sample_means <- function(earnings, title, mean_count = 200) {
  mean_samples <- rep(NA, mean_count)
  for (i in 1:mean_count) {
    mean_samples[i] <- mean(sample(earnings, size = 200, replace = FALSE))
  }
  means_df <- data.frame(mean_samples)
  means_plot <- ggplot(means_df, aes(x = mean_samples)) +
    geom_histogram(fill = "lightblue", color = "black", bins = 12) +
    ggtitle(title)
    theme(plot.title = element_text(hjust = 0.5))
  return(means_plot)
}


get_confidence_interval <- function(values, confidence) {
  max_standard_diff <- abs(qt((1 - confidence) / 2, df = length(values) - 1))
  max_diff <- max_standard_diff * sd(values) / sqrt(length(values))
  from_interval <- mean(values) - max_diff
  to_interval <- mean(values) + max_diff
  return(c(from_interval, to_interval))
}


get_fraction_of_mean_in_interval <- function(values, experiment_count) {
  mean_in_interval_counter <- 0
  mean_of_values <- mean(values)
  for (i in 1:experiment_count) {
    samples <- sample(values, size = 200, replace = FALSE)
    confidence_interval <- get_confidence_interval(samples, confidence = 0.95)
    if (confidence_interval[1] <= mean_of_values && confidence_interval[2] >= mean_of_values) {
      mean_in_interval_counter <- mean_in_interval_counter + 1
    }
  }
  return(mean_in_interval_counter / experiment_count)
}


income_df <- read.table(
  "http://www.math.uni.wroc.pl/~elsner/dydaktyka/dane/income.dat",
  col.names=c("id", "age", "education_level", "sex", "earnings", "employment_sector")
)
earnings_df <- get_earnings_df(income_df)
print(plot_earnings_sqrt(earnings_df))
id_means_plot <- plot_eanings_sample_means(
  earnings_df$earnings_id, title = "Means of 200 samples of earnings"
)
sqrt_means_plot <- plot_eanings_sample_means(
  earnings_df$earnings_sqrt, title = "Means of 200 samples of sqrt(earnings)"
)
print(plot_grid(id_means_plot, sqrt_means_plot))
print(paste("Average earnings", round(mean(earnings_df$earnings_id), digits = 2)))
print(paste("Average sqrt(earnings)", round(mean(earnings_df$earnings_sqrt), digits = 2)))
fraction_id <- get_fraction_of_mean_in_interval(earnings_df$earnings_id, experiment_count = 200)
fraction_sqrt <- get_fraction_of_mean_in_interval(earnings_df$earnings_sqrt, experiment_count = 200)
print(paste("earnings fraction = ", fraction_id))
print(paste("sqrt(earnings fraction) = ", fraction_sqrt))
rm(list = ls())

"
Firstly, we determine that the average for earnings is equal to 37864.61, while for sqrt(earnings) it
is equal to 178.69 (note that I replaced negative values of earnings with 0 as I assume that there
shouldn't be any negative earnings). We can conclude that the mean of sqrt(earnings) is lower than
sqrt(mean(earnings)), which is intuitive.

From the histogram of sqrt(earnings), we see that its distribution is close to the normal distribution,
but it is truncated on the left side as sqrt(earnings) is always greater or equal to 0. Moreover, there
are some outliers on the right side of the distribution.

In both cases (for earnings and sqrt(earnings)), the true mean is with the probability of about 95% in
the considered confidence interval. In most experiments, this probability is a bit lower for
earnings, comparing to sqrt(earnings) (a bit 0.5% of difference). The reason for that can be that our
sample size is set to 200 and the distribution of means can converge (CLT) to normal distribution with
different rates, depending on the distribution. From the plotted histograms, it actually can be seen
that the distribution of means for sqrt(earnings) tends to be closer to the normal distribution, comparing
to the distribution of means for earnings.
"
