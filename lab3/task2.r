generate_confidence_interval <- function(sample_count) {
  STANDARD_NORMAL_STD <- 1
  samples <- rnorm(sample_count)
  mean_of_samples <- mean(samples)
  max_mean_difference <- abs(qnorm(0.025)) * STANDARD_NORMAL_STD / sqrt(sample_count)
  from_interval <- mean_of_samples - max_mean_difference
  to_interval <- mean_of_samples + max_mean_difference
  return(c(from_interval, to_interval))
}


get_fraction_of_mean_in_interval <- function(experiment_count, sample_count) {
  mean_in_interval_counter <- 0
  for (i in 1:experiment_count) {
    confidence_interval <- generate_confidence_interval(sample_count)
    if (confidence_interval[1] <= 0 && confidence_interval[2] >= 0) {
      mean_in_interval_counter <- mean_in_interval_counter + 1
    }
  }
  return(mean_in_interval_counter / experiment_count)
}


get_average_interval_length <- function(experiment_count, sample_count) {
  interval_lengths <- c(0, experiment_count)
  for (i in 1:experiment_count) {
    confidence_interval <- generate_confidence_interval(sample_count)
    interval_lengths[i] <- confidence_interval[2] - confidence_interval[1]
  }
  return(mean(interval_lengths))
}


print(paste("n = 100 -> fraction =", get_fraction_of_mean_in_interval(experiment_count = 1000, sample_count = 100)))
print(paste("n = 200 -> fraction =", get_fraction_of_mean_in_interval(experiment_count = 1000, sample_count = 200)))
print(paste("n = 100 -> length =", get_average_interval_length(experiment_count = 1000, sample_count = 100)))
print(paste("n = 200 -> length =", get_average_interval_length(experiment_count = 1000, sample_count = 200)))
rm(list = ls())
"
In both cases (sample_size = 100 and sample_size = 200), the true mean is with the probability of 95% in the
considered confidence interval. This is expected behaviour as our confidence interval is constructed
to contain values with 95% confidence of containg the true mean.

On the hand, interval lengths differ depending on the number of samples. For 100 samples, the average of
interval lengths is about 0.39, while for 200 samples, the average is about 0.28. It naturally follows that
the mean of 200 samples is closer to the true mean (on average).

Note that in this task (in constrast to tasks 3 and 4) we assume that the standard deviation is known,
so we use normal distribution for our approximations, instead of Student distribution.
"