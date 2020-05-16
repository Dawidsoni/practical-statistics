library(ggplot2)


get_confidence_interval <- function(values, confidence) {
  max_standard_diff <- abs(qt((1 - confidence) / 2, df = length(values) - 1))
  max_diff <- max_standard_diff * sd(values) / sqrt(length(values))
  from_interval <- mean(values) - max_diff
  to_interval <- mean(values) + max_diff
  return(c(from_interval, to_interval))
}


plot_confidence_intervals <- function(values, title) {
  confidences <- seq(0.0, 1.0, 0.01)
  from_intervals <- rep(NA, length(confidences))
  to_intervals <- rep(NA, length(confidences))
  for (i in seq_along(confidences)) {
    interval <- get_confidence_interval(values, confidences[i])
    from_intervals[i] <- interval[1]
    to_intervals[i] <- interval[2]
  }
  intevals_df <- data.frame(confidences, from_intervals, to_intervals)
  plot_of_intervals <- ggplot(intevals_df) +
    geom_line(mapping = aes(x = from_intervals, y = confidences), col = "blue") +
    geom_line(mapping = aes(x = to_intervals, y = confidences), col = "green") +
    ggtitle(title) +
    theme(plot.title = element_text(hjust = 0.5))
  print(plot_of_intervals)
}

get_and_print_confidence_interval <- function(values, property, confidence) {
  interval <- round(get_confidence_interval(values, confidence), digits = 2)
  print(paste0(property, " (confidence ", confidence, "): ", interval[1], " to ", interval[2]))
}

grades_df <- read.table(
  "http://www.math.uni.wroc.pl/~elsner/dydaktyka/dane/grades.txt",
  col.names=c("id", "grades_mean", "iq", "sex", "test_result")
)
get_and_print_confidence_interval(grades_df$iq, property = "IQ", confidence = 0.9)
get_and_print_confidence_interval(grades_df$iq, property = "IQ", confidence = 0.95)
get_and_print_confidence_interval(grades_df$iq, property = "IQ", confidence = 0.99)
plot_confidence_intervals(grades_df$iq, title = "Confidence intervals of IQ")
get_and_print_confidence_interval(grades_df$test_result, property = "Test result", confidence = 0.9)
get_and_print_confidence_interval(grades_df$test_result, property = "Test result", confidence = 0.95)
get_and_print_confidence_interval(grades_df$test_result, property = "Test result", confidence = 0.99)
plot_confidence_intervals(grades_df$test_result, title = "Confidence intervals of test result")
rm(list = ls())

"
From the experiments, it follows that IQ is in the range [106.44, 111.41] for a confidence of 0.9,
in the range [105.95, 111.89] for a confidence of 0.95 and in the range [104.98, 112.86] for a
confidence of 0.99. Meanwhile, the test result is in the range [54.62, 59.3] for a confidence of 0.9,
in the range [54.16, 59.76] for a confidence of 0.95 and in the range [53.25, 60.67] for a
confidence of 0.99.

In summary, the ranges are quite big, so it's not possible to derrive the exact mean of IQ and
test result from this sample.
"
