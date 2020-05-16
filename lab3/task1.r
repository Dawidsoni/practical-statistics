library(ggplot2)


plot_binomal_with_gaussian <- function(prob, sample_count) {
  gaussian_mean <- sample_count * prob
  gaussian_std <- sqrt(sample_count * prob * (1 - prob))
  binom_df <- data.frame(
    sample = rbinom(50000, size = sample_count, p = prob)
  )
  binomal_plot <- ggplot(binom_df, aes(x = sample)) +
    geom_histogram(aes(y = ..density..), fill = "lightblue", color = "black", binwidth = 1.0) +
    stat_function(fun = dnorm, args = list(mean = gaussian_mean, sd = gaussian_std), size = 1.0, n = 1000) +
    ggtitle(paste0("Binomal distribution with n = ", sample_count, " prob = ", prob)) +
    theme(plot.title = element_text(hjust = 0.5))
  print(binomal_plot)
}


plot_binomal_with_gaussian(prob = 0.5, sample_count = 20)
plot_binomal_with_gaussian(prob = 0.5, sample_count = 100)
plot_binomal_with_gaussian(prob = 0.1, sample_count = 20)
plot_binomal_with_gaussian(prob = 0.1, sample_count = 100)
rm(list = ls())

"
From the experiments, it follows that if we draw sufficiently many samples from a binomal
distribution, the histogram of these samples resembles a normal distribution (to which
considered binomal disitrution converge). The number of samples to approximate it must be
suffiently high. Particularly, a histgram representing 50 thousands samples approximates
a real distribution much better than while using only several hundreds of samples. Note
that even while using 50000 samples, the approximation is not perfect as we can see that
the histograms are not ideally symmetric.
"