library(ggplot2)
library(cowplot)


get_prefix_means <- function(sequence) {
  prefix_means <- rep(NULL, length(sequence))
  prefix_sum <- 0
  for (i in 1:length(sequence)) {
    prefix_sum <- prefix_sum + sequence[i]
    prefix_means[i] <- prefix_sum / i
  }
  return(prefix_means)
}


get_coin_tosses <- function(n_tosses) {
  sequence <- rbinom(n=n_tosses, size=1, prob=0.5)
  prefix_means <- get_prefix_means(sequence)
  return(data.frame(trial=1:n_tosses, prefix_mean=prefix_means))
}


get_tosses_plot <- function(title, sample_number1, sample_number2) {
  coin_tosses1 <- get_coin_tosses(n_tosses=1000)
  coin_tosses1["sample_number"] <- paste("Sample", sample_number1)
  coin_tosses2 <- get_coin_tosses(n_tosses=1000)
  coin_tosses2["sample_number"] <- paste("Sample", sample_number2)
  coin_tosses <- rbind(coin_tosses1, coin_tosses2)
  return(ggplot(coin_tosses, aes(x=trial, y=prefix_mean, color=sample_number)) 
         + geom_line()
         + ggtitle(title)
         + theme(plot.title = element_text(hjust = 0.5, size=10)))
}


title <- "Coin tosses prefix means"
for (i in 1:3) {
  p1 <- get_tosses_plot(title, sample_number1=(i * 4 - 3), sample_number2=(i * 4 - 2))
  p2 <- get_tosses_plot(title, sample_number1=(i * 4 - 1), sample_number2=(i * 4))
  print(plot_grid(p1, p2, nrow=2))
}
rm(list = ls())

"
It's clear from the experiments that a mean of samples quickly converges to 0.5. Additionaly, for most of
sequences, a mean is stabilized around a value of 0.5 after about 200 trials. All of means of sequences were
in the range of [0.45, 0.55] after 1000 trias, which means that it happens with very high probability (it is
expected as a mean of samples is a normal distribution with decreasing variance for higher number of trials).
"