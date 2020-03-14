get_statistics = function(values) {
  boxplot(values)
  prop_min <- min(values)
  prop_max <- max(values)
  prop_mean <- mean(values)
  prop_var <- var(values)
  prop_std <- sd(values)
  prop_1st_q <- quantile(values, 0.25)  
  prop_3rd_q <- quantile(values, 0.75)
  stats <- c("min", "max", "mean", "var", "std", "1st Q", "3rd Q")
  values <- c(prop_min, prop_max, prop_mean, prop_var, prop_std, prop_1st_q, prop_3rd_q)
  df <- data.frame(stats, values)
  return(df)
}

par(mfrow=c(1, 3))
print(get_statistics(rgeom(200, 0.2)), digits=2)
print(get_statistics(rgamma(200, 2)), digits=2)
print(get_statistics(rexp(200, 0.2)), digits=2)
