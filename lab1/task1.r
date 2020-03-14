get_statistics <- function(name, values) {
  hist(values, main=name)
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

print_quantitive_statistics = function(name, grades_df) {
  print(name)
  print(get_statistics("IQ", grades_df$iq), digits=2)
  print(get_statistics("Grades mean", grades_df$grades_mean), digits=2)
  print(get_statistics("Test result", grades_df$test_result), digits=2)
  cat("\n\n")
}

par(mfrow=c(1, 3))
grades_df <- read.table(
  "~/Desktop/PSP/lab1/grades.txt",
  col.names=c("id", "grades_mean", "iq", "sex", "test_result")
)
print_quantitive_statistics("All", grades_df)
print_quantitive_statistics("Men only", grades_df[grades_df$sex == "M", ])
print_quantitive_statistics("Women only", grades_df[grades_df$sex == "F", ])
