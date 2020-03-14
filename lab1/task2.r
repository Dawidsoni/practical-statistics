split_based_on_grades_mean <- function(grades_df, splits) {
  df_slices = c()
  for (i in 2:length(splits)) {
    val_start = splits[i - 1]
    val_end = splits[i]
    df_slices[[i - 1]] = grades_df[
      grades_df$grades_mean >= val_start & grades_df$grades_mean < val_end,
    ]
  }
  return(df_slices)
}


plot_df_slices <- function(val_slices, df_slices) {
  par(mfrow=c(1, length(df_slices)))
  for (i in 1:length(df_slices)) {
    title = paste0("From ", val_slices[i], " to ", val_slices[i + 1])
    hist(df_slices[[i]]$iq, main=title)
  }
}


grades_df <- read.table(
  "~/Desktop/PSP/lab1/grades.txt",
  col.names=c("id", "grades_mean", "iq", "sex", "test_result")
)

two_splits = c(-Inf, 8.0, Inf)
two_slices = split_based_on_grades_mean(grades_df, two_splits)
plot_df_slices(two_splits, two_slices)

three_splits = c(-Inf, 7.0, 9.0, Inf)
three_slices = split_based_on_grades_mean(grades_df, three_splits)
plot_df_slices(three_splits, three_slices)

four_splits = c(-Inf, 6.5, 8.0, 9.5, Inf)
four_slices = split_based_on_grades_mean(grades_df, c(-Inf, 6.5, 8.0, 9.5, Inf))
plot_df_slices(four_splits, four_slices)

five_splits = c(-Inf, 5.5, 7.0, 9.0, 10.5, Inf)
five_slices = split_based_on_grades_mean(grades_df, five_splits)
plot_df_slices(five_splits, five_slices)
# When the number of slices is high, there are few points in each slice, so we overfit.

