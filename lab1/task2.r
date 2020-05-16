grades_df <- read.table(
  "http://www.math.uni.wroc.pl/~elsner/dydaktyka/dane/grades.txt",
  col.names=c("id", "grades_mean", "iq", "sex", "test_result")
)

print(ggplot(grades_df, aes(x=iq)) 
      + geom_histogram(fill="lightblue", color="black", bins=2)
      + ggtitle("Two bins"))
print(ggplot(grades_df, aes(x=iq)) 
      + geom_histogram(fill="lightblue", color="black", bins=4)
      + ggtitle("Very few number of bins"))
print(ggplot(grades_df, aes(x=iq)) 
      + geom_histogram(fill="lightblue", color="black", bins=7)
      + ggtitle("Few number of bins"))
print(ggplot(grades_df, aes(x=iq))
      + geom_histogram(fill="lightblue", color="black", bins=14)
      + ggtitle("Optimal number of bins"))
print(ggplot(grades_df, aes(x=iq)) 
      + geom_histogram(fill="lightblue", color="black", bins=30)
      + ggtitle("Large number of bins"))
print(ggplot(grades_df, aes(x=iq))
      + geom_histogram(fill="lightblue", color="black", bins=100)
      + ggtitle("Very large number of bins"))

rm(grades_df)

"
If the number of bins is too high, there is a lot of noise in the distribution. There are many
adjacent bars with highly varying height.

If the number of bins is too low, some important details about the distribution are not visible (e.g.
density of points far from the mean)

The optimal number of bins is around 14. In this case, it can be seen that the distribution is
close to the normal distribution.
"