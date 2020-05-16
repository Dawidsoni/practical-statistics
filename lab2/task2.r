library(ggplot2)
library(cowplot)
library(dplyr)


get_transformed_earnings_df <- function(income_df) {
  EPSILON = 1e-6
  earnings_id <- pmax(income_df$earnings, EPSILON)
  id_df <- data.frame(earnings=earnings_id, category=c("Id"))
  earnings_sqrt <- sqrt(earnings_id)
  sqrt_df <- data.frame(earnings=earnings_sqrt, category=c("Sqrt"))
  earnings_ln <- log(earnings_id)
  ln_df <- data.frame(earnings=earnings_ln, category=c("Ln"))
  earnings_ln_add1 <- log(earnings_id + 5000)
  ln_df <- data.frame(earnings=earnings_ln, category=c("Ln"))
  ln_add1_df <- data.frame(earnings=earnings_ln_add1, category=c("Ln_add1"))
  transformed_earnings_df <- NULL
  transformed_earnings_df <- rbind(transformed_earnings_df, id_df)  
  transformed_earnings_df <- rbind(transformed_earnings_df, sqrt_df)  
  transformed_earnings_df <- rbind(transformed_earnings_df, ln_df)
  transformed_earnings_df <- rbind(transformed_earnings_df, ln_add1_df)
  return(transformed_earnings_df)
}


get_earnings_plot <- function(earnings_df, title) {
  return(ggplot(earnings_df, aes(sample=earnings))
        + ggtitle(title)
        + theme(plot.title = element_text(hjust = 0.5, size=10))
        + geom_qq(colour="blue")
        + geom_qq_line(colour="green"))
}


income_df <- read.table(
  "http://www.math.uni.wroc.pl/~elsner/dydaktyka/dane/income.dat",
  col.names=c("id", "age", "education_level", "sex", "earnings", "emloyment_sector")
)
earnings_df <- get_transformed_earnings_df(income_df)
plot1 <- get_earnings_plot(earnings_df %>% filter(category == "Id"),
                           title="QQ-plot of Earnings")
plot2 <- get_earnings_plot(earnings_df %>% filter(category == "Sqrt"),
                           title="QQ-plot of Sqrt(earnings)")
plot3 <- get_earnings_plot(earnings_df %>% filter(category == "Ln"),
                           title="QQ-plot of Ln(earnings)")
plot4 <- get_earnings_plot(earnings_df %>% filter(category == "Ln_add1"),
                           title="QQ-plot of Ln(earnings + 5000)")
print(plot_grid(plot1, plot2, plot3, plot4))
rm(list = ls())

"
As expected, the outcome is that earnings are not normally distributed (its distribution is closer to the
exponential distribution). We can see that sqrt(earnings) distribution is problematic on both sides as it has
too high values in these areas. In contrast, applying ln(earnings) causes that values are too low on the left
side of the plot (because ln(x) decreases too quickly near 0). To overcome these issues, I applied a 
function ln(earnings + 5000), which is very close to a line of quantiles. It follows that this transformation
results in a distribution, which is close to the normal distribution.

Note that I assumed that earnings shouldn't be negative, so I assigned 0 (more precely: epsilon) to
negative values. There were very few negative values, so they don't have much impact on the results.
"