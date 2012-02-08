dostats(1:10, mean, median, sd, quantile, iqr)
ldply(mtcars, dostats, median, mean, sd, quantile, IQR)
