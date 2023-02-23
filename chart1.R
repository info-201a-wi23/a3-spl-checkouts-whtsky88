library(ggplot2)
library(zoo)
library(dplyr)

options(warn = -1)
options(dplyr.summarise.inform = FALSE)

checkout_data <- read.csv("2017-2023-10-Checkouts-SPL-Data.csv",
                          stringsAsFactors = FALSE)

checkout_data <- checkout_data %>%
  mutate(month_year = zoo::as.yearmon(paste(checkout_data$CheckoutYear,
                                            checkout_data$CheckoutMonth, sep = "-")))

# checkout by usage class for each month of year
checkout_by_month <- checkout_data %>%
  group_by(month_year, UsageClass) %>%
  summarize(checkout_total = sum(Checkouts))

# make usage class as factor
checkout_by_month$UsageClass <- as.factor(checkout_by_month$UsageClass)

# create a scatter plot
ggplot(checkout_by_month, aes(x = month_year,
                              y = checkout_total, color = UsageClass)) +
  geom_point() +
  geom_smooth(method = lm, formula = y ~ x, se = FALSE, fullrange = TRUE) +
  labs(title = "Trends over Time in Total Checkout for Different Usage Class",
       x = "Time (month)",
       y = "Total Checkout Number",
       color = "Usage Class")
