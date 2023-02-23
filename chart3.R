library(ggplot2)
library(dplyr)

options(warn = -1)
options(dplyr.summarise.inform = FALSE)

checkout_data <- read.csv("2017-2023-10-Checkouts-SPL-Data.csv",
                          stringsAsFactors = FALSE)

library(ggplot2)
library(dplyr)

options(warn = -1)
options(dplyr.summarise.inform = FALSE)

checkout_data <- read.csv("2017-2023-10-Checkouts-SPL-Data.csv",
                          stringsAsFactors = FALSE)


# checkout for The Hobbit for each month of year
checkout_by_month <- checkout_data %>%
  filter(Title == "The Hobbit" & CheckoutYear != "2023") %>%
  group_by(CheckoutMonth, CheckoutYear) %>%
  summarize(checkout_total_hobbit = sum(Checkouts))

# for label on the stacked bar chart
totals <- checkout_data %>%
  filter(Title == "The Hobbit" & CheckoutYear != "2023") %>%
  group_by(CheckoutMonth) %>%
  summarize(checkout_total = sum(Checkouts))

# make variables as factor
checkout_by_month$CheckoutYear <- as.factor(checkout_by_month$CheckoutYear)
checkout_by_month$CheckoutMonth <- as.factor(checkout_by_month$CheckoutMonth)

# create a stacked bar plot
ggplot(checkout_by_month, aes(x = CheckoutMonth,
                              y = checkout_total_hobbit, fill = CheckoutYear)) +
  geom_col() +
  theme() +
  geom_text(aes(CheckoutMonth, checkout_total + 5,
                label = checkout_total, fill = NULL), data = totals) +
  labs(title = "The Hobbit Total Checkout for Different Month",
       x = "Month",
       y = "Total Checkout Number",
       fill = "Year")
