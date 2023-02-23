library(dplyr)
library(tidyverse)

options(warn = -1)

checkout_data <- read.csv("2017-2023-10-Checkouts-SPL-Data.csv",
                          stringsAsFactors = FALSE)

# checkout ratio of digital in 2017
checkout_ratio_2017 <- checkout_data %>%
  filter(CheckoutYear == "2017") %>%
  group_by(UsageClass) %>%
  summarize(copies_version = sum(Checkouts))
digital_ratio_2017 <- round(checkout_ratio_2017$copies_version[1] /
                      sum(checkout_ratio_2017$copies_version) * 100, digit = 2)

# checkout ratio of digital in 2022
checkout_ratio_2022 <- checkout_data %>%
  filter(CheckoutYear == "2022") %>%
  group_by(UsageClass) %>%
  summarize(copies_version = sum(Checkouts))
digital_ratio_2022 <- round(checkout_ratio_2022$copies_version[1] /
                      sum(checkout_ratio_2022$copies_version) * 100, digit = 2)

# Most checkout year for The Hobbit
most_checkout_year_hobbit <- checkout_data %>%
  filter(Title == "The Hobbit") %>%
  group_by(CheckoutYear) %>%
  summarize(hobbit_checkout = sum(Checkouts)) %>%
  filter(hobbit_checkout == max(hobbit_checkout)) %>%
  pull(CheckoutYear)

# Least checkout year for The Hobbit excluding 2023
least_checkout_year_hobbit <- checkout_data %>%
  filter(Title == "The Hobbit" & CheckoutYear != "2023") %>%
  group_by(CheckoutYear) %>%
  summarize(hobbit_checkout = sum(Checkouts)) %>%
  filter(hobbit_checkout == min(hobbit_checkout)) %>%
  pull(CheckoutYear)

# Year with the most checkout for ebooks
most_checkout_year_ebook <- checkout_data %>%
  filter(MaterialType == "EBOOK") %>%
  group_by(CheckoutYear) %>%
  summarize(most_checkout = sum(Checkouts)) %>%
  filter(most_checkout == max(most_checkout)) %>%
  pull(CheckoutYear)
