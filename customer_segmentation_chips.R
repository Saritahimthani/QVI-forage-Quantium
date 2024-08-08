# Load the libraries
library(tidyverse)
library(readr)
library(summarytools)
library(ggplot2)
library(lubridate)


# Install required packages if not already installed
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("lubridate")) install.packages("lubridate")



# Import data
transaction_data <- read.csv("../QVI-forage/QVI_transaction_data.csv")
customer_data <- read.csv("../QVI-forage/QVI_purchase_behaviour.csv")

# Data inspection
str(transaction_data)
summary(transaction_data)
str(customer_data)
summary(customer_data)

# Check for missing values
sum(is.na(transaction_data))
sum(is.na(customer_data))

# Remove outliers if necessary
# For example, we can remove transactions with negative or zero prices
transaction_data <- transaction_data %>% filter(TOT_SALES > 0)

# Correct data formats if necessary
transaction_data$DATE <- ymd(transaction_data$DATE)

# Extract pack size and brand name from the product description
transaction_data <- transaction_data %>%
  mutate(PACK_SIZE = as.numeric(str_extract(PROD_NAME, "\\d+")),
         BRAND_NAME = word(PROD_NAME, 1))

# Merge transactions data with purchase behaviour data
merged_data <- merge(transaction_data, customer_data, by = "LYLTY_CARD_NBR")


# Total sales and total quantity by customer segment
sales_by_segment <- merged_data %>%
  group_by(LIFESTAGE, PREMIUM_CUSTOMER) %>%
  summarise(TOT_SALES = sum(TOT_SALES),
            TOT_QTY = sum(PROD_QTY))

# Average pack size by customer segment
avg_pack_size_by_segment <- merged_data %>%
  group_by(LIFESTAGE, PREMIUM_CUSTOMER) %>%
  summarise(AVG_PACK_SIZE = mean(PACK_SIZE, na.rm = TRUE))

# Average price per unit by customer segment
merged_data <- merged_data %>%
  mutate(PRICE_PER_UNIT = TOT_SALES / PROD_QTY)

avg_price_by_segment <- merged_data %>%
  group_by(LIFESTAGE, PREMIUM_CUSTOMER) %>%
  summarise(AVG_PRICE_PER_UNIT = mean(PRICE_PER_UNIT, na.rm = TRUE))

ggplot(sales_by_segment, aes(x = LIFESTAGE, y = TOT_SALES, fill = PREMIUM_CUSTOMER)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Total Sales by Customer Segment") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(avg_pack_size_by_segment, aes(x = LIFESTAGE, y = AVG_PACK_SIZE, fill = PREMIUM_CUSTOMER)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Average Pack Size by Customer Segment") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(avg_price_by_segment, aes(x = LIFESTAGE, y = AVG_PRICE_PER_UNIT, fill = PREMIUM_CUSTOMER)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Average Price per Unit by Customer Segment") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))









