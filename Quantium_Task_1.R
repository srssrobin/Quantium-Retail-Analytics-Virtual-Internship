# source: https://rpubs.com/ssha5953/Quantium_Task1 -----

#### Load libraries
library(ggplot2)
library(tidyverse)
library(tibble)
library(cowplot)
library(fitdistrplus)
library(data.table)
library(readr)
library(readxl)
library(stringr)
library(formatR)
library(ggmosaic)

#### Load dataset
transactionData <- read_excel("QVI_transaction_data.xlsx")
customerData <- read.csv("QVI_purchase_behaviour.csv")

#### 2. Examine transaction data
str(transactionData)

# Date is in integer format; change it to date
transactionData$DATE <- as.Date(transactionData$DATE, origin = "1899-12-30")

# Check that we are looking at the right products by examining PROD_NAME; 
# examine PROD_NAME
str(transactionData$PROD_NAME)

transactionData %>% count(PROD_NAME, sort = TRUE) %>% unique()

# Looks like we are definitely looking at potato chips. But we want to be sure 
# these are all "chips"
# Perform some basic text analysis by summarizing the individual words in the 
# product name

#### Examine the words in PROD_NAME to see if there are any incorrect entries
#### such as products that are NOT chips
productWords <- strsplit(unique(as.character(transactionData$PROD_NAME)), " ") %>% 
  unlist() %>%
  as.data.frame()
setnames(productWords, "Words")

#### Remove digits by filter out all those rows that contain digits using 
# str_detect()
productWords <- productWords %>% filter(!(str_detect(Words, "\\d")))

#### Remove special characters
productWords <- productWords %>% filter(str_detect(Words, "[:alpha:]"))

# Let's look at the most common words by counting the number of times a word 
# appears and sorting them by this frequency in a descending order (highest to
# lowest)

#### Sort words from highest to lowest frequency
productWords %>% 
  group_by(Words) %>% summarize(Count = n()) %>% arrange(desc(Count))

# We might want to remove the Salsa because they're not considered chips

#### Remove Salsa products
transactionData <- transactionData %>% 
  dplyr::filter(!grepl('Salsa', PROD_NAME)) 

#### Check if any Salsa products are there
transactionData %>% 
  dplyr::filter(grepl('Salsa', PROD_NAME)) 

#### Let's check some transactions w.r.t. Loyalty Card data. 

# Unique Loyalty Cards
transactionData %>% dplyr::select(LYLTY_CARD_NBR) %>% distinct()

# Top 50 repeat customers
transactionData %>% group_by(LYLTY_CARD_NBR) %>% summarise(count = n()) %>% 
  filter(count > 1) %>% arrange(desc(count)) %>% print(n = 50)

# Multiple Transaction ID's
transactionData %>% group_by(TXN_ID) %>% summarise(count = n()) %>% 
  filter(count > 1) %>% arrange(desc(count)) %>% print(n = 10)

# Check for the repeat transactions per Loyalty Card Number
transactionData %>% group_by(LYLTY_CARD_NBR, TXN_ID) %>% 
  summarize(count = n()) %>% 
  filter(count > 1) %>% arrange(desc(count))

#### Summarize the data to check for nulls and possible outliers
summary(transactionData)

# There's an anomaly where the product quantity is 200. Let's investigate
# this further
transactionData %>% filter(PROD_QTY == 200) 

# Same customer as identified by LYLTY_CARD_NBR 226000. Let's see if this 
# customer had any other transactions
transactionData %>% filter(LYLTY_CARD_NBR == 226000)

# This customer has had no other transactions recorded in the 12-month period, 
# and they've only purchased twice
# Both the times it was 200 packets of chips. This is an obvious anomaly and we can 
# remove this from the data

#### Filter the loyalty number causing the anomaly
transactionData <- transactionData %>% filter(LYLTY_CARD_NBR != 226000)

#### Re-examine the data
summary(transactionData)

# The summary now looks much more coherent (makes more sense). Now, let's look 
# at the number of transactions over time to check for any obvious data issues 
# such as missing data. We can do this by grouping the number of
# transactions by per day, and sorting by date

#### Count the number of transactions by date
transactionData %>% group_by(DATE) %>% summarize(count = n()) %>% arrange((DATE)) 

# There are only 364 dates which means there's a missing date. Let's create a 
# sequence of dates from 1 Jul 2018 (2018-07-01) to 30 Jun 2019 (2019-06-30) 
# create a chart of number of transactions over time to find the missing date.

#### Generate a sequence of dates and join this with the count of transactions by date
allDates <- data.frame(seq(as.Date('2018-07-01'), as.Date('2019-06-30'), by = "day"))
setnames(allDates, "DATE")

#### Join allDates to the transaction data sorted by date using a Right Join.
transactions_by_day <- 
  transactionData %>% 
  group_by(DATE) %>% summarize(count = n()) %>% arrange((DATE)) 

transactions_by_day <- right_join(transactions_by_day, allDates, by = "DATE")

#### Let's plot the transactions over time; this will help us visualize the 
#### missing date
transactions_by_day %>% ggplot(aes(x = DATE, y = count)) + geom_line() +
   labs(x = "Date", y = "Number of transactions", 
        title = "Transactions per day (2018-2019)") +
   theme_bw() + scale_x_date(breaks = "1 month") +
   theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

# We can see that there is a sharp increase in the number of transactions during
# December, and then a break in
# late December. Let's zoom in and check what's happening

#### Filter transaction by day only for December 2018
transactions_by_day %>% filter(month(DATE) == 12) %>% 
          ggplot(aes(x = DATE, y = count)) + geom_line() +
          labs(x = "Date", y = "Number of transactions", 
               title = "Transactions per day in December 2018") +
          theme_bw() + scale_x_date(breaks = "1 day") +
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

# We can see that the increase in sales occurs in the lead-up to Christmas, and 
# that there were 0 transactions on Christmas day itself. 
# This is because shops are closed on Christmas day.

# Now that we are satisfied that the data no longer has outliers, we can move on
# to creating other features such as brand of chips or pack size, from PROD_NAME 
# We will start with pack size.

#### Pack size: we can work this out by taking the digits that are in PROD_NAME
transactionData <- transactionData %>% mutate(PACK_SIZE = parse_number(PROD_NAME))

#### Let's check if the pack sizes look sensible; count pack sizes and group them
transactionData %>% group_by(PACK_SIZE) %>% summarize(Count = n()) %>% 
  arrange(desc(PACK_SIZE))

# The largest pack size is 380, the smallest is 70. This looks reasonable. 

#### Let's plot a histogram of PACK_SIZE, since we know it's a categorical 
#### variable and not a continuous one even though it is numeric
transactionData %>% dplyr::select(PACK_SIZE) %>% 
  ggplot(aes(x = PACK_SIZE)) + geom_histogram(bins = 30)

# Pack sizes look like they have a reasonable distribution, we would d now like 
# to create brands.We can use the first word (Position 1 in string) in PROD_NAME 
# to work out the brand name

#### Brands
BRAND <- as.vector(toupper((word(transactionData$PROD_NAME, 1))))
transactionData <- cbind(transactionData, BRAND)

#### Check transaction volume grouped by Brands
transactionData %>% group_by(BRAND) %>% summarize(Count = n()) %>% 
  arrange(desc(Count))

#### Investigate the BRAND column further to check for duplicated brand names
transactionData %>% dplyr::select(BRAND) %>% distinct()

# Looks like there are some duplicate brand names - such as WW and Woolworths,
# both of which are from the same brand, Woolworths. Let's combine them

#### Clean brand names
transactionData$BRAND[transactionData$BRAND == 'RRD'] <- 'RED'
transactionData$BRAND[transactionData$BRAND == 'WW'] <- 'WOOLWORTHS'
transactionData$BRAND[transactionData$BRAND == 'DORITO'] <- 'DORITOS'
transactionData$BRAND[transactionData$BRAND == 'SNBTS'] <- 'SUNBITES'
transactionData$BRAND[transactionData$BRAND == 'INFZNS'] <- 'INFUZIONS'
transactionData$BRAND[transactionData$BRAND == 'SMITH'] <- 'SMITHS'
transactionData$BRAND[transactionData$BRAND == 'GRAIN'] <- 'GRNWVES'
transactionData$BRAND[transactionData$BRAND == 'NCC'] <- 'NATURAL'

#### Check brand names again
transactionData %>% dplyr::select(BRAND) %>% distinct() 

# reduced by 8; from 28 to 20


#### Visualize the most common brand by total sales
transactionData %>% group_by(BRAND) %>% summarize(sales = sum(PROD_QTY)) %>%
  arrange(desc(sales)) %>% 
  ggplot(aes(y= fct_reorder(BRAND, sales), x = sales, fill = BRAND)) +
  geom_bar(stat = "identity") +
  theme_cowplot() +
  theme(axis.text.x = element_text(angle = 90), legend.position = "none")  +
  labs(x = "Total Sales", y = "Brand")

#### 3. Examine customer data
str(customerData)

# there are about 72,637 members in the customerData file who are in different
# stages in their lives. 

summary(customerData)

# Let's have a closer look at the LIFESTAGE and PREMIUM_CUSTOMER columns

#### Examining the LIFESTAGE column
customerData %>% dplyr::select(LIFESTAGE) %>% distinct()

#### Group the number of customers by LIFESTAGE
customerData %>% group_by(LIFESTAGE) %>% 
  summarize(Count = n()) %>% 
  distinct() %>%
  arrange(desc(Count))

# The Loyalty Card program is most popular with the Retirees, Older Singles/Couples,
# and Young Singles/Couples

#### Examining the PREMIUM_CUSTOMER column
customerData %>% dplyr::select(PREMIUM_CUSTOMER) %>% distinct()

#### Group the number of customers by PREMIUM_CUSTOMER
customerData %>% group_by(PREMIUM_CUSTOMER) %>%
  summarize(Count = n()) %>%
  distinct() %>%
  arrange(desc(Count))

# The most common customer type is "Mainstream" (29,245)

# Since there aren't any issues with the customerData, we can now join it with 
# transactionData using a left-join of transactionData on customerData
mergedData <- left_join(transactionData, customerData, by = "LYLTY_CARD_NBR")

# The number of rows in both transactionData and mergedData seems to be the same which means the
# join was successful, and no duplicates were created. A left-join takes all the rows from the 
# left-hand table and joins them with matching rows in the right table. If there were any unknown
# LYLTY_CARD_NBR, then there would be NAs. We could have also employed an inner join using 
# inner_join(transactionData, customerData, by = "LYLTY_CARD_NBR")

# Let's check if there are any missing customers whose information did not match 
sum(is.na(mergedData))

# Note: We'll use this dataset ("mergedData") for Task 2

#### 4. Data Analysis on Customer Segments

# The data is now ready for analysis. We can define some metrics of interest to the client:
  # 1. Who spends the most on chips (total sales), describing customer by lifestage,
  #    and how premium their general purchasing behavior is.
  # 2. How many customers are in each segment.
  # 3. How many chips are bought by each customer by segment.
  # 4. What's the average chip price by customer segment.
  # 5. Customer's total spend over the period and total spend for each transaction to 
  #    understand what portion of their grocery is spend on chips (Optional)
  # 6. Proportion of customer in each customer segment overall to compare against the 
  #    mix of customers who purchases chips (Optional)

# Let's start by calculating the "total sales" by LIFESTAGE and PREMIUM_CUSTOMER, and
# plotting the split by these segments to describe which customer segments buys the 
# most chips (contributes the most to chip sales)

#### Total Sales by LIFESTAGE and PREMIUM_CUSTOMER
sales_total <- mergedData %>% group_by(LIFESTAGE, PREMIUM_CUSTOMER) %>% 
  summarize(SALES = sum(TOT_SALES), .groups = "keep")

# most popular LIFESTAGE/PREMIUM_CUSTOMER type by total sales
sales_total %>% arrange(desc(SALES))

#### Create plot for sales_total
p1 <- ggplot(sales_total) + 
  geom_mosaic(aes(weight = SALES, x = product(PREMIUM_CUSTOMER, LIFESTAGE), 
                  fill = PREMIUM_CUSTOMER)) +
  labs(x = "Lifestage", y = "Premium Customer type", title = "
       Proportion of Total Sales") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_fill_brewer(palette="Dark2") 

#### Plot and label with proportion of sales
p1 + geom_text(data = ggplot_build(p1)$data[[1]], aes(x = (xmin + xmax)/2, 
                                                      y = (ymin + ymax)/2,
              label = as.character(paste(round(.wt/sum(.wt), 3)*100, "%"))))

# Looks like most of the contributions made to chip sales is coming from Budget 
# Older Families, Mainstream Young Singles/Couples, and Mainstream Retirees. 
# There is a possibility this could be due to these groups having more customers 
# that buy chips (multidisciplinary)

#### Number of customers by LIFESTAGE and PREMIUM_CUSTOMER
customers <- mergedData %>% 
  group_by(PREMIUM_CUSTOMER, LIFESTAGE) %>% 
  summarize(CUSTOMERS = uniqueN(LYLTY_CARD_NBR), .groups = "keep") %>%
  arrange(desc(CUSTOMERS))

#### Create Plot
p2 <- ggplot(data = customers) + 
  geom_mosaic(aes(weight = CUSTOMERS, x = product(PREMIUM_CUSTOMER, LIFESTAGE), 
                  fill = PREMIUM_CUSTOMER)) +
  labs(x = "Lifestage", y = "Premium Customer type", 
       title = "Proportion of Total Sales") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90)) 

#### Plot and label with proportion of customers
p2 + geom_text(data = ggplot_build(p2)$data[[1]], aes(x = (xmin + xmax)/2, 
                                                      y = (ymin + ymax)/2,
                      label = as.character(paste(round(.wt/sum(.wt), 3)*100, "%")))) +
  scale_fill_brewer(palette="Dark2") 

# There are more Mainstream Young Singles/Couples and Mainstream Retirees who buy chips. 
# This contributes to there being more sales to these customer segments. 
# However this is not a major driver for the Budget Older families Segment.

#### Average number of units per customer by LIFESTAGE and PREMIUM_CUSTOMERS
sales_avg <- mergedData %>% group_by(LIFESTAGE, PREMIUM_CUSTOMER) %>%
             summarise(AVG = sum(PROD_QTY)/uniqueN(LYLTY_CARD_NBR), .groups = "keep")

sales_avg %>% arrange(desc(AVG))

#### Create plot
ggplot(data = sales_avg, aes(weight = AVG, x = LIFESTAGE, fill = PREMIUM_CUSTOMER)) + 
  geom_bar(position = position_dodge()) + 
  labs(x = "Lifestage", y = "Avg units per transaction", title = "Units per customer") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90)) + 
  scale_fill_brewer(palette="Dark2")

# Older and younger families in general buy more chips per customer
# Let's also investigate the average price per unit of chips, bought for each customer
# segment as this is also a driver of total sales

avg_price <- mergedData %>% group_by(LIFESTAGE, PREMIUM_CUSTOMER) %>% 
             summarize(AVG = sum(TOT_SALES)/sum(PROD_QTY), .groups = "keep") %>%
             arrange(desc(AVG))

head(avg_price)

#### Create plot
ggplot(data = avg_price, aes(weight = AVG, x = LIFESTAGE, fill = PREMIUM_CUSTOMER)) +
  geom_bar(position = position_dodge()) +
  labs(x = "Lifestage", y = "Avg price per unit in AU$", title = "Price per unit") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90)) + 
  scale_fill_brewer(palette="Dark2")

# Mainstream Midage Singles/Couples and Mainstream Young Singles/Couples on an average
# pay more for each packet of chips compared to their budget and premium counterparts.
# This could also indicate that premium customer are more likely to buy chips, and only
# for particular occasions, rather than their own consumption. This hypothesis is further
# supported by the fact that there are fewer premium and budget Young Sigles/Couples, and
# Midage Singles/Couples who buy chips compared to their maintream counterparts.

#### Perform an independent t-test between mainstream vs premium and budget midage, and 
#### mainstream vs premium and budget young singles and couples
mergedData <- mergedData %>% mutate(price = TOT_SALES/PROD_QTY)

m1 <- mergedData %>% 
  filter(LIFESTAGE %in% c("YOUNG SINGLES/COUPLES", "MIDAGE SINGLES/COUPLES") 
                            & PREMIUM_CUSTOMER == "Mainstream") %>% dplyr::select(price)

m2 <- mergedData %>% 
  filter(LIFESTAGE %in% c("YOUNG SINGLES/COUPLES", "MIDAGE SINGLES/COUPLES") 
                            & PREMIUM_CUSTOMER != "Mainstream") %>% dplyr::select(price)

t.test(m1, m2, alternative = "greater")

# p-value less that 0.05 implies that the unit price for mainstream, young and mid-age
# singles and couples are significantly higher than there budget and premium counterparts
df <- mergedData %>% 
  mutate(GROUP = if_else(PREMIUM_CUSTOMER == "Mainstream", "Mainstream", 
                         "Non-Mainstream")) %>%
  dplyr::select(GROUP, price)
head(df)

df %>% ggplot(aes(x = price, fill = GROUP)) + 
  geom_histogram( bins = 9) +
  scale_color_brewer(palette="Dark2") +
  scale_fill_brewer(palette="Dark2") + 
  theme_bw() +
  labs(x = "Avg price per unit (AU$)", y = "Count")

#### 5. Deep diving into specific customer segments for insights

# We discovered quite a few interesting insights that we can investigate further.

# We might want to target customer segments that contribute the most to sales, 
# to retain them for further increase in sales. Let's start by looking at Mainstream
# Young Singles/Couples. For example, let's try and find out if they tend to buy a
# particular brand of chips

#### Deep diving into Mainstream, Young Singles/Couples
segment1 <- mergedData %>% 
  filter(LIFESTAGE == "YOUNG SINGLES/COUPLES" & PREMIUM_CUSTOMER == "Mainstream")

other <- mergedData %>% 
  filter(LIFESTAGE != "YOUNG SINGLES/COUPLES" & PREMIUM_CUSTOMER == "Mainstream")

quantity_segment1 <- sum(segment1$PROD_QTY)

quantity_other <- sum(other$PROD_QTY)

targetSegment1 <- segment1 %>% group_by(BRAND) %>% 
                summarise(targetSegment = sum(PROD_QTY)/quantity_segment1) 

targetOther <- other %>% group_by(BRAND) %>% 
                        summarise(other = sum(PROD_QTY)/quantity_other)

brand_proportions <- merge(targetSegment1, targetOther) %>% 
  mutate(affinityToBrand = targetSegment/other) 
 
brand_proportions %>% arrange(desc(affinityToBrand))

# We observe that:
# 1. Mainstream Young Singles/Couples are 23% more likely to buy TYRRELLS compared
  # to rest of the population
# 2. Mainstream Young Singles/Couples are 56% less likely to buy BURGER compared 
  # to rest of the population

# Let's find out if our target segment tends to also buy larger packs of chips
targetSegment1_by_pack <- segment1 %>% 
  group_by(PACK_SIZE) %>% 
  summarise(targetSegment = sum(PROD_QTY)/quantity_segment1)

targetOther_by_pack <- other %>% 
  group_by(PACK_SIZE) %>% 
  summarise(other = sum(PROD_QTY/quantity_other))

pack_proportions <- merge(targetSegment1_by_pack, targetOther_by_pack) %>% 
  mutate(affinityToPack = targetSegment/other)

pack_proportions %>% arrange(desc(affinityToPack))

# Mainstream Young Singles/Couples are 25% more likely to buy 270g pack of chips
# compared to the rest of the population. Which brands sell packs of this size?
mergedData %>% filter(PACK_SIZE == 270) %>% dplyr::select(PROD_NAME) %>% distinct()

# Twisties are the only brand that sell products of this size. This perhaps therefore
# reflects a higher likelihood of purchasing Twisties products
