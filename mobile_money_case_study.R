#set working directory
setwd("/home/omotolaolasope/Desktop/Data_Analysis/mobile_money_case_study/")

## Installing and loading common packages and libraries
library(tidyverse)
library(dplyr)
library(naniar)
library(gridExtra)
library(grid)
library(lessR)

#started the data cleaning process in google sheets
#created seperate columns for all account types and companies
#rows with account types were set to true while those without were set to false
#imported the file to R and converted any logical columns to character type
mmdf <- read.csv('mobile_money_data.csv')

mmdf <- data.frame(lapply(mmdata, function(x) if(is.logical(x)) { 
  return(as.character(x))
} else {  
  return(x) 
}
), stringsAsFactors=FALSE)

## Change logical columns to character
mmdf3$mobile_money <- as.character(mmdf3$mobile_money)
mmdf3$bank_account <- as.character(mmdf3$bank_account)
mmdf3$vsla_account <- as.character(mmdf3$vsla_account)
mmdf3$sacco_account <- as.character(mmdf3$sacco_account)
mmdf3$online_bank_account <- as.character(mmdf3$online_bank_account)
mmdf3$no_account <- as.character(mmdf3$no_account)

#changed all cells in account types with "FALSE" to blank cells

mmdf$mobile_money <- gsub('FALSE', '', mmdf$mobile_money)
mmdf$bank_account <- gsub('FALSE', '', mmdf$bank_account)
mmdf$vsla_account <- gsub('FALSE', '', mmdf$vsla_account)
mmdf$sacco_account <- gsub('FALSE', '', mmdf$sacco_account)
mmdf$online_bank_account <- gsub('FALSE', '', mmdf$online_bank_account)
mmdf$no_account <- gsub('FALSE', '', mmdf$no_account)

#exported resulting dataframe to csv file to continue the cleaning process in Sheets

write.csv(mmdf,"/home/omotolaolasope/Desktop/Data_Analysis/mobile_money_case_study/mmdf.csv", row.names = FALSE)

#merged all rows with identical hhid together
#deleted columns that are not needed
#import data file back into R Studio for analysis
mmdf <- read.csv('mmdf.csv')

## Exploring the data
head(mmdf)
colnames(mmdf)

## visualize missings per variable using the 'naniar' library
gg_miss_var(mmdf) +
  # add labels
  labs(
    title = "Numbers of missing values per variable",
    y = "Count of Missing Values") +
  # make axis text bold
  theme(axis.text.y = element_text(face = "bold"))

## Rename some columns
colnames(mmdf)[10] ="hh_location" #urban
colnames(mmdf)[22] ="undrstd_terms_nd_cond"
colnames(mmdf)[24] ="taken_mm_loan"
colnames(mmdf)[25] ="ntwrk_unavailable_mm_txn"
colnames(mmdf)[26] ="clear_about_txn_fees"
colnames(mmdf)[27] ="txn_ever_failed"
colnames(mmdf)[28] ="agent_ever_not_had_enough_cash"
colnames(mmdf)[29] ="have_copy_of_terms_nd_cond"
colnames(mmdf)[30] ="undrstd_hw_nd_whr_to_complain"
colnames(mmdf)[31] ="had_issue_reslvd_after_complaint"
colnames(mmdf)[32] ="understand_data_mm_providers_collect"
colnames(mmdf)[33] ="been_victim_of_fraud"

colnames(mmdf)

#get number of unique values per column
sapply(mmdf, function(x) n_distinct(x))
n_distinct(mmdf$hhid)
n_distinct(mmdf$account_type)
unique(mmdf$account_type)
unique(mmdf$highest_grade_completed)
unique(mmdf$mm_account_telco_main)
unique(mmdf$agent_trust)

#exported edited dataframe to csv file to further the cleaning process in Sheets
write.csv(mmdf,"/home/omotolaolasope/Desktop/Data_Analysis/mobile_money_case_study/mmdf2.csv", row.names = FALSE)

#import data file back into R Studio for analysis
mmdf3 <- read.csv('mmdf3.csv')
raw_df <- read.csv('mobile_money_data.csv')

#changed all cells in account types with "NA" back to 'FALSE'

mmdf3$mobile_money <- mmdf3$mobile_money %>% replace_na('FALSE')
mmdf3$bank_account <- mmdf3$bank_account %>% replace_na('FALSE')
mmdf3$vsla_account <- mmdf3$vsla_account %>% replace_na('FALSE')
mmdf3$sacco_account <- mmdf3$sacco_account %>% replace_na('FALSE')
mmdf3$online_bank_account <- mmdf3$online_bank_account %>% replace_na('FALSE')
mmdf3$no_account <- mmdf3$no_account %>% replace_na('FALSE')

#create pie chart of distribution of account types
#install.packages("lessR")
#library(lessR)

#acct_type <- data.frame(acct = mm_data2$account_type)
#PieChart(acct, hole = 0, values = "%", data = acct_type, main = "")

#create histogram of distribution of account types
p1 <- ggplot(aes(x = mobile_money, fill = mobile_money), data = mmdf3) +
  geom_histogram(stat="count")

p2 <- ggplot(aes(x = online_bank_account, fill = online_bank_account), data = mmdf3) +
  geom_histogram(stat="count")

p3 <- ggplot(aes(x = bank_account, fill = bank_account), data = mmdf3) +
  geom_histogram(stat="count")

p4 <- ggplot(aes(x = vsla_account, fill = vsla_account), data = mmdf3) +
  geom_histogram(stat="count")

p5 <- ggplot(aes(x = sacco_account, fill = sacco_account), data = mmdf3) +
  geom_histogram(stat="count")

p6 <- ggplot(aes(x = no_account, fill = no_account), data = mmdf3) +
  geom_histogram(stat="count")

grid.arrange(p1, p4, p5, p3, p6, p2, ncol=3, top = 'Count of Account Types')

#combined
count_acct_types <- raw_df %>% 
  group_by(account_type) %>% 
  summarise(n = n())

ggplot(aes(x = reorder(account_type, -n), y = n, fill = account_type), data = count_acct_types) +
  geom_bar(stat="identity") +
  labs(x = 'account type', y = 'count', title="Count of Account Types")


##Creating two dummy variables for whether each participant is: i) financially excluded and ii) digitally financially included.

mmdf2 <- transform(mmdf3, fin_excl = ifelse(no_account == "TRUE", TRUE, FALSE))

mmdf2 <- transform(mmdf2, dig_fin_incl = ifelse((mobile_money == "TRUE") | (online_bank_account =="TRUE"), TRUE, FALSE))

#overall rates of financial exclusion
rate_fin_excl <- mmdf2 %>% 
  group_by(fin_excl) %>% 
  summarise(n = n())

rate_fin_excl$fin_excl_percen <- 100*(rate_fin_excl$n/sum(rate_fin_excl$n))

ggplot(aes(x = reorder(fin_excl, fin_excl_percen), y = fin_excl_percen, fill = fin_excl), data = rate_fin_excl) +
  geom_bar(stat="identity") +
  labs(x = 'financially excluded', y = 'percent', title="overall rate of financial exclusion")

#create pie chart of rates of financial exclusion
pie(rate_fin_excl$fin_excl_percen,
    labels = paste0(round(rate_fin_excl$fin_excl_percen, 2), "%"),
    main = 'overall rate of financial exclusion')

#overall rates of digital financial inclusion 
dig_fin_inclu <- mmdf2 %>% 
  group_by(dig_fin_incl) %>% 
  summarise(n = n())

dig_fin_inclu$dig_fin_inclu_percen <- 100*(dig_fin_inclu$n/sum(dig_fin_inclu$n))

ggplot(aes(x = reorder(dig_fin_incl, dig_fin_inclu_percen), y = dig_fin_inclu_percen, fill = dig_fin_incl), data = dig_fin_inclu) +
  geom_bar(stat="identity") +
  labs(x = 'digital finance included', y = 'percent', title="overall rate of digital financial inclusion")

#create pie chart of rates of financial exclusion
pie(dig_fin_inclu$dig_fin_inclu_percen,
    labels = paste0(round(dig_fin_inclu$dig_fin_inclu_percen, 2), "%"),
    main = 'overall rate of digital financial inclusion')


## Describe in a few short paragraphs how the mobile money market is divided between the three companies. Include at least one chart or table to illustrate your findings.
#seperate columns with the telcos for easier analysis
col_telcos <- mmdf2 %>% 
  select(mm_account_telco_1, mm_account_telco_2, mm_account_telco_3)

#group the data by the telcos and get the count of each
count_mm_telco <- mmdf2 %>% 
  group_by(mm_account_telco_1, mm_account_telco_2, mm_account_telco_3) %>% 
  summarise(n = n())

view(count_mm_telco)

#get the sum of mobile money tel_co users for each company
sum_mm_telco_users <- data.frame(company = c('company A', 'company B', 'company C'),
                 total_users=c(sum(col_telcos == "Company_A"), sum(col_telcos == "Company_B"), sum(col_telcos == "Company_C")))

ggplot(aes(x = company, y = total_users), data = sum_mm_telco_users) +
  geom_bar(stat = 'identity') +
  labs(x = 'mobile money company', y = 'mobile money customers', title="Mobile money market between the three companies")

#From the resulting table, company A has the most customers with 597 mobile money customers, company B has 570 customers and company C has the least with 85 customers

#Is there a difference in the share of customers who have experienced failed mobile money transactions in rural and urban villages? If so, is it statistically significant? Explain your findings including any assumptions and limitations.
#seperate columns for easier analysis
df.txn <- mmdf2 %>% 
  select(hh_location, txn_ever_failed, had_issue_reslvd_after_complaint)

#know the count of customers at each location
count_hhlocation <- df.txn %>% 
  group_by(hh_location) %>% 
  summarise(number_of_residents = n())

View(count_hhlocation)

ggplot(aes(x = hh_location), data = df.txn) +
  geom_histogram(stat="count")

#customers in the rural areas are significantly more than those in the urban areas

#determine the percentage of failed transactions per location
df.txn <- mmdf2 %>% 
  select(hh_location, txn_ever_failed, had_issue_reslvd_after_complaint)

grp_txn <- df.txn %>% 
  group_by(hh_location, txn_ever_failed) %>% 
  summarise(number = n())

#Filter Rural transactions
rural_txns <- grp_txn %>% 
  filter(hh_location == 'Rural')

#get percentage and create new column for it
rural_txns$txn_percen <- round(100*(rural_txns$number/sum(rural_txns$number)), 2)
print(rural_txns)

#graph of rural transactions
ruralG <- ggplot(aes(x = txn_ever_failed, y = txn_percen, fill = txn_ever_failed), data = rural_txns) +
  geom_bar(stat = 'identity') +
  ylim(0, 100) +
  labs(x = 'Has Transaction Ever Failed', y = 'Percentage (%)', title="Graph showing failed transaction percentages in Rural location") +
  scale_fill_manual(values = c("#ADD8E6", "#F93B19", "green"),
                    labels = c("N/A", "No","Yes")) +
  guides(fill=guide_legend(title="Has transaction \never failed?"))

#Filter Urban transactions
urban_txns <- grp_txn %>% 
  filter(hh_location == 'Urban')

#get percentage and create new column for it
urban_txns$txn_percen <- round(100*(urban_txns$number/sum(urban_txns$number)), 2)
View(urban_txns)

#graph of urban transactions
urbanG <- ggplot(aes(x = txn_ever_failed, y = txn_percen, fill = txn_ever_failed), data = urban_txns) +
  geom_bar(stat = 'identity') +
  ylim(0, 100) +
  labs(x = 'Has Transaction Ever Failed', y = 'Percentage (%)', title="Graph showing failed transaction percentages in Urban location") +
  scale_fill_manual(values = c("#ADD8E6", "#F93B19", "green"),
                    labels = c("N/A", "No","Yes")) +
  guides(fill=guide_legend(title="Has transaction \never failed?"))

#compare percentages for both regions
grid.arrange(ruralG, urbanG, ncol=2, top = 'Compare percentage Transactions by Location')

#There is a significant difference in the share of customers who have experienced failed mobile money transactions in rural and urban villages. 32% of transactions
#in urban locations have failed before while 16% of transactions in rural areas have failed before. Nevertheless, there are limitations to this figures due to the
#number of customers that did not give records of if their transactions failed or not.


#What variables are good predictors that someone will cancel their mobile money account?
#filter data by mobile money accounts that have been cancelled
mm_acc_cancled <- mmdf2 %>% 
  filter(mm_account_cancelled == 'yes')

mmTrue <- mmdf2 %>% 
  filter(mobile_money == 'TRUE')

mm_acc_cancled %>% 
  group_by(mm_trust) %>% 
  summarise(n = n())

ggplot(aes(x = district, fill = hh_location), data = mmdf2) +
  geom_histogram(stat = 'count') +
  scale_fill_manual(values = c("brown", "#AB9B98")) +
  labs(title = 'location of customers per district') +
  guides(fill=guide_legend(title="Customer location"))

ggplot(aes(x = district, fill = district), data = mm_acc_cancled) +
  geom_histogram(stat = 'count') +
  scale_fill_manual(values = c("green", "#F93B19", "#AB9B98")) +
  labs(title = 'number of mobile money accounts cancelled by district')

ggplot(aes(x = mm_trust, fill = agent_trust), data = mmdf2) +
  geom_histogram(stat = 'count') +
  labs(title = 'Trust Levels for Mobile Money and Mobile Money Agents', x = 'Do you trust mobile money?') +
  guides(fill=guide_legend(title="Do you trust \nmobile money agents?")) +
  scale_fill_manual(values = c("#ADD8E6", "#F93B19", "green"),
                    labels = c("N/A", "No","Yes"))

mm_users <- ggplot(aes(x = mm_trust, fill = agent_trust), data = mmTrue) +
  geom_histogram(stat = 'count') +
  ylim(0, 600) +
  scale_fill_manual(values = c("#ADD8E6", "#F93B19", "green"),
                    labels = c("N/A", "No","Yes")) +
  guides(fill=guide_legend(title="Do you trust \nmobile money agents?")) +
  labs(title = 'Graph of customers using mobile money \nwho dont trust mobile money and mobile money agents', x = 'trust mobile money?')

mm_canc <- ggplot(aes(x = mm_trust, fill = agent_trust), data = mm_acc_cancled) +
  geom_histogram(stat = 'count') +
  ylim(0, 600) +
  scale_fill_manual(values = c("#ADD8E6", "#F93B19", "green"),
                    labels = c("N/A", "No","Yes")) +
  guides(fill=guide_legend(title="Do you trust \nmobile money agents?")) +
  labs(title = 'Graph of customers who cancelled their mobile money account and \ndont trust mobile money and mobile money agents', x = 'trust mobile money?')

#compare for both mm users and those cancelled
grid.arrange(mm_users, mm_canc, ncol=2, top = 'Compare mobile money customers that dont trust mobile money and mobile money agents')

#From the data provided, all customers in District B are in rural locations while most customers in District C are in rural locations.
#District A had the least number of mobile money accounts cancelled while district B had the highest number of mobile money accounts cancelled.
#majority of customers don't trust mobile money or mobile money agents. However, only a few (179) cancelled their mobile money accounts.
#Of the 179 customers who cancelled their mobile money accounts, 151 don't trust the mobile money platform (about 84.36%).

#Trust in mobile money and mobile money agents are variables that are good predictors that someone will cancel their mobile money account

#Discuss what causes a customer to stop using their mobile money account including how strong the evidence is.

#During analysis, it was gathered that a vast majority of customers prefer cash.
#Furthermore, majority do not have a copy of the mobile money terms and conditions and do not understand what data mobile money providers collect about them. 
#Thus, one can assume that customers lack of trust in data collection and conditions of mobile money use, 
#coupled with a preference for the cash option are factors that can cause a customer to stop using their mobile money account 

ggplot(aes(x = prefer_cash), data = mmdf2) +
  geom_histogram(stat = 'count') +
  labs(x = 'prefer cash?')

ggplot(aes(x = have_copy_of_terms_nd_cond, fill = understand_data_mm_providers_collect), data = mmdf2) +
  geom_histogram(stat = 'count') +
  scale_fill_manual(values = c("white", "#F93B19", "#AB9B98"),
                    labels = c("N/A", "No","Yes")) +
  labs(x = 'Do you have a copy of the mobile money terms and conditions?') +
  guides(fill=guide_legend(title="Do you understand what\n data mobile money\n providers collect about you?"))



qplot(data = mmdf2, x = mobile_money)

mm_customers <- mmdf2 %>% 
  group_by(mobile_money) %>% 
  summarise(number = n())

mm_customers$mm_percen <- 100*(mm_customers$number/sum(mm_customers$number))

ggplot(aes(x = mobile_money, y = mm_percen, fill = number), data = mm_customers) +
  geom_bar(stat="identity") +
  ylim(0, 100) +
  labs(x = 'mobile money user', y = 'percentage (%)', title="Percentage of Mobile Money Users Across Sample Data")

pie(mm_customers$mm_percen,
    labels = paste0(round(mm_customers$mm_percen, 2), "%"),
    main = 'Percentage of Mobile Money Users Across Sample Data',
    )

########
ggplot(aes(x = hh_location, fill = mobile_money), data = df.mm_txn) +
  geom_histogram(stat="count")



col_mm_users <- mmdf2 %>% 
  select(mobile_money, hh_location)

df.mm_users <- data.frame(location = c('Rural', 'Urban'),
                           total_mm_users=c(sum((col_mm_users$mobile_money == "TRUE" & col_mm_users$hh_location == "Rural")),
                                               sum((col_mm_users$mobile_money == "TRUE" & col_mm_users$hh_location == "Urban"))),
                           total_customers=c(sum(col_mm_users$hh_location == "Rural"),
                                             sum(col_mm_users$hh_location == "Urban")))

df.mm_users$mm_users_percen <- round(100*(df.mm_users$total_mm_users/df.mm_users$total_customers), 2)

print(df.mm_users)

ggplot(aes(x = location, y = mm_users_percen), data = df.mm_users) +
  geom_bar(stat = 'identity') +
  ylim(0, 100) +
  labs(x = 'mobile money users percentage/location', y = 'percent (%)', title="Percentage of Mobile Money Users Per Location")

########

ggplot(aes(x = understand_data_mm_providers_collect, fill = have_copy_of_terms_nd_cond), data = mmdf2) +
  geom_histogram(stat = 'count') +
  scale_fill_manual(values = c("white", "#F93B19", "#AB9B98"),
                    labels = c("N/A", "No","Yes")) +
  labs(x = 'Do you understand what data mobile money providers collect about you?') +
  guides(fill=guide_legend(title="Do you have a copy of the \nmobile money terms and conditions?"))

###################two-proportions z-test######################

#we have two locations:
#Rural: n = 940
#Urban: n = 265

#The number of failed mobile money transactions in each location is as follow:
#Urbans: n = 265, 85 failed mobile money transactions, pU = 85/265 = 32.08
#Rural: n = 940, 155 failed mobile money transactions, pR = 155/940 = 16.49

#In this setting:
#The overall proportion of failed mobile money transactions is p = frac(155+85)940+265 = 19.92
#The overall proportion of successful mobile money transactions is q = 1−p = 80.08

#The test statistic (also known as z-test) can be calculated as follow:
  
#z = ((pU - pR)/sqrt((pq/nU) + (pq/nR)))

#where,
#pU is the proportion of failed mobile money transactions observed in Urban with size nU
#pR is the proportion of failed mobile money transactions observed in Rural with size nR
#p and q are the overall proportions

#using R

prop.test(x = c(85, 155), n = c(265, 940),
          correct = FALSE)





#####


