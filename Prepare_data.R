library(tidyverse)
library(scales)
library(RSQLite)
library(dbplyr)
library(RPostgres)
library(dplyr)
library(readr)
library(sqldf)
library(readr)
library(mice)

#crsp_monthly <- read_csv("Data/crsp-monthly.csv")
#compustat2 <-read_csv("Data/compustat2.csv)
#import all the data sets in the data folder
#Accessing WRDS 

wrds <- dbConnect(
  Postgres(),
  host = 'wrds-pgdata.wharton.upenn.edu',
  dbname = 'wrds',
  port = 9737,
  sslmode = 'require',
  user = "endi1222",
  password = "12345@FIE453A"
)

res <- dbSendQuery(wrds, "select cusip,permno,date,hsiccd
                   from crsp.msf
                   where date between '2011-12-31'
                   and '2023-05-31'")
industry <- dbFetch(res, n=-1)
dbClearResult(res)



msf_db <- tbl(wrds, in_schema('crsp','msf'))
msenames_db <- tbl(wrds, in_schema("crsp", "msenames"))
msedelist_db <- tbl(wrds, in_schema("crsp", "msedelist"))

start_date <- as.Date('2011-12-31')
end_date <- as.Date('2023-05-31')

industry <- industry %>%
  mutate(industry = case_when(
    hsiccd >= 1 & hsiccd <= 999 ~ "Agriculture",
    hsiccd >= 1000 & hsiccd <= 1499 ~ "Mining",
    hsiccd >= 1500 & hsiccd <= 1799 ~ "Construction",
    hsiccd >= 2000 & hsiccd <= 3999 ~ "Manufacturing",
    hsiccd >= 4000 & hsiccd <= 4899 ~ "Transportation",
    hsiccd >= 4900 & hsiccd <= 4999 ~ "Utilities",
    hsiccd >= 5000 & hsiccd <= 5199 ~ "Wholesale",
    hsiccd >= 5200 & hsiccd <= 5999 ~ "Retail",
    hsiccd >= 6000 & hsiccd <= 6799 ~ "Finance",
    hsiccd >= 7000 & hsiccd <= 8999 ~ "Services",
    hsiccd >= 9000 & hsiccd <= 9999 ~ "Public",
    TRUE ~ "Missing"
  ))

filtered_compustat <- c('GVKEY','LPERMNO','datadate','fyearq','fqtr','fyr','dlcq','dlttq','atq','teqq','xintq','chq','revtq','ppegtq','niq','cogsq','xoprq','cshoq','epsf12','oiadpq','dpq')
df_compustat <- compustat2[filtered_compustat]


df_compustat['Total_Debt_Ratio'] = (df_compustat$dlcq+df_compustat$dlttq)/df_compustat$atq
df_compustat['Debt_Equity_Ratio'] = (df_compustat$atq - df_compustat$teqq)/ df_compustat$teqq
df_compustat['Equity_Multiplier'] = df_compustat$atq/df_compustat$teqq
df_compustat['Longterm_Debt_Ratio'] = df_compustat$dlttq/(df_compustat$dlttq+df_compustat$teqq)
df_compustat['Cash_To_Debt_Coverage'] = df_compustat$chq/ (df_compustat$dlttq+df_compustat$dlcq)

df_compustat['Fixed_Asset_Turnover'] = df_compustat$revtq/df_compustat$ppegtq
df_compustat['Total_Asset_Turnover'] = df_compustat$revtq/df_compustat$atq

df_compustat['Profit_Margin'] = df_compustat$niq/df_compustat$revtq
df_compustat['Basic_Earning_Power'] = (df_compustat$revtq-df_compustat$cogsq-df_compustat$xoprq)/df_compustat$atq
df_compustat['ROA'] = df_compustat$niq/df_compustat$atq
df_compustat['ROE'] = df_compustat$niq/df_compustat$teqq

df_compustat <- df_compustat %>% arrange(LPERMNO,datadate,fqtr)
df_compustat <- df_compustat %>% mutate(previous_quarter_epsf12=lag(epsf12,n=1))
df_compustat['EPS_Growth'] = df_compustat$epsf12/df_compustat$previous_quarter_epsf12 - 1 



#Merge with CRSP

crsp_monthly$PRC <- abs(crsp_monthly$PRC)
crsp_monthly$RET <- as.numeric(crsp_monthly$RET)
crsp_monthly$RETX <- as.numeric(crsp_monthly$RETX)

## yearmon to merge with Compustat
to.year <- function (x) {
  x %/% 10000 - 1900
}

to.month <- function (x) {
  (x %/% 100) %% 100 - 1  
}

to.yearmon <- function (x) {
  to.year(x) * 12 + to.month(x)
}

names(df_compustat)[ names(df_compustat) == "LPERMNO" ] = "PERMNO"
dups <- duplicated(df_compustat[,1:3])
df_compustat <- df_compustat[!dups,]

crsp_monthly$yearmon <- to.yearmon(crsp_monthly$date)
df_compustat$yearmon <- to.yearmon(df_compustat$datadate)

crsp_monthly$MARKETCAP <- crsp_monthly$PRC * crsp_monthly$SHROUT
df_compustat = merge(df_compustat, crsp_monthly[c("PERMNO", "yearmon", "MARKETCAP")], by=c("PERMNO", "yearmon"), all.x = TRUE)
crsp_monthly$MARKETCAP <- NULL

## rename
names(df_compustat)[ names(df_compustat) == "PERMNO" ] = "PERMNO2"
names(df_compustat)[ names(df_compustat) == "yearmon" ] = "yearmon2"

merged <- sqldf("select * from crsp_monthly, df_compustat where crsp_monthly.PERMNO = df_compustat.PERMNO2 and crsp_monthly.yearmon - df_compustat.yearmon2 between 4 and 6 order by PERMNO, date")

merged$yearmon <- NULL
merged$yearmon2 <- NULL
merged$PERMNO2 <- NULL

#Add some more ratios and remove columns
merged['Price_to_sales'] = (merged$PRC*merged$cshoq)/merged$revtq
merged['EV_to_sales'] = (merged$PRC*merged$cshoq+merged$dlcq+merged$dlttq-merged$chq)/(merged$oiadpq+merged$dpq)
merged['PE_Ratio'] = merged$PRC/merged$epsf12

#Drop NA/ infinity values
print("Count of missing values by column wise") 
sapply(merged, function(x) sum(is.na(x)))

#Dealing with NAs values.
merged$ppegtq <- NULL #drop columns that na>50%

df<-na.omit(merged)

#Merge with industry and macro data(interest_rate,gpa,inflation)
gdp <- read_csv("Data/gdp.csv")
gdp <- subset(gdp, select =c(YEAR,QUARTER,GDP))
inflation <- read_csv("Data/inflation.csv")
inflation <-subset(inflation,select = c(YEAR, QUARTER,Inflation_rate))
interest <- read_csv("Data/interest.csv")
interest<-subset(interest,select = c(YEAR, QUARTER,Interest_rate))
industry<- subset(industry, select = c(permno,industry))
industry<- distinct(industry)

final_merged <- sqldf("select * from df, industry where df.PERMNO = industry.permno order by PERMNO, date")
final_merged$permno <- NULL
final_merged <- sqldf("select * from final_merged, gdp where final_merged.fyearq = gdp.YEAR and final_merged.fqtr=gdp.QUARTER order by PERMNO,date")
final_merged$YEAR <- NULL
final_merged$QUARTER <- NULL
final_merged <- sqldf("select * from final_merged, inflation where final_merged.fyearq = inflation.YEAR and final_merged.fqtr=inflation.QUARTER order by PERMNO,date")
final_merged$YEAR <- NULL
final_merged$QUARTER <- NULL
final_merged <- sqldf("select * from final_merged, interest where final_merged.fyearq = interest.YEAR and final_merged.fqtr=interest.QUARTER order by PERMNO,date")
final_merged$YEAR <- NULL
final_merged$QUARTER <- NULL


final_merged$GVKEY<-NULL
#Encode Industry column
final_merged$industry<- as.numeric(factor(final_merged$industry))
#remove infinity values
final_merged <- final_merged[is.finite(rowSums(final_merged)), ]

#Create one more column to store the average of PE ratio group by industry
final_df <- final_merged %>%
  group_by(fyearq,fqtr,industry) %>%
  mutate(avg_pe = mean(PE_Ratio, na.rm = TRUE)) %>%
  ungroup()
#Create one binary variable to evaluate the stock
final_df$objective = as.numeric(final_df$PE_Ratio > final_df$avg_pe)

#Drop PE_Ratio and avg_pe columns as our objective column is comparing these two
final_df <- subset(final_df, select = -c(PE_Ratio,avg_pe))

train_start <- 20110131
train_end <- 20181231
valid_start <- 20190101
valid_end <- 20191231
test_start <- 20200101
test_end <- 20201231

df_train <- final_df[final_df$date >= train_start & final_df$date <= train_end, ]
df_valid <- final_df[final_df$date >= valid_start & final_df$date <= valid_end, ]
df_test <- final_df[final_df$date >= test_start & final_df$date <= test_end, ]


