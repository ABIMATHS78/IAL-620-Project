# Whenever you see libraries listed at the top of code, you should always make sure that you have them installed. To install a library--or a 'package' as they are often called--use the install.packages() function in R.
library(rvest)
library(tidyverse)
library(httr)
library(purrr)

# Read Fortune 500 Companies stock symbol data
fortune500 <- read_csv("SP500.csv")

fortune500_list <- fortune500$Symbol
head(fortune500_list)

fortune500_list10 <- fortune500_list[1:10]

# slow down requests from server to avoid blocking
throttled_GET <- slowly(
  ~ GET(.),
  rate = rate_delay(3)
)


# This is the primary URL from which you will extract other URLs containing content of interest
base_url <- "https://stockanalysis.com/stocks/"

# loop through a list of URLs
url_list <-  c("/financials/",
               "/company/",
               "/financials/cash-flow-statement/",
               "/financials/ratios/",
               "/financials/balance-sheet/")

for(url in url_list){
  print(url)
}


# Creates an empty vector that will be filled data by the 'for loop' below
company_name.list <- vector()
share_outstanding.list <- vector()
revenue.list <- vector()
net_income.list <- vector()
operating_exp.list <- vector()
founded.list <- vector()
industry.list <- vector()
sector.list <- vector()
employee_nos.list <- vector()
ceo.list <- vector()
capital_exp.list <- vector()
market_cap.list <- vector()
long_term_debt.list <- vector()
common_stock_value.list <- vector()
financial_perf.list <- vector()
analyst_forecast.list <- vector()
top_news.list <- vector()

for(company in fortune500_list10){
  print(company)
  print(paste0(base_url, company, url_list[1]))
  html_elem_financials <- content(throttled_GET(paste0(base_url, company, url_list[1])))
  html_elem_company <- read_html(paste0(base_url, company, url_list[2]))
  html_elem_cashflow <- read_html(paste0(base_url, company, url_list[3]))
  html_elem_ratios <- read_html(paste0(base_url, company, url_list[4]))
  html_elem_balancesheet <- read_html(paste0(base_url, company, url_list[5]))
  
  #Collects text content from pages
  company_name.add <- html_nodes(html_elem_financials, xpath='//title') %>%
    html_text() %>% 
    {if(length(.) == 0) NA else .}
  company_name.add <- gsub(" \\(", "", str_extract(company_name.add, "([A-Za-z0-9 ])+ \\("))
  
  share_outstanding.add <- html_nodes(html_elem_financials, xpath='//span[text()="Shares Outstanding (Basic)"]/following::td[1]') %>%
    html_text() %>% 
    {if(length(.) == 0) NA else .}
  
  revenue.add <- html_nodes(html_elem_financials, xpath='//span[text()="Revenue"]/following::td[1]') %>%
    html_text() %>% 
    {if(length(.) == 0) NA else .}
  
  net_income.add <- html_nodes(html_elem_financials, xpath='//span[text()="Net Income"]/following::td[1]') %>%
    html_text() %>% 
    {if(length(.) == 0) NA else .}
  
  operating_exp.add <- html_nodes(html_elem_financials, xpath='//span[text()="Operating Expenses"]/following::td[1]') %>%
    html_text() %>% 
    {if(length(.) == 0) NA else .}
  
  founded.add <- html_nodes(html_elem_company, xpath='//td[text()="Founded"]/following::td[1]') %>%
    html_text() %>% 
    {if(length(.) == 0) NA else .}
  
  industry.add <- html_nodes(html_elem_company, xpath='//td[text()="Industry"]/following::td[1]') %>%
    html_text() %>% 
    {if(length(.) == 0) NA else .}
  
  sector.add <- html_nodes(html_elem_company, xpath='//td[text()="Sector"]/following::td[1]') %>%
    html_text() %>% 
    {if(length(.) == 0) NA else .}
  
  employee_nos.add <- html_nodes(html_elem_company, xpath='//td[text()="Employees"]/following::td[1]') %>%
    html_text() %>% 
    {if(length(.) == 0) NA else .}
  
  ceo.add <- html_nodes(html_elem_company, xpath='//td[text()="CEO"]/following::td[1]') %>%
    html_text() %>% 
    {if(length(.) == 0) NA else .}
  
  capital_exp.add <- html_nodes(html_elem_cashflow, xpath='//span[text()="Capital Expenditures"]/following::td[1]') %>%
    html_text() %>% 
    {if(length(.) == 0) NA else .}
  
  market_cap.add <- html_nodes(html_elem_ratios, xpath='//a[text()="Market Capitalization"]/following::td[1]') %>%
    html_text() %>% 
    {if(length(.) == 0) NA else .}
  
  long_term_debt.add <- html_nodes(html_elem_balancesheet, xpath='//span[text()="Long-Term Debt"]/following::td[1]') %>%
    html_text() %>% 
    {if(length(.) == 0) NA else .}
  
  common_stock_value.add <- html_nodes(html_elem_balancesheet, xpath='//span[text()="Common Stock"]/following::td[1]') %>%
    html_text() %>% 
    {if(length(.) == 0) NA else .}
  
  
  company_name.list <- c(company_name.list, company_name.add)
  # company_name.list <- gsub(" Financial Statements: Income | Stock Analysis", "", company_name.list)
  # gsub(" \\(", "", str_extract("3M Company (MMM)|", "([A-Za-z0-9 ])+ \\("))
  share_outstanding.list <- c(share_outstanding.list, share_outstanding.add)
  revenue.list <- c(revenue.list, revenue.add)
  net_income.list <- c(net_income.list, net_income.add)
  operating_exp.list <- c(operating_exp.list, operating_exp.add)
  founded.list <- c(founded.list, founded.add)
  industry.list <- c(industry.list, industry.add)
  sector.list <- c(sector.list, sector.add)
  employee_nos.list <- c(employee_nos.list, employee_nos.add)
  ceo.list <- c(ceo.list, ceo.add)
  
}

# share_outstanding.list
# revenue.list

# Using tibble, the list of URLs is combined with the text scraped from each URL to create a dataframe for our combined dataset
scrape.data <- tibble('Company Name'=company_name.list, 'Shares Outstanding'=share_outstanding.list, 'Revenue'=revenue.list, 'Net Income'=net_income.list, 'Operating Expenses'=operating_exp.list, 'Founded'=founded.list, 'Industry'=industry.list, 'Sector'=sector.list, 'Number of Employees'=employee_nos.list, 'CEO'=ceo.list)

# Save dataframe as a CSV file
write.csv(scrape.data, 'stockdata.csv', row.names = FALSE)












######################################################################################################################
# //p/following-sibling::div[@class="mt-1 text-sm text-gray-700 sm:order-1 sm:mt-0"]/preceding-sibling::p
# //p/preceding-sibling::h3



result <- html_nodes(read_html('https://stockanalysis.com/stocks/aapl/'), xpath='//p/preceding-sibling::h3') %>%
  html_text()
result <- result[1:10] 
result <- result %>% 
  str_c(collapse = " ")
result
