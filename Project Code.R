#Data Quality
#Task: Draft a document to the client identifying the data quality issues and strategies to mitigate these issues. Refer to 'Data Quality Framework Table' and resources below for criteria and dimensions which you should consider.

library(readxl)

#1. Import dataset
#Four datasets were included for this analysis.

Transactions<- read_excel("C:/Users/Amar/Documents/GitHub/KPMG_Virtual_Internship/data/KPMG_VI_New_raw_data_update_final.xlsx", 
                          sheet = "Transactions", skip = 1)
CustomerList <- read_excel("C:/Users/Amar/Documents/GitHub/KPMG_Virtual_Internship/data/KPMG_VI_New_raw_data_update_final.xlsx", 
                           sheet = "NewCustomerList", skip = 1)
CustomerDemographic <- read_excel("C:/Users/Amar/Documents/GitHub/KPMG_Virtual_Internship/data/KPMG_VI_New_raw_data_update_final.xlsx", 
                                  sheet = "CustomerDemographic", skip = 1)

CustomerAddress <- read_excel("C:/Users/Amar/Documents/GitHub/KPMG_Virtual_Internship/data/KPMG_VI_New_raw_data_update_final.xlsx", 
                              sheet = "CustomerAddress", skip = 1)

#2. Data Quality Dimensions AS-IS
#Data quality meets six dimensions: accuracy, completeness, consistency, timeliness, relevancy, validity, and uniqueness.

#2.1 Accuracy
#The term "accuracy" refers to the degree to which information accurately reflects an event or object described.

#2.1.1 Transactions

summary(Transactions)

#Everything seems to be alright with the type of data of the variables, minimum values, maximum values, and format. Therefore, we assume the accuracy of the datasets given is high.

#2.1.2 Customer List

summary(CustomerList)

#We see problems with the purchases on bike related purchases, DOB, postcode, and property valuation for having a wrong format. Additionally there is a lack of customer ID, which makes validation of information more difficult between datasets.

#2.1.3 Customer Demographics

summary(CustomerDemographic)

#We see problems with, DOB for having a wrong format. However, the most concerning variable is default, since it does not make sense, it is just a group of random characters.

#2.1.4 Customer Address

summary(CustomerAddress)

#Everything seems to be alright with the type of data of the variables, minimum values, maximum values, and format. Therefore, we assume the accuracy of the datasets given is high.

#2.2 Completeness
#Data is considered "complete" when it fulfills expectations of comprehensiveness. We analyze the missing values in each of the datasets.

#2.2.1 Transactions

install.packages("dlookr")
library(dlookr)
library(dplyr)

diagnose(Transactions) %>%
  select(-unique_count, -unique_rate)%>%
  filter(missing_count != 0)%>%
  arrange(desc(missing_percent))%>% 
  knitr::kable(align = 'c', format = "markdown")

#The 7 variables displayed above show missing values; however, these values are not significative since the maximum percentage of missing values reach 1.8% at most.

#2.2.2 Custumer List
diagnose(CustomerList) %>%
  select(-unique_count, -unique_rate)%>%
  filter(missing_count != 0)%>%
  arrange(desc(missing_percent))%>% 
  knitr::kable(align = 'c', format = "markdown")

#The 3 variables displayed above show missing values, the job_title variable shows a significant missing percentage with 10.6%; however, this variable might not be critical to the analysis. On the other hand, the other two variables show signifficant completiness since the maximum percentage of missing values reach 2.9% at most.

#2.2.3 Custumer Demographics
diagnose(CustomerDemographic) %>%
  select(-unique_count, -unique_rate)%>%
  filter(missing_count != 0)%>%
  arrange(desc(missing_percent))%>% 
  knitr::kable(align = 'c', format = "markdown")

#The 5 variables displayed above show missing values, the job_title variable shows a significant missing percentage with 12.65%; however, this variable might not be critical to the analysis. On the other hand, the other two variables show signifficant completiness since the maximum percentage of missing values reach 6% at most.

#2.2.4 Custumer Address
diagnose(CustomerAddress) %>%
  select(-unique_count, -unique_rate)%>%
  filter(missing_count != 0)%>%
  arrange(desc(missing_percent))%>% 
  knitr::kable(align = 'c', format = "markdown")

#This means that there are not missing values, therefore the whole dataset is complete.

#2.3 Consistency
#Consistency refers to having the same data across different datasets.

#First, we identify the name of the variables that are repeated accross datasets.

colnames(Transactions)

colnames(CustomerList)

colnames(CustomerDemographic)

colnames(CustomerAddress)

#At a first glance it is possible to determine the shared variables between datasets. In particular, Customer List, seems to include most of the variables in Customer Demographics and Customer Address.

#2.3.1 Customer List vs Customer Demographic

install.packages("compareDF")

library(compareDF)

compare_df(CustomerList, CustomerDemographic)

#There are 11 variables that are shared. However, since there is not a user ID, the validation of the consistency will be done through the last name of the user.

summary(comparedf(CustomerList, CustomerDemographic, by="last_name",
                  control=comparedf.control(tol.vars = "case")))$diffs.byvar.table %>%
  knitr::kable(align = 'c', format = "markdown")

#The table above shows the number of data that do not match between datasets.

summary(comparedf(CustomerList, CustomerDemographic, 
                  by="last_name",
                  tol.char = "case" #ignores case in character vectors
))$comparison.summary.table %>%
  knitr::kable(align = 'c', format = "markdown")

#Additionally, there are 3803 observations that are registered in the demographics, but they are not part of the Customer List

#2.4 Timeliness
#There is no time stamp that shows how the data is managed and the availability of it. Therefore a timeliness parameter cannot be analyzed.

#2.5 Relevancy
#Some of the variables are not that relevant to some of the datasets. For example the variable default has no relevance in the CustomerDemographic dataset. In the same way, the duplication of data in the CustomerList makes the demographics included there not relevant. Additionally, a CustomerID is missing from this dataset.

#2.6 Validity
#As seen in the accuracy section some variables do not follow an expected format that they are supposed to, therefore the data is not validated and standardized for these variables.

#2.7 Uniqueness
#The following sections show the uniqueness value of each variable per dataset.

#2.2.1 Transactions
diagnose(Transactions) %>%
  select(-missing_count,-missing_percent)%>%
  arrange(desc(unique_count))

#2.2.2 Customer List
diagnose(CustomerList) %>%
  select(-missing_count,-missing_percent)%>%
  arrange(desc(unique_count))

#2.2.3 Customer Demographic
diagnose(CustomerDemographic) %>%
  select(-missing_count,-missing_percent)%>%
  arrange(desc(unique_count))

#2.2.4 Customer List
diagnose(CustomerAddress) %>%
  select(-missing_count,-missing_percent)%>%
  arrange(desc(unique_count))

#3. Overall Levels
#After analyzing each of the different categories we can determine the level of the data dimension into three cathegories. Red, will mean that there is a lot to improve, and it is a priority to solve before moving into the next stage. Yellow, meaning that it is fairly good and minor changes should be made; and green, meaning that no issues could be found and its ready for the next stage.

#Accuracy: Yellow

#Completeness: Yellow

#Consistency: Red

#Timeliness: Red

#Relevancy: Yellow

#Validity: Yellow

#Uniqueness: Green

#4. Mitigation measures
#Accuracy: As of right now the accuracy of the data is relatively high. Naturally some cleanliness in the data and standardization will increase this accuracy.

#Completeness: Most of the data is complete, the factor of missing values is usually non significants. Some of the variables that are not complete, are duplicated with other dataset, so after validating the entrees, the completeness level will increase even more. Additionally, it is necessary to the source of the missing data, if they ar missing at random, they can be ignored for future analysis. If not, they can be predicted with the mean or meadia of the categories missing.

#Consistency: This is the main problem of the data. A lot of data is not consistent across datasets. It is recommended to add a Customer ID for this matter in the customer list. There is a significant number of entries that differ across datasets. Mayor cleaning must be performed.

#Timeliness: Adding a time stamp would create the timeliness parameter that will allow the company to know when the data is available.

#Relevancy: Besides the variable default in the CustomerDemographics dataset, all the variables seem to be relevant.

#Validity: Some minor adjustments must be done. Specially on the variables regarding dates. A standardized format must be implemented for future datasets.

#Uniqueness: The data seems to have unique entries, however, a better control can be added once all the datasets are dependent on the CustomerID


