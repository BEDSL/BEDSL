library(fastDummies)
library(readxl)
library(dplyr)
library(haven)
library(survey)
library(svrep)
library(sjlabelled)


# Importare
postcodified <- read_dta("ZA7579_v1-0-0 - postcodified.dta")


# Eurobarometru-----------------------------------------------------------------

# D11 Age
summary(postcodified$d11)
mydata <- data.frame(Age = postcodified$d11)

#Q1 Nationality
#q1_1 - q1_28 -> 28 countries
#q1_29        -> other countries: 93
#q1_30 DK     -> don't know: 0
table(postcodified$q1_30)
table(postcodified$q1_29)
mydata <- mydata %>% bind_cols(postcodified[, 23:51])


# D70 Life satisfaction
mydata <- mydata %>%
          mutate(Life_satisfaction = case_when(
                postcodified$d70 == 1 | postcodified$d70 == 2 ~ 1,  
                TRUE ~ 0 ))                 
table(mydata$Life_satisfaction)


# D71 Political Interest
mydata <- mydata %>% bind_cols(postcodified[, 54:56] %>%
                                 mutate(across(everything(), ~ if_else(. %in% c(1, 2), 1, 0))))
table(mydata$d71_3)



# D15a Occupation
sum(table(postcodified$d15a))
table(postcodified$d15a)
dummy_vars <- dummy_cols(postcodified['d15a'], select_columns = 'd15a', remove_first_dummy = FALSE, remove_selected_columns = TRUE)
mydata<- cbind(mydata, dummy_vars)


# DX1 Migrant
table(postcodified$dx1_6)
table(postcodified$dx1_7)
mydata$dx1_country <- postcodified$dx1_1
mydata$dx1_eu <- ifelse(postcodified$dx1_2 == 1 | postcodified$dx1_3 == 1, 1, 0)
mydata$dx1_outside <- ifelse(postcodified$dx1_4 == 1 | postcodified$dx1_5 == 1, 1, 0)

# QD1 Undeclared workers in social circle
table(postcodified$qd1)
mydata <- mydata %>%
  mutate(qd1 = case_when(
        postcodified$qd1 == 1 | postcodified$qd1 == 2 | postcodified$qd1 == 4 ~ 1,  
        TRUE ~ 0 ))                 
table(mydata$qd1)


# QD2 Sanction
table(postcodified$qd2)
mydata$qd2_payback <- ifelse(postcodified$qd2 == 1, 1, 0)
mydata$qd2_fine <- ifelse(postcodified$qd2 == 2, 1, 0)
mydata$qd2_prison <- ifelse(postcodified$qd2 == 3, 1, 0)


# QD3 Risk perception
table(postcodified$qd3)
mydata$qd3 <- ifelse(postcodified$qd3 == 1 | postcodified$qd3 == 2, 1, 0)
table(mydata$qd3)

# QD4 Trust in authorities/inspectorate
table(postcodified$qd4_1)
table(postcodified$qd4_2)
mydata$qd4_1 <- ifelse(postcodified$qd4_1 == 1, 1, 0)
mydata$qd4_2 <- ifelse(postcodified$qd4_2 == 1, 1, 0)

# QD5  Tax morality computation
postcodified$qd5_4t             # labels 1 2 3
sum(table(postcodified$qd5_4t)) # exista date lipsa
mydata <- mydata %>%
  mutate(tax_morality = case_when(
    (postcodified$qd5_1t == 3 | postcodified$qd5_2t == 3 | postcodified$qd5_3t == 3 | postcodified$qd5_4t == 3 | postcodified$qd5_5t == 3) |
      (postcodified$qd13 == 1 | postcodified$qd13 == 2)  ~ 1,
    TRUE ~ 0
  ))
table(mydata$tax_morality)

#QD6 Bought undeclared products
table(postcodified$qd6)
mydata$qd6 <- ifelse(postcodified$qd6 == 1 | postcodified$qd6 == 3, 1, 0)

#QD8 QD20 Paid/Has been paid illegally
# Undeclared work players 
mydata <- mydata %>%
  mutate(undeclared_work_players = case_when(
    (postcodified$qd8_1 == 1 | postcodified$qd8_2 == 1 | postcodified$qd8_3 == 1 | postcodified$qd8_4 == 1 | postcodified$qd8_5 == 1 | 
       postcodified$qd8_6 == 1 | postcodified$qd8_7 == 1 | postcodified$qd8_8 == 1 | postcodified$qd8_9 == 1 | 
       postcodified$qd20_1 == 1 | postcodified$qd20_2 == 1 | postcodified$qd20_3 == 1 | postcodified$qd20_4 == 1 | 
       postcodified$qd20_5 == 1 | postcodified$qd20_6 == 1 | postcodified$qd20_7 == 1 | postcodified$qd20_8 == 1) ~ 1,
    TRUE ~ 0
  ))

#QD9 motivations
# Low price motivation
table(postcodified$qd9_1)
mydata <- mydata %>%
  mutate(low_price_motivation = case_when(
    (postcodified$qd9_1 == 1  ) ~ 1,
    TRUE ~ 0
  ))
table(mydata$low_price_motivation)
                                       

# Faster service!
table(postcodified$qd9_2)
mydata <- mydata %>%
  mutate(faster_service_motivation = case_when(
    (postcodified$qd9_2 == 1  ) ~ 1,
    TRUE ~ 0
  ))
table(mydata$faster_service_motivation)
  

# Better quality
table(postcodified$qd9_3)
mydata <- mydata %>%
  mutate(better_quality_motivation = case_when(
    (postcodified$qd9_3 == 1) ~ 1,
    TRUE ~ 0
  ))
table(mydata$better_quality_motivation)

# Help someone in need for money
table(postcodified$qd9_4)
mydata <- mydata %>%
  mutate(help_someone_in_need_for_money = case_when(
    (postcodified$qd9_4 == 1) ~ 1,
    TRUE ~ 0
  ))
table(mydata$help_someone_in_need_for_money)


# Favour
table(postcodified$qd9_5)
mydata <- mydata %>%
  mutate(favour_motivation = case_when(
    (postcodified$qd9_5 == 1) ~ 1,
    TRUE ~ 0
  ))
table(mydata$favour_motivation)


# Hard to find on regular market
table(postcodified$qd9_6)
mydata <- mydata %>%
  mutate(hard_to_find_regular_market = case_when(
    (postcodified$qd9_6 == 1) ~ 1,
    TRUE ~ 0
  ))
table(mydata$hard_to_find_regular_market)


# Only realised afterwards that it was undeclared
table(postcodified$qd9_7)
mydata <- mydata %>%
  mutate(by_mistake = case_when(
    (postcodified$qd9_7 == 1) ~ 1,
    TRUE ~ 0
  ))
table(mydata$by_mistake)



# QD10 Undeclared Salary
#table(postcodified$qd10)
#mydata <- mydata %>%
#  mutate(qd10 = case_when(
#    (postcodified$qd10 == 1 | postcodified$qd10 == 3 | postcodified$qd10 == 4 ) ~ 1,
#    TRUE ~ 0
#  ))
#table(mydata$qd10)


#QD10 QD11 QD12 Envelope wage
mydata <- mydata %>%
  mutate(envelope_wage = case_when(
    (postcodified$qd10 == 1 | postcodified$qd10 == 3 | postcodified$qd10 == 4| 
       postcodified$qd11 == 1 | postcodified$qd11==2 | postcodified$qd11==3 | postcodified$qd11==4|
       postcodified$qd12r == 1 |postcodified$qd12r == 2 | postcodified$qd12r == 3 | postcodified$qd12r == 4 | postcodified$qd12r == 5) ~ 1,
    TRUE ~ 0
  ))
table(mydata$envelope_wage)
postcodified$qd12


#QD10 QD13 Has/refuses/DK OR would like to have undeclared salary
mydata <- mydata %>%
  mutate(has_OR_would_have_undeclared_salary = case_when(
    (postcodified$qd10 == 1 | postcodified$qd10 == 3 | postcodified$qd10 == 4| 
     postcodified$qd13 == 1 | postcodified$qd13 == 2| postcodified$qd13 == 4| postcodified$qd13 == 5) ~ 1,
    TRUE ~ 0
  ))
table(mydata$has_OR_would_have_undeclared_salary)


#QD13 Disponibility towards illegal salary
#table(postcodified$qd13)
#mydata$qd13 <- ifelse(postcodified$qd13 == 1 | postcodified$qd13 == 2| postcodified$qd13 == 4| postcodified$qd13 == 5, 1, 0)
#table(mydata$qd13)

#QD14 Refused illegal salary
#table(postcodified$qd14)
#mydata$qd14 <- ifelse(postcodified$qd14 == 1, 1, 0)
#table(mydata$qd14)

#QD15 Number of employees
table(postcodified$qd15)
summary(postcodified$qd15) #NA = unemployed
mydata$qd15 <- ifelse(postcodified$qd15 %in% c(NA), 0, postcodified$qd15)
# Calculate the mode excluding 0, 8, and 9 and replace 8 and 9 with the mode
mode_value <- mydata %>% filter(!qd15 %in% c(0, 8, 9)) %>% pull(qd15) %>% { as.numeric(names(which.max(table(.)))) }
mydata <- mydata %>% mutate(qd15 = if_else(qd15 %in% c(8, 9), mode_value, qd15))
table(mydata$qd15)
summary(mydata$qd15)

#QD16 Undeclared paid activities
table(postcodified$qd16)
mydata$qd16 <- ifelse(postcodified$qd16 == 1 | postcodified$qd16 == 3| postcodified$qd16 == 4, 1, 0)
table(mydata$qd16)

#QD19 Digital Undeclared activity
table(postcodified$qd19)
mydata <- mydata %>%
  mutate(qd19 = case_when(postcodified$qd19 == 1 | postcodified$qd19 == 2| postcodified$qd19 == 4| postcodified$qd19 == 5 ~ 1,
                          TRUE ~ 0))
table(mydata$qd19)

#QD24 Without formal contract
table(postcodified$qd24a_3)
mydata <- mydata %>%
  mutate(without_formal_contract = case_when(postcodified$qd24a_1 == 1 | postcodified$qd24a_2 == 1 | postcodified$qd24a_3 == 1 | postcodified$qd24a_4 == 1 | postcodified$qd24a_5 == 1 ~ 1,
                                     TRUE ~ 0))
table(mydata$without_formal_contract)

#QD25 Perceived percentage of population that is working undeclared
table(postcodified$qd25)
mode_value <- postcodified %>% filter(!qd25 %in% c( 9, 10)) %>% pull(qd25) %>% { as.numeric(names(which.max(table(.)))) }
mydata$qd25 <- ifelse(postcodified$qd25 %in% c(9, 10), mode_value, postcodified$qd25)
table(mydata$qd25)

#D1 Left/right politics
table(postcodified$d1r1)
dummy_vars <- dummy_cols(postcodified['d1r1'], select_columns = 'd1r1', remove_first_dummy = FALSE, remove_selected_columns = TRUE)
mydata<- cbind(mydata, dummy_vars)

#D7 Civil state
table(postcodified$d7r2)
dummy_vars <- dummy_cols(postcodified['d7r2'], select_columns = 'd7r2', remove_first_dummy = FALSE, remove_selected_columns = TRUE)
mydata<- cbind(mydata, dummy_vars[,-c(5,6)])

#D10 Gender
table(postcodified$d10)
mydata$gender<- ifelse(postcodified$d10 == 1, 1, 0)
table(mydata$gender)

#D8 Age when stopped full-time education
table(postcodified$d8r2)
dummy_vars <- dummy_cols(postcodified['d8r2'], select_columns = 'd8r2', remove_first_dummy = FALSE, remove_selected_columns = TRUE)
mydata<- cbind(mydata, dummy_vars[,-c(6,7)])

#D25 Rural/Medium city/Big city
table(postcodified$d25)
mode_value <- names(sort(table(postcodified$d25), decreasing = TRUE)[1])
mydata$d25 <- ifelse(postcodified$d25 == 8, mode_value, postcodified$d25)
table(mydata$d25)

#D40a Adults in household
summary(table(postcodified$d40a))
mean_value <- mean(postcodified$d40a, na.rm = TRUE)
mydata$adults_in_household <- ifelse(is.na(postcodified$d40a), round(mean_value), postcodified$d40a)

#D40b/c Children in household
summary(table(postcodified$d40b))
mean_value <- mean(postcodified$d40b, na.rm = TRUE)
children_below_10 <- ifelse(is.na(postcodified$d40b), round(mean_value), postcodified$d40b)
summary(table(postcodified$d40c))
mean_value <- mean(postcodified$d40c, na.rm = TRUE)
children_10_14 <- ifelse(is.na(postcodified$d40c), round(mean_value), postcodified$d40c)
mydata$children_in_household <- children_below_10 + children_10_14
table(mydata$children_in_household)

#D60 Financial difficulties
table(postcodified$d60)
dummy_vars <- dummy_cols(postcodified['d60'], select_columns = 'd60', remove_first_dummy = FALSE, remove_selected_columns = TRUE)
mydata<- cbind(mydata, dummy_vars)

#D63 Social Class
table(postcodified$d63)
mode_value <- postcodified %>% filter(!d63 %in% c( 6, 7,8, 9)) %>% pull(d63) %>% { as.numeric(names(which.max(table(.)))) }
mydata$d63 <- ifelse(postcodified$d63 %in% c(6, 7,8, 9), mode_value, postcodified$d63)
table(mydata$d63)

#P5 Respondent cooperation
table(postcodified$p5)
mydata$p5 <- postcodified$p5

#dep_self_empl / dependent workers / bogus self employment
table(postcodified$dep_self_empl)
summary(postcodified$dep_self_empl)
mydata$dep_self_empl<- ifelse(postcodified$dep_self_empl %in% c(NA), 0, postcodified$dep_self_empl)


# Macroeconomice---------------------------------------------------------------

# The macroeconomic data of which country to assign to which individual?
first_1_index <- max.col(mydata[, 2:30] == 1, ties.method = "first")
first_1_column <- names(mydata)[first_1_index + 1]  # Adding 1 to adjust for columns 2 to 30
mydata$id <- first_1_column


# Print the result
sum(table(first_1_column))    #27565 individuals
length(table(first_1_column)) #28 labels

macro <- read.csv("Date Macroeconomice.csv")
merged_data <- left_join(mydata, macro %>% select(-Country, -Year), by = "id")
merged_data <- merged_data %>% select(-id)
write.csv(merged_data, "Merged_data.csv", row.names = FALSE)

# Check ups -------------------------------------------------------------------
# Get column names with NA values
names(which(colSums(is.na(mydata)) > 0))
names(which(colSums(is.na(merged_data)) > 0))

#tax morality
table(postcodified$qd5_1t) 
summary(postcodified$qd5_1t)
table(mydata$tax_morality)
summary(mydata$tax_morality)

#envelope_wage
table(postcodified$envelope_wage)
summary(postcodified$envelope_wage)
table(mydata$qd10)
summary(mydata$qd10)

#without_contract
table(postcodified$without_contract)
summary(postcodified$without_contract)
table(mydata$without_formal_contract)
summary(mydata$without_formal_contract)

#dep_self_empl / dependent workers / bogus self employment
table(postcodified$dep_self_empl)
summary(postcodified$dep_self_empl) 
table(mydata$dep_self_empl)
summary(mydata$dep_self_empl)


# Dataset pentru predictii-------------

# TO PREDICT: without_formal_contract, envelope_wage, dep_self_empl
# ELIMINATING: 
# undeclared_work_players (contine env wage)
# QD16 Undeclared paid activities
# QD19 Digital Undeclared activity

dataset_final <- merged_data %>% select(-c(undeclared_work_players, qd16,qd19))
write.csv(dataset_final, "Date_finale_pt_predictii.csv",  row.names = FALSE)

library(writexl)
write_xlsx(dataset_final,"Date_finale_pt_predictii.xlsx")

#dff <- as.data.frame(dataset_final)
#write.csv(dff, "Date_finale_pt_predictii.csv")







