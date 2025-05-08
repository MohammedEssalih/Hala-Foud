# Install the necessary packages if you don't have them
install.packages("readxl")
install.packages("writexl")
library(readxl)
library(writexl)
Data_2_$`Would you be willing to buy Halal food?\r\n`

Data=Data_2_
Data$`Would you be willing to buy Halal food?\r\n`

head(Data)

# Clean and encode the data
# 1. Remove timestamp
Data$`Informazioni cronologiche` <- NULL
unique(Data$`How old are you?`)
# 2. Encode age
Data$Age <- dplyr::recode(Data$`How old are you?`,
                          "Under 25 years old" = 1,
                          "Between 25 and 50" = 2,
                          "Over 50 years old" = 3)

unique(Data$`Gender:`)
# 3. Encode gender
Data$Gender <- dplyr::recode(Data$`Gender:`,
                             "Male" = 0,
                             "Female" = 1)

# 4. Encode fast food frequency
unique(Data$`How often do you eat fast food?`)
Data$FastFoodFrequency <- dplyr::recode(Data$`How often do you eat fast food?`,
                                        
                                        "Rarely" = 0,
                                        "More than once a month" = 1,
                                        "Never" = 2,
                                        "More than once a week" = 3)



# 5. Encode willingness to buy Halal
unique(Data$`How much are you willing to pay for Halal fast food menu (fried chicken/burger + french fries + drink) considering that a normal menu is €8?`                                   
)

Data$WillingToBuyHalal <- dplyr::recode(Data$`Would you be willing to buy Halal food?\r\n`,
                                        "Yes" = 1,
                                        "No" = 0,
                                        "Maybe" = 1)

# 6. Encode last time ate Halal food
unique(Data$`When was the last time you ate Halal food?`)
Data$LastHalalConsumption <- dplyr::recode(Data$`When was the last time you ate Halal food?`,
                                           "This week" = 1,
                                           "Within the last month"=2,
                                           "More than one month ago" = 3,
                                           "Never" = 4)

# 7. Encode why consume Halal
unique(Data$`Why would you consume Halal products?`)
Data$WhyConsumeHalal <- dplyr::recode(Data$`Why would you consume Halal products?`,
                                      "Out of curiosity"  =1,                                    
                                      "Because of religious reasons"  =2,                        
                                      "For satisfaction"  =3,                                    
                                      "Because I consider them healthier"  =4,                   
                                      "For religious reasons and I consider them healthierder"  =5,
                                      "Cause I got no other options"            =6,              
                                      "Nn mangio halal food" =7)


# 8. Willingness to pay
unique(Data$`How much are you willing to pay for Halal fast food menu (fried chicken/burger + french fries + drink) considering that a normal menu is €8?`)
Data$WillingnessToPay <- dplyr::recode(Data$`How much are you willing to pay for Halal fast food menu (fried chicken/burger + french fries + drink) considering that a normal menu is €8?`,
                                       "Less than €8"=1,
                                       "8" =2,
                                       "9"=3,
                                       "More than €9" =4)




# 9. Menu choice
unique(Data$`If you had these menu options, which one would you choose?`)
Data$MenuChoice <- dplyr::recode(Data$`If you had these menu options, which one would you choose?`,
                                 "Standard menu (non-halal) with premium ingredients (selected sauces and spices) – €10.00"=1,
                                 "Halal menu with standard ingredients – €9"=2,                                               
                                 "Halal menu with premium ingredients (selected sauces and spices) – €11"=3,
                                 "Standard menu (non-halal) – €8.00"=4)
# 10. Remove original text columns
df_final <- Data %>% dplyr::select(
  `How important is it to you that fast food restaurants offer Halal products?`,
  Age, Gender, FastFoodFrequency, WillingToBuyHalal,
  LastHalalConsumption, WhyConsumeHalal, WillingnessToPay, MenuChoice
)
library(tidyverse)
# 11. Save the cleaned and encoded data
write_xlsx(df_final, "Cleaned_encoded_data.xlsx")
# Logistic regression model
model <- glm(WillingToBuyHalal ~ Age + Gender + FastFoodFrequency, 
             data = df_final, 
             family = binomial)

