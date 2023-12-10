rm(list = ls())  
setwd("/Users/tierney/Desktop/Rcodes")
library(foreign)
library(survey)
library(tidyverse)
library(ipumsr)
library(dplyr)
library(haven)
library(labelled)
library(ggplot2)
library(magrittr)
library(knitr)
library(stargazer)
library(gtsummary)
library(broom)
library(gt)


# DDI file, dataframes: Senegal 2017 DHS 
#Household Survey: "data"
ddi <- read_ipums_ddi("idhs_00010.xml")
data_file_path <- "idhs_00010.dat"
data <- read_ipums_micro(ddi, data_file = data_file_path)

#Women's Survey "datawomen"
ddi_women <- read_ipums_ddi("idhs_00008.xml")
datawomen <- read_ipums_micro(ddi_women, data_file = "idhs_00008.dat")

#Survey Weights: Household Survey Data and Women's survey data
design_data <- svydesign(
  ids = ~HHID,  
  weights = ~HHWEIGHT,
  data = data
)

design_datawomen <- svydesign(
  ids = ~HHID,
  weights = ~PERWEIGHT,
  data = datawomen
)

#Merged Data and Survey Weight Merged Data (USING MY HHWEIGHT)
mergeddata <- merge(data, datawomen)

#Changed urban to (0,1 values instead of 1, 2)- CHECKED FOR MISSING OR NA VALUES AND NONE PRESENT
selected_vars <- mergeddata %>%
  select(
    HHID,HHAGE,GEO_SN2017,HHRESIDENT,FPEVUSECAL,DELDOC_01,DELNURM_01, DELNONE_01, HHWEIGHT, HHBIRTHCERT, WEALTHQRURBHH, URBANHH, HHAGE, INSHHCOVER, ELECTRC, HHPHONE, INTERNET, HHPHONEHH, MOBPHONE, PC
  ) %>%
  mutate(
    URBAN = ifelse(URBANHH == 1, 1, 0)
  ) %>%
  select(-URBANHH)


# Restricted Values by excluding Data not in Universe and made HHBIRTHCERT a binary variable, additionally limited population to under the age of five 
selected_vars <- selected_vars %>%filter(!(HHBIRTHCERT %in% c(99, 98, 97)))

selected_vars <- selected_vars %>%
  filter(!(HHRESIDENT %in% c(2, 8)))

selected_vars <- selected_vars %>%
  filter(!(HHPHONEHH %in% c(6, 8)))

selected_vars <- selected_vars %>%
  filter(!(INSHHCOVER %in% c(7, 8,9)))

selected_vars <- selected_vars %>%
  filter(!(ELECTRC %in% c(6, 8)))

selected_vars <- selected_vars %>%
  filter(!(INTERNET %in% c(6, 8,9)))

selected_vars <- selected_vars %>%
  filter(!(HHBIRTHCERT %in% c(8)))

selected_vars <- selected_vars %>%
  filter(!(DELDOC_01 %in% c(8,9)))

selected_vars <- selected_vars %>%
  filter(!(DELNURM_01 %in% c(8,9)))

selected_vars <- selected_vars %>%
  filter(!(DELNONE_01 %in% c(8,9)))

selected_vars <- selected_vars %>%
  filter(!(FPEVUSECAL %in% c(98,99)))



# Created Dummy Variables for the four regions (WEST, CENTER, NORTHEAST, SOUTH) of populaition from DHS
selected_vars$GEO_SN2017 <- as.factor(selected_vars$GEO_SN2017)

# Converted GEO_SN2017 into dummy variables
dummy_geo <- model.matrix(~ GEO_SN2017, data = selected_vars)
selected_vars <- cbind(selected_vars, dummy_geo[, 2:5])
colnames(selected_vars)[(ncol(selected_vars) - 3):ncol(selected_vars)] <- c("WEST", "CENTER", "NORTHEAST"
                                                                            , "SOUTH")

#HHBIRTHCERT_BINARY, FPEVUSECAL_BINARY Variables
selected_vars$HHBIRTHCERT_BINARY <- ifelse(selected_vars$HHBIRTHCERT %in% c(10, 11, 12, 13), 1, 0)
selected_vars$FPEVUSECAL_BINARY <- ifelse(selected_vars$FPEVUSECAL %in% c(10,11, 12), 1, 0)
summary(selected_vars)
design_merged <- svydesign(ids = ~HHID, weights = ~HHWEIGHT,  data = selected_vars) 


# Logistic regression formula
formula_binary <- as.formula(
  "HHBIRTHCERT_BINARY~ WEALTHQRURBHH + URBAN + INSHHCOVER + ELECTRC + 
  HHPHONE + INTERNET + MOBPHONE + PC + DELDOC_01 + DELNURM_01 + DELNONE_01 + WEST + CENTER + NORTHEAST + SOUTH +
  FPEVUSECAL_BINARY")

# Fitted logistic regression model
logistic_model <- svyglm(formula = formula_binary, design = design_merged)
summary(logistic_model)

#ODDS RATIOS of the logistic model Displayed
tbl_regression(logistic_model, exp = TRUE)


# Tested for the Odds Ratios with a 95% Confidence Interval
odds_ratios <- data.frame(exp(cbind(Odds_Ratio = coef(logistic_model), confint(logistic_model))))
odds_ratios %<>% mutate(variable = rownames(odds_ratios))

# Filtered for the variables of interest: "WEST", "CENTER", "NORTHEAST", "SOUTH", "WEALTHQRURBHH", "URBAN", "INSHHCOVER", "ELECTRC", "HHPHONE", "INTERNET", "MOBPHONE", "PC", "DELDOC_01", "DELNURM_01", "DELNONE_01", "FPEVUSECAL_BINARY"
selected_variables <- c("WEST", "CENTER", "NORTHEAST" , "SOUTH", "WEALTHQRURBHH", "URBAN", "Dakar, Thiès (West) +Diourbel", "Diourbel, Fatick, Kaolack, Louga, Kaffrine (Center)", "Saint-Louis, Tambacounda, Matam, Kedougou (North East)","Kolda, Ziguinchor, Sedhiou (South)", "INSHHCOVER", "ELECTRC", "HHPHONE", "INTERNET", "MOBPHONE", "PC", "DELDOC_01", "DELNURM_01", "DELNONE_01", "FPEVUSECAL_BINARY")
odds_ratios <- odds_ratios %>% filter(variable %in% selected_variables)

# Plotted the Odds Ratios with annotations
odds_ratios$color <- ifelse(odds_ratios$Odds_Ratio > 1, "tomato4", "aquamarine4")
plot <- ggplot(odds_ratios, aes(y = variable, x = Odds_Ratio, xmin = odds_ratios$X2.5.., xmax = odds_ratios$X97.5.., color = color, fill = color)) +
  geom_pointrange(size = 0.9) +
  geom_vline(xintercept = 1, linetype = "dashed", size = 1.0) +
  geom_errorbarh(aes(xmin = odds_ratios$X2.5.., xmax = odds_ratios$X97.5..), height = 0.2, size = 1.0) +
  geom_text(aes(label = sprintf("%.2f", Odds_Ratio)), vjust = -1.0, color = "black") +
  scale_color_manual(values = c("tomato4", "aquamarine4"), guide = "none") +
  scale_fill_manual(values = c("tomato4", "aquamarine4"), guide = "none") +
  labs(title = "Odds Ratios with 95% Confidence Interval") +
  theme(panel.background = element_rect(fill = "aliceblue"))
ggsave("odds_ratios_plot.png", plot, width = 8, height = 10, units = "in", dpi = 300)

# Data frame for variables table
table_data <- data.frame(
  Variable = c("HHBIRTHCERT_BINARY", "WEST", "CENTER", "NORTHEAST", "SOUTH", "WEALTHQRURBHH", "URBAN", "INSHHCOVER", "ELECTRC", "HHPHONE", "INTERNET", "MOBPHONE", "PC", "DELDOC_01", "DELNURM_01", "DELNONE_01", "FPEVUSECAL_BINARY"),
  Description = c("Binary indicator for birth certificate","Dakar, Thiès (West)", "Diourbel, Fatick, Kaolack, Louga, Kaffrine (Center)", "Saint-Louis, Tambacounda, Matam, Kedougou (North East)", "Kolda, Ziguinchor, Sedhiou (South)", "Wealth quartile of urban household", "Recoded urban indicator (0 or 1)", "Health insurance coverage", "Electricity in the household", "Household has a phone", "Internet in the household", "Mobile phone in the household", "Personal computer in the household", "Delivery assisted by doctor (last birth)", "Delivery assisted by nurse/midwife (last birth)", "Delivery not assisted (last birth)", "Binary indicator for contraceptive use")
)
table_plot <- gt(table_data) %>%
  tab_header(
    title = "Summary of Variables"
  ) %>%
  gtsave(filename = "variable_summary_table.png", path = "/Users/tierney/Desktop/Rcodes")




