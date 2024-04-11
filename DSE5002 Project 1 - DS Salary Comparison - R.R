
#Import data 
salaries <- read.csv(file.choose())
head(salaries)

#libraries 
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)

# 1. Comparison of different Data Role salaries 
# US- Companies 
salaries %>%
  filter(company_location == "US" &
           employment_type == "FT" &
           str_detect(job_title, "Data")) %>%
  group_by(job_title, work_year) %>%
  summarise(us_mean_salary = mean(salary_in_usd)) %>%
  ggplot(aes(x = job_title, y = us_mean_salary, fill = job_title)) +
  geom_bar(stat = "identity", color = "black") +  
  facet_grid(work_year ~ .) +
  labs(x = "Job Title", 
       y = "Average Salary in USD", 
       title = "Data Role Average Salaries (2020-2022) - US Companies",
       fill = "Job Title") +
  scale_y_continuous(labels=scales::dollar_format()) +
  theme_minimal() +
  theme(axis.text.x = element_blank()) +
  guides(fill = guide_legend(ncol = 1))

# Non-US Companies 
salaries %>%
  filter(company_location != "US" &
           employment_type == "FT" &
           str_detect(job_title, "Data")) %>%
  group_by(job_title, work_year) %>%
  summarise(us_mean_salary = mean(salary_in_usd))  %>%
  ggplot(aes(x = job_title, y = us_mean_salary, fill = job_title)) +
  geom_bar(stat = "identity", color = "black") +  
  facet_grid(work_year ~ .) +
  labs(x = "Job Title", 
       y = "Average Salary in USD", 
       title = "Data Role Average Salaries (2020-2022) - Non-US Companies",
       fill = "Job Title") +
  scale_y_continuous(labels=scales::dollar_format()) +
  theme_minimal() +
  theme(axis.text.x = element_blank()) +
  guides(fill = guide_legend(ncol = 1))




#-------------------------------------------------------------------------------------------------------------------

# 2. Data Scientist Salaries by Experience Level - 2020-2022
# US Companies 
salaries %>%
  filter(company_location == "US" & 
           job_title == "Data Scientist" &
           employment_type == "FT") %>%
  ggplot(aes(x = work_year, y = salary_in_usd, group=work_year)) +
  geom_jitter(width = 0.2) +  
  geom_boxplot(width = 0.5, alpha=0.5) +  
  facet_grid(experience_level~.) +
  labs(x = "Work Year", y = "Salary in USD", title = "Data Scientist Salaries by Work Year & Experience Level (US Companies)") +
  theme_minimal() +
  scale_y_continuous(labels=scales::dollar_format()) +
  scale_x_continuous(breaks=c(2020, 2021, 2022)) 

# Non-US Companies 
salaries %>%
  filter(company_location != "US" & 
           job_title == "Data Scientist" &
           employment_type == "FT") %>%
  ggplot(aes(x = work_year, y = salary_in_usd, group=work_year)) +
  geom_jitter(width = 0.2) +  
  geom_boxplot(width = 0.5, alpha=0.5) + 
  facet_grid(experience_level~. ,
             scales= "free_y") +
  labs(x = "Work Year", y = "Salary in USD", title = "Data Scientist Salaries by Work Year & Experience Level (Non-US Companies)") +
  theme_minimal() +
  scale_y_continuous(labels=scales::dollar_format()) +
  scale_x_continuous(breaks=c(2020, 2021, 2022))

#-------------------------------------------------------------------------------------------------------------------
# 3. Data Scientist Salaries - US vs non-US - by Company Size 
#Entry Level - Full Time 
us_en_ds_salaries <- salaries %>%
  filter(experience_level == "EN" & 
           employment_type == "FT" & 
           company_location == "US" & 
           job_title == "Data Scientist") %>%
  group_by(job_title, experience_level, employment_type, company_location, company_size) %>%
  summarise(us_mean_salary = mean(salary_in_usd))

non_us_en_ds_salaries <- salaries %>%
  filter(experience_level == "EN" & 
           employment_type == "FT" & 
           company_location != "US" & 
           job_title == "Data Scientist") %>%
  group_by(job_title, experience_level, employment_type, company_location, company_size) %>%
  summarise(us_mean_salary = mean(salary_in_usd))

# Combine Entry-Level US and non-US data based on Compnay Size 
entry_level_combined_data <- rbind(us_en_ds_salaries, non_us_en_ds_salaries)

# Convert List to Data Frame - Entry Level DS Positions
ds_salaries_entry_level <- data.frame(entry_level_combined_data)
ds_salaries_entry_level 

# Creating Visualizations - Entry Level DS Positions
ds_salaries_en_plot <- ggplot(data = ds_salaries_entry_level, aes(x = company_location, y = us_mean_salary, 
                                                            fill= company_location)) + 
  geom_col(color= "Black") + 
  facet_grid(company_size~.) +
  labs(x = "Country Code",
       y = "Salary in USD",
       title = "Entry Level DS Salaries by Country & Company Size",
       fill = "Country Code") +
  theme(axis.title.x = element_text(size=10),
        axis.title.y = element_text(size=10),
        
        legend.title = element_text(size=10),
        legend.text = element_text(size=10),
        legend.position = "right",
        legend.justification = "center",
        
        plot.title = element_text(size=12)) +
  scale_y_continuous(labels=scales::dollar_format()) +
  theme_minimal()

ds_salaries_en_plot


#--------------------------------------------------------------------------------------------------------

#Mid-Level - Full Time 
us_mi_ds_salaries <- salaries %>%
  filter(experience_level == "MI" & 
           employment_type == "FT" & 
           company_location == "US" & 
           job_title == "Data Scientist") %>%
  group_by(job_title, experience_level, employment_type, company_location, company_size) %>%
  summarise(us_mean_salary = mean(salary_in_usd))

non_us_mi_ds_salaries <-  salaries %>%
  filter(experience_level == "MI" & 
           employment_type == "FT" & 
           company_location != "US" & 
           job_title == "Data Scientist") %>%
  group_by(job_title, experience_level, employment_type, company_location, company_size) %>%
  summarise(us_mean_salary = mean(salary_in_usd))

# Combine Mid-Level US and non-US data based on Compnay Size 
mid_level_combined_data <- rbind(us_mi_ds_salaries, non_us_mi_ds_salaries)

# Convert List to Data Frame - Mid Level DS Positions
ds_salaries_mid_level <- data.frame(mid_level_combined_data)
ds_salaries_mid_level 

# Creating Visualizations - Mid Level DS Positions
ds_salaries_mid_plot <- ggplot(data = ds_salaries_mid_level, aes(x = company_location, y = us_mean_salary, 
                                           fill= company_location)) + 
  geom_col(color= "Black") + 
  facet_grid(company_size~.) +
  labs(x = "Country Code",
       y = "Salary in USD",
       title = "Mid Level DS Salaries by Country & Company Size",
       fill = "Country Code") +
  theme(axis.title.x = element_text(size=10),
        axis.title.y = element_text(size=10),
        
        legend.title = element_text(size=10),
        legend.text = element_text(size=10),
        legend.position = "right",
        legend.justification = "center",
        
        plot.title = element_text(size=12)) +
  guides(fill = guide_legend(ncol = 2)) +
  scale_y_continuous(labels=scales::dollar_format(),
                     breaks=c(25000,50000,75000,100000,125000,150000)) +
  theme_minimal()

ds_salaries_mid_plot

#--------------------------------------------------------------------------------------------------------------------

#Senior Level - Full Time 
us_se_ds_salaries <- salaries %>%
  filter(experience_level == "SE" & 
           employment_type == "FT" & 
           company_location == "US" & 
           job_title == "Data Scientist") %>%
  group_by(job_title, experience_level, employment_type, company_location, company_size) %>%
  summarise(us_mean_salary = mean(salary_in_usd))

non_us_se_ds_salaries <- salaries %>%
  filter(experience_level == "SE" & 
           employment_type == "FT" & 
           company_location != "US" & 
           job_title == "Data Scientist") %>%
  group_by(job_title, experience_level, employment_type, company_location, company_size) %>%
  summarise(us_mean_salary = mean(salary_in_usd))

# Combine Senior-Level US and non-US data based on Compnay Size 
se_level_combined_data  <- rbind(us_se_ds_salaries,non_us_se_ds_salaries)

# Convert List to Data Frame - Mid Level DS Positions
ds_salaries_se_level <- data.frame(se_level_combined_data)
ds_salaries_se_level 

# Creating Visualizations - Mid Level DS Positions
ds_salaries_se_plot <- ggplot(data = ds_salaries_se_level, aes(x = company_location, y = us_mean_salary, 
                                         fill= company_location)) + 
  geom_col(color= "Black") + 
  facet_grid(company_size~.) +
  labs(x = "Country Code",
       y = "Salary in USD",
       title = "Senior Level DS Salaries by Country & Company Size",
       fill = "Country Code") +
  theme(axis.title.x = element_text(size=10),
        axis.title.y = element_text(size=10),
        
        legend.title = element_text(size=10),
        legend.text = element_text(size=10),
        legend.position = "right",
        legend.justification = "center",
        
        plot.title = element_text(size=12)) +
  scale_y_continuous(labels=scales::dollar_format(),
                     breaks=c(50000,100000,150000,200000)) +
  theme_minimal()

ds_salaries_se_plot

#Expert Level - no data for EX data scientist in US or non-US







