# recreate a plot in R from Slide 11 using the chatbot

load("data/df.Rda")

library(tidyverse)
library(ggplot2)
library(survey)
library(survival)


# Define a custom color palette with NCHS colors
custom_palette <- function() {
  c("#008bb0", "#d06f1a", "#ffd200", "#006858",  "#695e4a", "#0033A1", "pink")
}
# note: pink is an add-on; not in official NCHS template

# Create a custom ggplot theme
custom_theme <- function() {
  theme_minimal(base_size = 15) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      axis.title.x = element_text(face = "bold"),
      axis.title.y = element_text(face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.text = element_text(face = "bold"),
      legend.position = "right",
      panel.grid.major = element_line(color = "grey80"),
      panel.grid.minor = element_blank()
    )
}

# slide 11: age group by education level (initial code output) ----

# Box plot for age_p
ggplot(data, aes(x = factor(XEDUC), y = AGE_P, fill = factor(STATUS))) +
  geom_boxplot() +
  facet_wrap(~ XAGEGROUP) +
  labs(title = "Baseline Mean Age by Education and Age Groups",
       x = "Education Level",
       y = "Age") +
  theme_minimal()

# Box plot for PERY
ggplot(data, aes(x = factor(XEDUC), y = PERY, fill = factor(MORTSTAT))) +
  geom_boxplot() +
  facet_wrap(~ XAGEGROUP) +
  labs(title = "Follow-Up Years by Education and Age Groups",
       x = "Education Level",
       y = "Follow-Up Years") +
  theme_minimal()

# Slide 11: plot age by education level ----

ggplot(data, aes(x = factor(XEDUC), y = AGE_P)) +
  geom_boxplot(fill = "coral") +
  facet_wrap(~ XAGEGROUP, labeller = as_labeller(c("1" = "25-39", "2" = "40-64", "3" = "65+"))) +  # Custom labels
  labs(title = "Boxplots of Age by Education Level and Age Group",
       x = "Education Level (XEDUC)",
       y = "Age") +
  theme_minimal() +
  stat_boxplot(geom = "errorbar", width = 0.25)


# Slide 11: plot follow-up yrs by education level ----

ggplot(data, aes(x = factor(XEDUC), y = PERY)) +
  geom_boxplot(fill = "cyan") +
  facet_wrap(~ XAGEGROUP, labeller = as_labeller(c("1" = "25-39", "2" = "40-64", "3" = "65+"))) +  # Custom labels
  labs(title = "Boxplots of Follow-up Years by Education Level and Age Group",
       x = "Education Level (XEDUC)",
       y = "Follow-up Years") +
  theme_minimal() +
  stat_boxplot(geom = "errorbar", width = 0.25)

## try with custom theme ----


ggplot(data, aes(x = factor(XEDUC), y = PERY)) +
  geom_boxplot(fill = "cyan") +
  facet_wrap(~ XAGEGROUP, labeller = as_labeller(c("1" = "25-39", "2" = "40-64", "3" = "65+"))) +  # Custom labels
  labs(title = "Boxplots of Follow-up Years by Education Level and Age Group",
       x = "Education Level (XEDUC)",
       y = "Follow-up Years") +
  custom_theme() +
  stat_boxplot(geom = "errorbar", width = 0.25)

# Slide 12: Mortality status by education, by age group ----

summary_data <- data %>%
  group_by(XAGEGROUP, XEDUC, STATUS) %>%
  summarise(Frequency = n(), .groups = 'drop')

ggplot(summary_data, aes(x = factor(XEDUC), y = Frequency, fill = factor(STATUS))) +
  geom_bar(stat = "identity", position = "dodge") +  # Use dodge position for side-by-side bars
  facet_wrap(~ XAGEGROUP) +  # Create separate plots for each XAGEGROUP
  labs(title = "Frequency of Mortality Status by Education Level and Age Group",
       x = "Education Level (XEDUC)",
       y = "Frequency",
       fill = "Mortality Status") +  # Legend title
  theme_minimal()

## custom theme ----

custom_colors <-c("#008bb0", "#d06f1a")


ggplot(summary_data, aes(x = factor(XEDUC), y = Frequency, fill = factor(STATUS))) +
  geom_bar(stat = "identity", position = "dodge") +  # Use dodge position for side-by-side bars
  facet_wrap(~ XAGEGROUP) +  # Create separate plots for each XAGEGROUP
  labs(title = "Frequency of Mortality Status by Education Level and Age Group",
       x = "Education Level (XEDUC)",
       y = "Frequency",
       fill = "Mortality Status") +  # Legend title
  scale_fill_manual(values = custom_colors) +  # Use custom color palette
  custom_theme()

# try adding percentages to graph

summary_data <- summary_data %>%
  group_by(XEDUC, XAGEGROUP) %>%
  mutate(Percentage = Frequency / sum(Frequency) * 100)  # Calculate percentage

# Create the ggplot
ggplot(summary_data, aes(x = factor(XEDUC), y = Frequency, fill = factor(STATUS))) +
  geom_bar(stat = "identity", position = "dodge") +  # Use dodge position for side-by-side bars
  facet_wrap(~ XAGEGROUP) +  # Create separate plots for each XAGEGROUP
  labs(title = "Frequency of Mortality Status by Education Level and Age Group",
       x = "Education Level (XEDUC)",
       y = "Frequency",
       fill = "Mortality Status") +  # Legend title
  scale_fill_manual(values = custom_colors) +  # Use custom color palette
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), 
            position = position_dodge(width = 0.9),
            size = 2.5,
            #angle = 45,
            vjust = -0.8) +  # Adjust vertical position of text
  custom_theme()

##  flip axes ----

ggplot(summary_data, aes(x = factor(XEDUC), y = Frequency, fill = factor(STATUS))) +
  geom_bar(stat = "identity", position = "dodge") +  # Use dodge position for side-by-side bars
  facet_wrap(~ XAGEGROUP, ncol = 1) +  # Stack plots vertically
  labs(title = "Frequency of Mortality Status by Education Level and Age Group",
       x = "Education Level (XEDUC)",
       y = "Frequency",
       fill = "Mortality Status") +  # Legend title
  scale_fill_manual(values = custom_colors) +  # Use custom color palette
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), 
            position = position_dodge(width = 0.9), 
            hjust = -0.2,  # Adjust horizontal justification for better placement
            size = 3,) + # Adjust the size of the text labels) 
  coord_flip() +  # Flip the axes
  custom_theme()

## further adjust axes ----

# Calculate percentages
summary_data <- summary_data %>%
  group_by(XEDUC, XAGEGROUP) %>%
  mutate(Percentage = Frequency / sum(Frequency) * 100)  # Calculate percentage

# Modify the factor levels of XEDUC to reverse the order
summary_data$XEDUC <- factor(summary_data$XEDUC, levels = rev(unique(summary_data$XEDUC)))

# Create the ggplot with flipped axes
ggplot(summary_data, aes(x = factor(XEDUC), y = Frequency, fill = factor(STATUS))) +
  geom_bar(stat = "identity", position = "dodge") +  # Use dodge position for side-by-side bars
  facet_wrap(~ XAGEGROUP, ncol = 1) +  # Stack plots vertically
  labs(title = "Frequency of Mortality Status by Education Level and Age Group",
       x = "Education Level (XEDUC)",
       y = "Frequency",
       fill = "Mortality Status") +  # Legend title
  scale_fill_manual(values = custom_colors) +  # Use custom color palette
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), 
            position = position_dodge(width = 0.9), 
            hjust = -0.2,  # Adjust horizontal justification for better placement
            size = 2.5) +  # Adjust the size of the text labels
  coord_flip() +  # Flip the axes
  custom_theme()

# slide 15: leading causes of death by education group ----


# Filter the data for XAGEGROUP == 1 and exclude MORTSTAT == 0
filtered_data <- data %>%
  filter(XAGEGROUP == 1, MORTSTAT != 0, XEDUC %in% 1:4) %>%
  mutate(UCOD_group = case_when(
    UCOD_LEADING %in% c("001", "002", "004", "005", "007", "010") ~ UCOD_LEADING,
    TRUE ~ "All others"
  ))

# Calculate percent frequencies for UCOD groups by XEDUC
percent_data <- filtered_data %>%
  group_by(XEDUC, UCOD_group) %>%
  summarise(count = n(), .groups = 'drop') %>%
  group_by(XEDUC) %>%  # Group by XEDUC for percentage calculation
  mutate(percent = (count / sum(count)) * 100)  # Calculate percent

# Create the ggplot with facets for each XEDUC group
ggplot(percent_data, aes(x = UCOD_group, y = percent, fill = UCOD_group)) + 
  geom_bar(stat = "identity") + 
  facet_wrap(~ XEDUC, ncol = 2) +  # Arrange in a square formation
  labs(title = "Percent Frequencies of UCOD_LEADING Groups by XEDUC Groups (XAGEGROUP == 1, MORTSTAT != 0)",
       x = "UCOD Groups",
       y = "Percent Frequency") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +  # Format y-axis as percentage
  scale_fill_brewer(palette = "Set1")  # Use a color palette for different bar

## try with labels and flipped axes ----

# Filter the data for XAGEGROUP == 1 and exclude MORTSTAT == 0
filtered_data <- data %>%
  filter(XAGEGROUP == 1, MORTSTAT != 0, XEDUC %in% 1:4) %>%
  mutate(UCOD_group = case_when(
    UCOD_LEADING == "001" ~ "Diseases of heart",
    UCOD_LEADING == "002" ~ "Malignant neoplasms",
    UCOD_LEADING == "004" ~ "Accidents",
    UCOD_LEADING == "005" ~ "Cerebrovascular disease",
    UCOD_LEADING == "007" ~ "Diabetes mellitus",
    UCOD_LEADING == "010" ~ "All other causes (residual)",
    TRUE ~ "All others"
  ))

# Calculate percent frequencies for UCOD groups by XEDUC
percent_data <- filtered_data %>%
  group_by(XEDUC, UCOD_group) %>%
  summarise(count = n(), .groups = 'drop') %>%
  group_by(XEDUC) %>%  # Group by XEDUC for percentage calculation
  mutate(percent = (count / sum(count)) * 100)  # Calculate percent

# Create the ggplot with facets for each XEDUC group and rotated axes
ggplot(percent_data, aes(x = UCOD_group, y = percent, fill = UCOD_group)) + 
  geom_bar(stat = "identity") + 
  facet_wrap(~ XEDUC, ncol = 2) +  # Arrange in a square formation
  labs(title = "Percent Frequencies of UCOD_LEADING Groups by XEDUC Groups (XAGEGROUP == 1, MORTSTAT != 0)",
       x = "UCOD Groups",
       y = "Percent Frequency") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +  # Format y-axis as percentage
  scale_fill_manual(values = c("Diseases of heart" = "blue",
                               "Malignant neoplasms" = "red",
                               "Accidents" = "green",
                               "Cerebrovascular disease" = "purple",
                               "Diabetes mellitus" = "orange",
                               "All other causes (residual)" = "pink",
                               "All others" = "grey"),
                    name = "UCOD Groups") +  # Custom colors for each group
  coord_flip()  # Rotate axes

## try with custom theme ----
ggplot(percent_data, aes(x = UCOD_group, y = percent, fill = UCOD_group)) + 
  geom_bar(stat = "identity") + 
  facet_wrap(~ XEDUC, ncol = 2) +  # Arrange in a square formation
  labs(title = "Percent Frequencies of UCOD_LEADING Groups by XEDUC Groups, Age 25-39 years",
       x = "UCOD Groups",
       y = "Percent Frequency") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +  # Format y-axis as percentage
  scale_fill_manual(values = custom_palette(), name = "UCOD Groups") +  # Call the function
  coord_flip() +  # Rotate axes
  custom_theme()  # Apply the custom theme

# Slide 18: cox proportional hazards model ----

## from AI tool ----

hzrt <- function(xld, data) {
  
  # Create a new data frame based on the input data
  cdemo_puf_model <- data %>%
    mutate(
      # Create the xlead variable based on the value of xld
      xlead = case_when(
        xld == "000" ~ ifelse(MORTSTAT %in% c(0, 1), MORTSTAT, NA),
        TRUE ~ ifelse(UCOD_LEADING == xld, 1, ifelse(MORTSTAT %in% c(0, 1), 0, NA))
      )
    )
  
  # Fit the Cox model
  cox_model <- svycoxph(Surv(PERY, xlead) ~ AGE_P + SEX + RACE_ETH + XMARITAL_STATUS + REGION + XEDUC,
                        design = svydesign(ids = ~PPSU, strata = ~PSTRAT, weights = ~SAWT_1218, data = cdemo_puf_model, nest = TRUE))
  
  # Extract parameter estimates
  hrpm <- summary(cox_model)$coefficients
  
  # Filter for the xeduc parameter
  hr_results <- hrpm[grep("^XEDUC", rownames(hrpm)), ]
  
  # Create a data frame to store results
  results <- data.frame(
    inset = xld,
    Parameter = rownames(hr_results),
    HazardRatio = exp(hr_results[, "coef"]),
    HRLowerCL = exp(hr_results[, "coef"] - 1.96 * hr_results[, "se(coef)"]),
    HRUpperCL = exp(hr_results[, "coef"] + 1.96 * hr_results[, "se(coef)"])
  )
  
  return(results)
}



# Example of running the function for different xld values
results_list <- list()
for (xld in c("000", "001", "002", "003", "004", "005")) {
  results_list[[xld]] <- hzrt(xld, data)
}

# Combine results into a single data frame
final_results <- bind_rows(results_list)

# View the final results
print(final_results)

## PROBLEM: Throwing an error; need to test Cindy's code instead

# tables ----

summary_stats <- data %>%
  filter(ELIGSTAT == 1) %>%
  summarise(
    n_age_p = sum(!is.na(AGE_P)),
    mean_age_p = mean(AGE_P, na.rm = TRUE),
    sum_age_p = sum(AGE_P, na.rm = TRUE),
    min_age_p = min(AGE_P, na.rm = TRUE),
    p25_age_p = quantile(AGE_P, 0.25, na.rm = TRUE),
    p75_age_p = quantile(AGE_P, 0.75, na.rm = TRUE),
    max_age_p = max(AGE_P, na.rm = TRUE),
    
    n_sawt_1218 = sum(!is.na(SAWT_1218)),
    mean_sawt_1218 = mean(SAWT_1218, na.rm = TRUE),
    sum_sawt_1218 = sum(SAWT_1218, na.rm = TRUE),
    min_sawt_1218 = min(SAWT_1218, na.rm = TRUE),
    p25_sawt_1218 = quantile(SAWT_1218, 0.25, na.rm = TRUE),
    p75_sawt_1218 = quantile(SAWT_1218, 0.75, na.rm = TRUE),
    max_sawt_1218 = max(SAWT_1218, na.rm = TRUE),
    
    n_pery = sum(!is.na(PERY)),
    mean_pery = mean(PERY, na.rm = TRUE),
    sum_pery = sum(PERY, na.rm = TRUE),
    min_pery = min(PERY, na.rm = TRUE),
    p25_pery = quantile(PERY, 0.25, na.rm = TRUE),
    p75_pery = quantile(PERY, 0.75, na.rm = TRUE),
    max_pery = max(PERY, na.rm = TRUE)
  )

# Print summary statistics
print(summary_stats)

# PROBLEM: right now this just prints a one-row table of all the stats; need it to look like the SAS original