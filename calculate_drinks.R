# Script by Vera Thornton (verathor14)

# Calculate drinks function as a separate unit for sharing with Yoonhoo's tobacco project
# This will generate a lot of warnings about coercion to NA, this conversion is intentional!
# Can set warning to FALSE to suppress these

# Why this function is better:
# Functionalization!
# Drink units not g/day, g/day is false precision because we don't actually have a reliable conversion for that
# In the past I was converting all missing values to 0, which means if I wanted to replace from
# baseline I had to replaced EVERY column here. This preserves true missings as NA in the drink_sum
# column so they can be backfilled.


calculate_drinks <- function(ukbio_alcohol) {
  #### Updated 3/14/22
  # Function takes the data frame containing alcohol fields from ONE instance of the UK Biobank touchscreen questionnaire
  # Converts drinks by type into an overall sum of drinks and then calculates year dose, week dose, drinks per day,
  # and binary binge
  # Also keeps track of how many values are missing at each step
  # Where no valid responses are provided the data frame contains NA (so we know there was no valid response)
  
  
  # Strip the visit indicators so the column names will match inside the function
  names(ukbio_alcohol) <- names(ukbio_alcohol) %>%
    gsub("_0_0", "", .) %>%
    gsub("_2_0", "", .)
  
  # Split on the monthly, weekly, and never drinkers  
  monthly_drinkers <- ukbio_alcohol %>%
    filter(n_1558 == "One to three times a month"
           | n_1558 == "Special occasions only") %>%
    mutate(q_group = "Monthly")
  
  weekly_drinkers <- ukbio_alcohol %>%
    filter(n_1558 == "Daily or almost daily"
           | n_1558 == "Three or four times a week"
           | n_1558 == "Once or twice a week") %>%
    mutate(q_group = "Weekly")
  
  never_drinkers <- ukbio_alcohol %>%
    filter(n_1558 == "Never") %>%
    mutate(q_group = "Never")
  
  missing_drinkers <- ukbio_alcohol %>%
    filter(n_1558 == "Prefer not to answer" |
             n_1558 == "") %>%
    mutate(q_group = "Missing") %>%
    mutate(n_1558 = NA) # Set 1558 (frequency) to NA so it is more clear it is missing
  
  # Keep tabs on missing data
  missing_report <- data.frame("data_field" = "1558 Frequency", "n_missing" = nrow(missing_drinkers))
  
  
  monthly_drinkers <- monthly_drinkers %>%
    mutate(n_4407 = as.numeric(n_4407)) %>% # Convert strings to numeric
    mutate(n_4418 = as.numeric(n_4418)) %>%
    mutate(n_4429 = as.numeric(n_4429)) %>%
    mutate(n_4440 = as.numeric(n_4440)) %>%
    mutate(n_4451 = as.numeric(n_4451)) %>%
    mutate(n_4462 = as.numeric(n_4462)) %>%
    rowwise() %>%
    # Add all the different drink types
    mutate(month_drinks = sum(n_4407, n_4418, n_4429, n_4440, n_4451, n_4462, na.rm = TRUE)) %>%
    ungroup() %>%
    # If all categories were NA, put NA back as the sum (it gets turned to 0 by na.rm = TRUE)
    mutate(month_drinks = ifelse(is.na(n_4407) & is.na(n_4418) & is.na(n_4429) & is.na(n_4440)
                                 & is.na(n_4451) & is.na(n_4462), NA, month_drinks)) %>%
    mutate(week_drinks = month_drinks / 4.3) %>%
    select(-month_drinks)
  
  # Update the missing report
  missing_report <- rbind(missing_report, data.frame("data_field" = "Monthly sum drinks",
                                                     "n_missing" = sum(is.na(monthly_drinkers$week_drinks))))
  
  # Repeat with weekly drinkers
  weekly_drinkers <- weekly_drinkers %>%
    mutate(n_1568 = as.numeric(n_1568)) %>%
    mutate(n_1578 = as.numeric(n_1578)) %>%
    mutate(n_1588 = as.numeric(n_1588)) %>%
    mutate(n_1598 = as.numeric(n_1598)) %>%
    mutate(n_1608 = as.numeric(n_1608)) %>%
    mutate(n_5364 = as.numeric(n_5364)) %>%
    rowwise() %>%
    mutate(week_drinks = sum(n_1568, n_1578, n_1588, n_1598, n_1608, n_5364, na.rm = TRUE)) %>%
    ungroup() %>%
    # If all categories were NA, put NA back as the sum (it gets turned to 0 by na.rm = TRUE)
    mutate(week_drinks = ifelse(is.na(n_1568) & is.na(n_1578) & is.na(n_1588) & is.na(n_1598)
                                & is.na(n_1608) & is.na(n_5364), NA, week_drinks))
  
  # Update the missing report
  missing_report <- rbind(missing_report, data.frame("data_field" = "Weekly sum drinks",
                                                     "n_missing" = sum(is.na(weekly_drinkers$week_drinks))))
  
  never_drinkers <- never_drinkers %>%
    mutate(week_drinks = 0)
  
  missing_drinkers <- missing_drinkers %>%
    mutate(week_drinks = NA)
  
  all_drinkers <- rbind(monthly_drinkers, weekly_drinkers, never_drinkers, missing_drinkers)
  
  
  # Update the missing report
  missing_report <- rbind(missing_report, data.frame("data_field" = "All sum drinks",
                                                     "n_missing" = sum(is.na(all_drinkers$week_drinks))))
  
  
  # Missing report gets printed, not returned
  missing_report <- missing_report %>%
    mutate(percent_missing = n_missing / nrow(ukbio_alcohol) * 100)
  
  print(missing_report)
  
  return(all_drinkers)
}




imaging <- calculate_drinks(imaging)
