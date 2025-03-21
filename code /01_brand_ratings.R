# -----------------------------------------------------------------------------
# Script purpose: processing survey data to obtain brand ratings.
# -----------------------------------------------------------------------------


# load libraries
library(dplyr)

# read raw survey data
ratings <- read.csv("raw_data/survey_raw_data.csv")

# define function to extract brand names in text
extract_brands <- function(input_string) {
  # Split the string on dashes
  parts <- strsplit(input_string, " - ")[[1]]    
  return(trimws(parts[2]))  
}

# apply function to rating columns to obtain brand names
brand_names <- sapply(ratings[1,15:269], extract_brands)
names(ratings)[15:269] <- brand_names

# filter responses
ratings <- ratings %>% 
  filter(intro=="I consent to take part in this study" &
         Progress=="100") %>% 
  select(-c(1:14)) %>% # remove unnecessary columns 
  mutate(age = as.numeric(age))

# brands in two groups
brands_group1 <- brand_names[1:127]
brands_group2 <- brand_names[128:255]

# convert brand rating columns to numeric
for (column in brand_names) {
  ratings[[column]] <- as.numeric(as.character(ratings[[column]]))
}



# participants demographics

## average age
mean(ratings$age,na.rm = T)

# gender distribution
frequency <- table(ratings$gender)
total <- sum(frequency)  
percentages <- (frequency / total)
print(percentages)


# separate rating data for two groups
ratings_group1 <- ratings[ratings$group=="1",brands_group1]
ratings_group2 <- ratings[ratings$group=="2",brands_group2]

# calculate 4 values for each brand: percent of empty responses, average rating
# after removing outliers, lower and upper level of 95% confidence interval.

process_ratings <- function(x) {
  ## Removing outliers: values outside 2 standard deviations
  clean_x <- x[x > (mean(x, na.rm = TRUE) - 2*sd(x, na.rm = TRUE)) & 
                 x < (mean(x, na.rm = TRUE) + 2*sd(x, na.rm = TRUE))]
  
  ## calculate mean
  mean_val <- mean(clean_x, na.rm = TRUE)
  
  ## calculate 95% confidence intervals
  n <- length(clean_x)
  stderr <- sd(clean_x, na.rm = TRUE) / sqrt(n)
  lower_ci <- mean_val - 1.96 * stderr
  upper_ci <- mean_val + 1.96 * stderr
  
  ## calculate percent of empty responses
  percent_na <- sum(is.na(x)) / length(x) 
  
  # Return a result list 
  list(mean = mean_val, lower_ci = lower_ci, upper_ci = upper_ci, perc_na=percent_na)
}

# get the rating results for each brand group
results_group1 <- do.call(rbind, lapply(ratings_group1, process_ratings))
results_group2 <- do.call(rbind, lapply(ratings_group2, process_ratings))

# combine 2 groups
results <- rbind(results_group1,results_group2)

write.csv(results, "clean_data/survey_ratings.csv")
