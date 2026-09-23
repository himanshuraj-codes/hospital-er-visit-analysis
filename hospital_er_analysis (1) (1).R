## =============================================================
## Hospital Emergency Room (ER) Visit and Patient Satisfaction Analysis
## Author: [Your Name]
## Date: 2026-09-22
## =============================================================

# ---- 3. Data Importing ----
library(ggplot2)

er_data <- read.csv("hospital_er_data.csv")
head(er_data)

# ---- 4. Understanding the Dataset ----
cat("Number of records:", nrow(er_data))
cat("Number of variables:", ncol(er_data))
names(er_data)
str(er_data)
summary(er_data)

# ---- 5. Data Preprocessing and Cleaning ----
missing_values <- colSums(is.na(er_data))
missing_values

duplicate_count <- sum(duplicated(er_data))
cat("Number of duplicate records:", duplicate_count)

sapply(er_data, class)
unique(er_data$Visit_Status)
unique(er_data$Department_Type)

# ---- 6. Creating a Clean Dataset ----
er_clean <- er_data
er_clean <- er_clean[!duplicated(er_clean), ]
er_clean <- er_clean[
  !is.na(er_clean$Consultation_Fee) &
  !is.na(er_clean$Additional_Charges) &
  !is.na(er_clean$Total_Bill) &
  !is.na(er_clean$Wait_Time_Min) &
  !is.na(er_clean$Treatment_Time_Min) &
  !is.na(er_clean$Patient_Rating),
]
er_clean <- er_clean[
  er_clean$Consultation_Fee >= 0 &
  er_clean$Additional_Charges >= 0 &
  er_clean$Total_Bill >= 0 &
  er_clean$Wait_Time_Min >= 0 &
  er_clean$Treatment_Time_Min >= 0,
]
cat("Original number of records:", nrow(er_data), "\n")
cat("Records after cleaning:", nrow(er_clean), "\n")
cat("Records removed:", nrow(er_data) - nrow(er_clean))

# ---- 7. Basic Statistical Analysis ----
fee_stats <- c(
  Mean = mean(er_clean$Consultation_Fee),
  Median = median(er_clean$Consultation_Fee),
  Minimum = min(er_clean$Consultation_Fee),
  Maximum = max(er_clean$Consultation_Fee),
  Standard_Deviation = sd(er_clean$Consultation_Fee)
)
fee_stats

bill_stats <- c(
  Mean = mean(er_clean$Total_Bill),
  Median = median(er_clean$Total_Bill),
  Minimum = min(er_clean$Total_Bill),
  Maximum = max(er_clean$Total_Bill),
  Standard_Deviation = sd(er_clean$Total_Bill)
)
bill_stats

# ---- 8. Visit Status Analysis ----
status_count <- table(er_clean$Visit_Status)
status_data <- as.data.frame(status_count)
names(status_data) <- c("Visit_Status", "Number_of_Visits")
ggplot(status_data, aes(x = Visit_Status, y = Number_of_Visits)) +
  geom_col() +
  labs(title = "Distribution of ER Visit Status",
       x = "Visit Status", y = "Number of Visits") +
  theme_minimal()

# ---- 9. Department Type Analysis ----
dept_count <- table(er_clean$Department_Type)
dept_data <- as.data.frame(dept_count)
names(dept_data) <- c("Department_Type", "Number_of_Visits")
ggplot(dept_data, aes(x = reorder(Department_Type, Number_of_Visits),
                       y = Number_of_Visits)) +
  geom_col() + coord_flip() +
  labs(title = "Number of Visits by Department Type",
       x = "Department Type", y = "Number of Visits") +
  theme_minimal()

# ---- 10. Hospital Analysis ----
hospital_count <- table(er_clean$Hospital_Name)
hospital_data <- as.data.frame(hospital_count)
names(hospital_data) <- c("Hospital_Name", "Number_of_Visits")
hospital_data <- hospital_data[order(-hospital_data$Number_of_Visits), ]
head(hospital_data, 10)

ggplot(head(hospital_data, 10),
       aes(x = reorder(Hospital_Name, Number_of_Visits), y = Number_of_Visits)) +
  geom_col() + coord_flip() +
  labs(title = "Top Hospitals by Number of ER Visits",
       x = "Hospital", y = "Number of Visits") +
  theme_minimal()

# ---- 11. Consultation Fee Analysis ----
ggplot(er_clean, aes(x = Consultation_Fee)) +
  geom_histogram(bins = 20) +
  labs(title = "Distribution of Consultation Fees",
       x = "Consultation Fee", y = "Number of Visits") +
  theme_minimal()

# ---- 12. Total Bill Analysis ----
ggplot(er_clean, aes(x = Total_Bill)) +
  geom_histogram(bins = 20) +
  labs(title = "Distribution of Total Bill",
       x = "Total Bill", y = "Number of Visits") +
  theme_minimal()

# ---- 13. Treatment Time Analysis ----
ggplot(er_clean, aes(x = Treatment_Time_Min)) +
  geom_histogram(bins = 20) +
  labs(title = "Distribution of Treatment Time",
       x = "Treatment Time (Minutes)", y = "Number of Visits") +
  theme_minimal()

# ---- 14. Wait Time Analysis ----
ggplot(er_clean, aes(x = Wait_Time_Min)) +
  geom_histogram(bins = 20) +
  labs(title = "Distribution of Patient Wait Time",
       x = "Wait Time (Minutes)", y = "Number of Visits") +
  theme_minimal()

# ---- 15. Patient Rating Analysis ----
rating_count <- table(er_clean$Patient_Rating)
average_rating <- mean(er_clean$Patient_Rating, na.rm = TRUE)
cat("Average patient rating:", round(average_rating, 2))

# ---- 16. Patient Analysis ----
number_of_patients <- length(unique(er_clean$Patient_ID))
cat("Number of unique patients:", number_of_patients)

patient_visits <- sort(table(er_clean$Patient_ID), decreasing = TRUE)
head(patient_visits, 10)

# ---- 17. Total Bill by Department ----
dept_bill <- aggregate(Total_Bill ~ Department_Type, data = er_clean, FUN = mean)
names(dept_bill)[2] <- "Average_Total_Bill"
dept_bill <- dept_bill[order(-dept_bill$Average_Total_Bill), ]
dept_bill

# ---- 18. Treatment Time and Patient Rating ----
ggplot(er_clean, aes(x = Treatment_Time_Min, y = Patient_Rating)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Relationship Between Treatment Time and Patient Rating",
       x = "Treatment Time (Minutes)", y = "Patient Rating") +
  theme_minimal()

# ---- 19. Consultation Fee and Total Bill ----
ggplot(er_clean, aes(x = Consultation_Fee, y = Total_Bill)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Relationship Between Consultation Fee and Total Bill",
       x = "Consultation Fee", y = "Total Bill") +
  theme_minimal()

# ---- 20. Correlation Analysis ----
correlation_data <- er_clean[, c("Consultation_Fee", "Additional_Charges",
                                  "Total_Bill", "Wait_Time_Min",
                                  "Treatment_Time_Min", "Patient_Rating")]
correlation_matrix <- cor(correlation_data, use = "complete.obs")
round(correlation_matrix, 3)

# ---- 21. Total Visit Time Analysis ----
total_time_stats <- c(
  Mean = mean(er_clean$Total_Visit_Time),
  Median = median(er_clean$Total_Visit_Time),
  Minimum = min(er_clean$Total_Visit_Time),
  Maximum = max(er_clean$Total_Visit_Time),
  Standard_Deviation = sd(er_clean$Total_Visit_Time)
)
total_time_stats

ggplot(er_clean, aes(x = Total_Visit_Time)) +
  geom_histogram(bins = 20) +
  labs(title = "Distribution of Total Visit Time",
       x = "Total Visit Time (Minutes)", y = "Number of Visits") +
  theme_minimal()

# ---- 22. Overall Project Summary ----
overall_summary <- data.frame(
  Total_Visits = nrow(er_clean),
  Unique_Patients = length(unique(er_clean$Patient_ID)),
  Unique_Hospitals = length(unique(er_clean$Hospital_Name)),
  Unique_Departments = length(unique(er_clean$Department_Type)),
  Average_Consultation_Fee = mean(er_clean$Consultation_Fee),
  Average_Total_Bill = mean(er_clean$Total_Bill),
  Average_Wait_Time = mean(er_clean$Wait_Time_Min),
  Average_Treatment_Time = mean(er_clean$Treatment_Time_Min),
  Average_Rating = mean(er_clean$Patient_Rating)
)
overall_summary
