#Function to check how many NAs there are in source system patient id column.
#It is important to check this, since source system patient id is the primary key to obtain
#variables from other datasets within the Vaccination Data Layer

check_na_percentage <- function(df, column_name) {
  if (!column_name %in% names(df)) {
    message("Column '", column_name, "' is not present in the data frame.")
    return(invisible(NULL))
  }
  
  na_sum <- sum(is.na(df[[column_name]]))
  total_length <- length(df[[column_name]])
  percentage <- (na_sum / total_length) * 100
  
  message("There are ", na_sum, " NA values in the '", column_name, "' column, out of a total ", total_length)
  message("This accounts for ", round(percentage, 2), "% of all entries.")
  
  return(list(
    column = column_name,
    na_count = na_sum,
    total = total_length,
    na_percentage = percentage
  ))
}


check_analysis_significant_missing <- function(df) {
  # Define variables to check
  vars_to_check <- c(
    "source_system_patient_id",
    "cohort_group_ML_analysis",
    "patient_sex",
    "patient_age",
    "SIMD_quintile",
    "UR8_2022",
    "patient_postcode_ML_analysis"
  )
  
  # Check for missing columns
  missing_cols <- setdiff(vars_to_check, names(df))
  if (length(missing_cols) > 0) {
    message("The following required columns are missing from the dataframe: ", paste(missing_cols, collapse = ", "))
    return(invisible(NULL))
  }
  
  total_rows <- nrow(df)
  
  # Report NA or missing values per column
  for (var in vars_to_check) {
    na_count <- sum(is.na(df[[var]]) | df[[var]] == "")
    na_percent <- round((na_count / total_rows) * 100, 2)
    message("In '", var, "' column, there are ", na_count, " missing entries (", na_percent, "% of total rows).")
  }
  
  # Rows with at least one missing analysis-significant value
  missing_row_count <- df %>%
    select(all_of(vars_to_check)) %>%
    mutate(across(everything(), ~ ifelse(. == "", NA, .))) %>%
    filter(if_any(everything(), is.na)) %>%  
    nrow()
  
  missing_row_percent <- round((missing_row_count / total_rows) * 100, 2)
  
  message("\nThere are ", missing_row_count,
          " rows where at least one analysis-significant variable is missing (",
          missing_row_percent, "% of total rows).")
  
  # Ask to remove rows with missing values
  if (missing_row_count > 0) {
    user_response <- readline(prompt = "Do you want to remove rows with missing analysis-significant variables? (yes/no): ")
    
    if (tolower(trimws(user_response)) == "yes") {
      # Remove rows with missing values
      cleaned_df <- df %>%
        mutate(across(all_of(vars_to_check), ~ ifelse(. == "", NA, .))) %>%
        filter(!if_any(all_of(vars_to_check), is.na))
      
      rows_removed <- total_rows - nrow(cleaned_df)
      message("\nDataset has been cleaned. ", rows_removed, " rows removed.")
      message("New dataset has ", nrow(cleaned_df), " rows.")
      
      return(cleaned_df)
    } else {
      message("\nDataset has not been changed. No rows were removed.")
      return(df)
    }
  } else {
    message("\nNo missing values found. Dataset remains unchanged.")
    return(df)
  }
}

#Function to scape vaccination_event_analysis data 
vaccination_event_data <- function(output_name) {
  data <- tbl(DVPROD, "vaccination_event_analysis") %>%
    filter(
      vacc_status == "completed",
      vacc_type_target_disease == "Influenza (disorder)",
      vacc_occurence_time >= vacc_min_date,
      vacc_occurence_time <= vacc_max_date
    ) %>%
    select(
      source_system_patient_id,
      vacc_event_created_at
    ) %>%
    #
    group_by(source_system_patient_id) %>%
    window_order(source_system_patient_id, vacc_event_created_at) %>%
    distinct(source_system_patient_id, .keep_all = TRUE) %>%
    collect()
    setDT(data)
  
  # Dynamically assign the result to the name passed in
  assign(output_name, data, envir = .GlobalEnv)
}


#Function to scape vaccination_patient_cohort_analysis_audit data 
vaccination_patient_cohort_analysis_audit_data <- function(output_name) {
  data <- tbl(DVPROD, "vaccination_patient_cohort_analysis_audit") %>%
    filter(
      cohort_phase %in% cohort_phase_input
    ) %>%
    select(cohort,
           cohort_phase,
           source_system_patient_id,
           cohort_target_diseases,
           patient_derived_post_code,
           patient_current_postcode,
           cohort_post_code,
           patient_sex,
           patient_date_of_birth
           
    ) %>%
    collect() %>%
    distinct(source_system_patient_id, .keep_all = TRUE) %>%
    mutate(patient_age = as.integer(floor(time_length(interval(patient_date_of_birth, end_of_season), "years"))))%>%
    mutate(patient_postcode_ML_analysis = clean_postcode_column(coalesce(patient_current_postcode, patient_derived_post_code,cohort_post_code))) %>%
    left_join(master_postcode_df, by = "patient_postcode_ML_analysis")
    
    setDT(data)
  
  assign(output_name, data, envir = .GlobalEnv)
}

  
process_cohort_data <- function() {

  data <- cohort_dataset %>%
    select(source_system_patient_id, cohort_group_ML_analysis) %>%
    left_join(patient_cohort_dataset_name, by = "source_system_patient_id")
  cleaned_data <- check_analysis_significant_missing(data)
  final_dataset <- cleaned_data %>%
    left_join(event_analysis_dataset_name, by = "source_system_patient_id") %>%
    mutate(attended_vaccination_event = !is.na(vacc_event_created_at)) %>%
  
  return(invisible(final_dataset))
}


process_all_cohorts <- function() {
  files <- list.files(input_dir, pattern = "_cleaned\\.rds$", full.names = TRUE)
  
  for (file_path in files) {
    file_name <- basename(file_path)
    file_base <- str_remove(file_name, "\\.rds$")
    cohort_name <- str_replace(file_base, "cleaned", "final")
    
    cohort_dataset <- readRDS(file_path)
    
    assign("cohort_dataset", cohort_dataset, envir = .GlobalEnv)
    message("Processing ", cohort_name)
    
    final_dataset <- process_cohort_data()
    
    assign(cohort_name, final_dataset, envir = .GlobalEnv)
    
    output_path <- file.path(output_dir, paste0(cohort_name, ".rds"))
    saveRDS(final_dataset, output_path)
    
    message("Processed and saved: ", output_path)
  }
}


clean_postcode_column <- function(postcode_column) {
  postcode_column %>%
    toupper() %>%
    str_replace_all("\\s+", "") %>%
    str_replace_all("[^A-Z0-9]", "")
}




