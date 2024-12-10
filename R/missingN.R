# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

Missing_Nasir <- function(data) {
  # Get total number of rows and columns
  n_rows <- nrow(data)
  n_cols <- ncol(data)

  # Calculate missing values statistics
  missing_by_column <- sapply(data, function(x) sum(is.na(x)))
  missing_by_row <- apply(data, 1, function(x) sum(is.na(x)))

  # Calculate percentages
  pct_missing_by_column <- (missing_by_column / n_rows) * 100

  # Create summary statistics
  total_missing <- sum(missing_by_column)
  pct_total_missing <- (total_missing / (n_rows * n_cols)) * 100

  # Create summary dataframe for columns
  column_summary <- data.frame(
    column_name = names(missing_by_column),
    missing_count = missing_by_column,
    missing_percentage = round(pct_missing_by_column, 2)
  )

  # Sort by missing count in descending order
  column_summary <- column_summary[order(-column_summary$missing_count), ]

  # Identify columns with no missing values
  complete_columns <- names(missing_by_column[missing_by_column == 0])

  # Create result list
  result <- list(
    total_missing_values = total_missing,
    total_missing_percentage = round(pct_total_missing, 2),
    rows_with_missing = sum(missing_by_row > 0),
    columns_with_missing = sum(missing_by_column > 0),
    column_summary = column_summary,
    complete_columns = complete_columns,
    rows_missing_count = missing_by_row
  )

  return(result)
}
