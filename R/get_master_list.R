get_master_list <- function() {
    # Function to get master list of drugs from LiverTox
    # Arguments: none
    # Returns: data frame of master list, cleaned minimally

    # Scrape URL of master list Excel file
    # URL changes with updates, hence the need to scrape
    master_list_site_url <- "https://www.ncbi.nlm.nih.gov/books/NBK571102/"
    master_list_file_url <- read_html(master_list_site_url) |>
        html_nodes("a:contains('here')") |>
        html_attr("href") |>
        url_absolute(master_list_site_url)
    # Download master list Excel file to temp directory
    temp_file <- tempfile(fileext = ".xlsx")
    download.file(
        master_list_file_url,
        temp_file,
        mode = "wb"
    )
    # Read master list Excel file into R
    master_list <- readxl::read_excel(temp_file,
        skip = 1
    ) |>
        janitor::clean_names()
    # Select columns and rows of interest
    # Excel file contains summary statistics at the end that need to be removed
    # Find first row with all NA values and drop that row and all rows after
    # Drop first column (index of row numbers)
    first_na_row <- apply(master_list, 1, function(x) all(is.na(x))) |>
        which() |>
        first()
    master_list <- master_list[1:(first_na_row - 1), 2:ncol(master_list)]
}
