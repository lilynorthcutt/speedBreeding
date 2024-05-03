readALlSheets <- function(filepath){
  '
  Function that reads in all sheets in an excel workbook into a tibble and 
  Input: 
  - filepath (char): path to excel workbook
  Output:
  - X (tibble): contains all sheets in workbook, with sheet names
  '
  sheets <- readxl::excel_sheets(filepath)
  x <- lapply(sheets, function(X) readxl::read_excel(filepath, sheet = X))
  x <- lapply(x, as.data.frame)
  names(x) <- sheets
  
  return(x)
}


# Inputs
filepath <- "Data/Speed Breeding 2023data.xlsx"
sheetnames <- "Seedlings"

### Function ###

sheets <- sapply(c("SB", "C"), function (x) paste0(x, " ", sheetnames))

# Get Dates list 
  ## (remove words b/c at the end it says "TERMINARED" )

# Get unique column names list

# Read in first column (SB Entry)

  ## Read in just data for entry in column list i

  ## Add dates as the colnames


  ## pivot longer s.t. 



# Read in 
