### Function to download the cost reports
download_CMSCostReport <- function(zipfile_url, # in quotes
                                   ALPHA.CSV, # in quotes
                                   NMRC.CSV, # in quotes
                                   RPT.CSV # in quotes
){
  
  # Dowloads a zip file, which contains the 3 files needed
  #
  # Args:
  #   zipfile_url: the url of the zipfile
  #   ALPHA.CSV = the name of the alphanumeric CSV file to extract and download
  #   NMRC.CSV = the name of the numeric CSV file to extract and download
  #   RPT.CSV = the name of the RPT CSV file to extract and download
  # Notes:
  # Returns:
  #   list, with 3 elements; each element is a dataset
  
  # create a temp directory
  temp <- tempfile() 
  
  # download the alphanumeric zip folder
  # "wb" for text data
  download.file(zipfile_url,temp, mode = "wb") # wb for text data
  
  # once file is downloaded, need to unzip the desired file
  unzip(temp, ALPHA.CSV) 
  unzip(temp, NMRC.CSV)
  unzip(temp, RPT.CSV)
  
  # read in the unzipped file
  df_ALPHA <- fread(ALPHA.CSV, sep = ",", header = FALSE)
  df_NMRC <- fread(NMRC.CSV, sep = ",", header = FALSE)
  df_RPT <- fread(RPT.CSV, sep = ",", header = FALSE)
  
  # no longer need the temp file, you will be working from the environment
  unlink(temp)
  
  # assign column names
  colnames(df_ALPHA) <- c("RPT_REC_NUM","WKSHT_CD","LINE_NUM","CLMN_NUM","ITM")
  colnames(df_NMRC) <- c("RPT_REC_NUM","WKSHT_CD","LINE_NUM","CLMN_NUM","ITM")
  colnames(df_RPT) <- c("RPT_REC_NUM","PRVDR_CTRL_TYPE_CD","PRVDR_NUM","NPI","RPT_STUS_CD","FY_BGN_DT","FY_END_DT","PROC_DT","INITL_RPT_DT","LAST_RPT_DT","TRNSMTL_NUM","FI_NUM","ADR_VNDR_CD","FI_CREAT_DT","UTIL_CD","NPR_DT","SPEC_IND","FI_RCPT_DT")
  
  # create list of dataframes
  SNF10_year <- list(df_ALPHA, df_NMRC, df_RPT)
  
  return(SNF10_year)
  
}



### Function to confirm column/data types
data_types_func <- function(df) {
  
  # Change column types 
  #
  # Args:
  #   df: dataframe to manipulate
  # Notes:
  #   Only meant to manipulate Alphanumeric-Numeric dataset.
  # Returns:
  #   data.table
  
  
  df$RPT_REC_NUM <- as.character(df$RPT_REC_NUM)
  df$WKSHT_CD <- as.character(df$WKSHT_CD) 
  df$LINE_NUM <- as.character(df$LINE_NUM)
  df$CLMN_NUM <- as.character(df$CLMN_NUM)
  df$ITM <- as.character(df$ITM)
  
  return(df)
}



### Function to extract, transform, and format the Alphanumeric-Numeric dataset
# the dplyr piping operator is used to more easily see the step-through process
format_an <- function(df){
  
  # Extracts, transform, and format Alphanumeric-Numeric dataset
  #
  # Args:
  #   df: dataframe to manipulate
  # Notes:
  #   Only meant to manipulate Alphanumeric-Numeric dataset.
  #
  #   A new data frame is created for each variable we extract.
  #   
  #   To gather different/more data, this function can be manipulated to
  #   include the correct Worksheet Code, Line Number, and Column Number.
  #   Bulk data pull can be done by only referring to the
  #   the Worksheet Code and Column Number.
  # Returns:
  #   data.table
  
  # here, we are grabbing the name of the SNF
  SNF_Name <- df %>% 
    filter(WKSHT_CD == "S200001", 
           LINE_NUM == "400", 
           CLMN_NUM == "100") %>% 
    arrange(RPT_REC_NUM) %>% 
    select(RPT_REC_NUM, ITM) %>%
    rename(SNF_Name = ITM) 
  
  # A method to convert the dataset from long to wide was attempted. 
  # Computer memory was maxed out. 
  # For reproducibility purposes, this more verbose and timely approach
  # of extracting data variable-by-variable is used. 
  
  # The above example of extracting the SNF Name was written 
  # with the intent of how pipes should be used
  # To conserve on lines of code, single lines will be written for 
  # variable extraction.
  
  # SNF info
  ProviderNumber <- df %>% filter(WKSHT_CD == "S200001", LINE_NUM == "400", CLMN_NUM == "200") %>% arrange(RPT_REC_NUM) %>% select(RPT_REC_NUM, ITM) %>% rename(ProviderNumber = ITM)
  Address <- df %>% filter(WKSHT_CD == "S200001", LINE_NUM == "100", CLMN_NUM == "100") %>% arrange(RPT_REC_NUM) %>% select(RPT_REC_NUM, ITM) %>% rename(Address = ITM)
  City <- df %>% filter(WKSHT_CD == "S200001", LINE_NUM == "200", CLMN_NUM == "100") %>% arrange(RPT_REC_NUM) %>% select(RPT_REC_NUM, ITM) %>% rename(City = ITM)
  State <- df %>% filter(WKSHT_CD == "S200001", LINE_NUM == "200", CLMN_NUM == "200") %>% arrange(RPT_REC_NUM) %>% select(RPT_REC_NUM, ITM) %>% rename(State = ITM)
  ZipCode <- df %>% filter(WKSHT_CD == "S200001", LINE_NUM == "200", CLMN_NUM == "300") %>% arrange(RPT_REC_NUM) %>% select(RPT_REC_NUM, ITM) %>% rename(ZipCode = ITM)
  County <- df %>% filter(WKSHT_CD == "S200001", LINE_NUM == "300", CLMN_NUM == "100") %>% arrange(RPT_REC_NUM) %>% select(RPT_REC_NUM, ITM) %>% rename(County = ITM)
  
  # Bed Info
  SNF_BedCount <- df %>% filter(WKSHT_CD == "S300001", LINE_NUM == "100", CLMN_NUM == "00100") %>% arrange(RPT_REC_NUM) %>% select(RPT_REC_NUM, ITM) %>% rename(SNF_BedCount = ITM) %>% mutate(SNF_BedCount = as.integer(SNF_BedCount))
  SNF_BedDaysAvailable <- df %>% filter(WKSHT_CD == "S300001", LINE_NUM == "100", CLMN_NUM == "00200") %>% arrange(RPT_REC_NUM) %>% select(RPT_REC_NUM, ITM) %>% rename(SNF_BedDaysAvailable = ITM) %>% mutate(SNF_BedDaysAvailable = as.integer(SNF_BedDaysAvailable))
  
  # Note:
  # Title V pertains to Maternal and Child Health Services, 
  # this is excluded from the analysis as it is out of scope of Senior Living
  # Title XVIII pertains to Medicare funded
  # Title XIX pertains to Medicaid funded
  # Other pertains to Private Pay or Other funded methods
  
  # SNF Inpateint Days:
  SNF_MedicareInpatientDays <- df %>% filter(WKSHT_CD == "S300001", LINE_NUM == "100", CLMN_NUM == "00400") %>% arrange(RPT_REC_NUM) %>% select(RPT_REC_NUM, ITM) %>% rename(SNF_MedicareInpatientDays = ITM) %>% mutate(SNF_MedicareInpatientDays = as.integer(SNF_MedicareInpatientDays))
  SNF_MedicaidInpatientDays <- df %>% filter(WKSHT_CD == "S300001", LINE_NUM == "100", CLMN_NUM == "00500") %>% arrange(RPT_REC_NUM) %>% select(RPT_REC_NUM, ITM) %>% rename(SNF_MedicaidInpatientDays = ITM) %>% mutate(SNF_MedicaidInpatientDays = as.integer(SNF_MedicaidInpatientDays))
  SNF_OtherInpatientDays <- df %>% filter(WKSHT_CD == "S300001", LINE_NUM == "100", CLMN_NUM == "00600") %>% arrange(RPT_REC_NUM) %>% select(RPT_REC_NUM, ITM) %>% rename(SNF_OtherInpatientDays = ITM) %>% mutate(SNF_OtherInpatientDays = as.integer(SNF_OtherInpatientDays))
  
  # SNF Discharges
  SNF_MedicareDischarges <- df %>% filter(WKSHT_CD == "S300001", LINE_NUM == "100", CLMN_NUM == "00900") %>% arrange(RPT_REC_NUM) %>% select(RPT_REC_NUM, ITM) %>% rename(SNF_MedicareDischarges = ITM) %>% mutate(SNF_MedicareDischarges = as.integer(SNF_MedicareDischarges))
  SNF_MedicaidDischarges <- df %>% filter(WKSHT_CD == "S300001", LINE_NUM == "100", CLMN_NUM == "01000") %>% arrange(RPT_REC_NUM) %>% select(RPT_REC_NUM, ITM) %>% rename(SNF_MedicaidDischarges = ITM) %>% mutate(SNF_MedicaidDischarges = as.integer(SNF_MedicaidDischarges))
  SNF_OtherDischarges <- df %>% filter(WKSHT_CD == "S300001", LINE_NUM == "100", CLMN_NUM == "01100") %>% arrange(RPT_REC_NUM) %>% select(RPT_REC_NUM, ITM) %>% rename(SNF_OtherDischarges = ITM) %>% mutate(SNF_OtherDischarges = as.integer(SNF_OtherDischarges))
  
  # SNF Average Length of Stay (ALOS)
  SNF_MedicareALOS <- df %>% filter(WKSHT_CD == "S300001", LINE_NUM == "100", CLMN_NUM == "01400") %>% arrange(RPT_REC_NUM) %>% select(RPT_REC_NUM, ITM) %>% rename(SNF_MedicareALOS = ITM) %>% mutate(SNF_MedicareALOS = as.integer(SNF_MedicareALOS))
  SNF_MedicaidALOS <- df %>% filter(WKSHT_CD == "S300001", LINE_NUM == "100", CLMN_NUM == "01500")%>% arrange(RPT_REC_NUM) %>% select(RPT_REC_NUM, ITM) %>% rename(SNF_MedicaidALOS = ITM) %>% mutate(SNF_MedicaidALOS = as.integer(SNF_MedicaidALOS))
  # Other ALOS not provided in cost report
  
  # each variable is its own dataframe
  # now the dataframes must be joined together
  
  # Joining multiple tables:
  formatted_df <- list(ProviderNumber,
                       SNF_Name, 
                       Address, 
                       City, 
                       State,
                       ZipCode, 
                       County,
                       SNF_BedCount,
                       SNF_BedDaysAvailable,
                       SNF_MedicareInpatientDays,
                       SNF_MedicaidInpatientDays,
                       SNF_OtherInpatientDays,
                       SNF_MedicareDischarges,
                       SNF_MedicaidDischarges,
                       SNF_OtherDischarges,
                       SNF_MedicareALOS,
                       SNF_MedicaidALOS) %>%
    Reduce(function(dtf1, dtf2) left_join(dtf1, dtf2, by = "RPT_REC_NUM"), .)
  
  # Clean Zipcodes:
  # remove last four digits of zip code
  # add leading zeros
  formatted_df$ZipCode <- clean.zipcodes(formatted_df$ZipCode)
  
  setDT(formatted_df)
  setkey(formatted_df, RPT_REC_NUM)
  
  return(formatted_df)
  
}



### Function to extract, transform, and format the RPT dataset
format_rpt <- function(df){
  
  # Extracts, transform, and format RPT dataset
  #
  # Args:
  #   df: dataframe to manipulate
  # Notes:
  #   only meant to manipulate RPT dataset
  # Returns:
  #   data.table
  
  # add leading zeros to provider numbers
  df$PRVDR_NUM <- str_pad(df$PRVDR_NUM, 6, pad = "0")
  
  # convert RPT_REC_NUM to a character variable
  df$RPT_REC_NUM <- as.character(df$RPT_REC_NUM)
  
  # convert date fields from character to date variables
  df$FY_BGN_DT <- as.Date(df$FY_BGN_DT, format= "%m/%d/%Y")
  df$FY_END_DT <- as.Date(df$FY_END_DT, format= "%m/%d/%Y")
  
  # extract year
  df$YEAR <- year(df$FY_END_DT)
  
  # calculate days in a cost reporting period
  # assumption: need to scale values by days for a fair comparison
  # i.e. a provider with a longer cost reporting period will naturally have 
  # more discharges than a provider with a shorter cost reporting period simply
  # due to length of time for discharges to accrue 
  df$DAYS_IN_COST_REPORTING_PERIOD <- as.numeric(df$FY_END_DT - df$FY_BGN_DT)
  
  setkey(df, RPT_REC_NUM)
  
  return(df)
}
