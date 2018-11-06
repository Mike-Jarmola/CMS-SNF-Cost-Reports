library(data.table)
library(zipcode)
library(tidyverse)
library(scales)
library(lubridate)

# The files downloaded are too big to store in a repository on their own.
# Without the downloaded files, a reproducible example would be hard to come
# up with, hence, the data are directly downloaded. 
# The download may take 6 to 8 minutes for each year to download. 
# Time may vary across users. 

# source functions
source("//ds/home/michaeja/Desktop/CMS SNF Cost Report Functions.R")

# The cleaned dataset is available in the repository if the user
# would like to start from here and not wait for the transformation steps below.
# Once line 19 imports the dataset, start at line 86
#snf_df <- fread("CMS_Cost_Reports_2017.csv")

# pass a sequence of years you want data for
# years <- 2011:2017

# or pass in one year
years <- 2017

# create empty data table to store results
snf_df <- data.table()

# run For Loop through each year
# output of this For Loop is a data frame for specific fields as
# indicated in the function "format_an"
for(x in 1:length(years)){

  # iterate through years
  yrs <- years[x]

  # name the object to be in your enviorment
  CostReport_names <- 'snf_cost_report'

  # paste together the downloadable zip file link
  # ***Always verify that the url's are accurate/pulling the correct files***
  # ***File paths may change from year to year as data gets archived***
  zipfile_url = paste("http://downloads.cms.gov/Files/hcris/SNF10FY", yrs, ".zip", sep = "")

  # paste together the CSV file name to extract from the zip file
  ALPHA.CSV = paste("snf10_", yrs, "_ALPHA.CSV", sep = "")
  NMRC.CSV = paste("snf10_", yrs, "_NMRC.CSV", sep = "")
  RPT.CSV = paste("snf10_", yrs, "_RPT.CSV", sep = "")

  # download and create the Alphanumeric, Numeric, and RPT files as
  # datasets in a list
  assign(CostReport_names,
         download_CMSCostReport(zipfile_url = zipfile_url,
                                ALPHA.CSV = ALPHA.CSV,
                                NMRC.CSV = NMRC.CSV,
                                RPT.CSV = RPT.CSV))

  # bind alphanumeric and numeric datasets ("_an")
  snf_cost_report_an <- rbindlist(c(snf_cost_report[1], snf_cost_report[2]))

  # dataset is now a long dataset
  # specify column types
  snf_cost_report_an <- data_types_func(snf_cost_report_an)

  # extract, transform, and format the dataset
  snf_cost_report_an_formatted <- format_an(snf_cost_report_an)

  # take RPT dataset out of the list and make a dataframe
  snf_cost_report_rpt <- rbindlist(snf_cost_report[3])

  # extract, transform, and format the dataset
  snf_cost_report_rpt_formatted <- format_rpt(snf_cost_report_rpt)

  # left join Alphanumeric-Numeric dataset to RPT file
  # this is how to left join using the data.table method
  snf_cost_report <- snf_cost_report_an_formatted[snf_cost_report_rpt_formatted]

  # append dataset to the empty dataset, if running for one year,
  # or append the next year's dataset to the last
  snf_df <- rbind(snf_df, snf_cost_report)

}

### Let's look at a charctceristic most common among all SNFs, beds. 
summary(snf_df$SNF_BedCount) 

# Notice the maximum number of beds. This may be a typo on the part of 
# the provider that entered this information. It seems unlikely that a
# SNF has more than 12 million beds. Let's check the next highest bed counts.

snf_df %>%
  arrange(desc(SNF_BedCount)) %>%
  select(SNF_BedCount) %>%
  head()

# It seems that the maximum value is indeed a typo. 
# The second and third top values still seem to be pretty high. 
# We'll make the assumption that a SNF has at least one bed, 
# but no more than 10,000 beds. 
snf_df <- snf_df %>%
  filter(SNF_BedCount > 0 & SNF_BedCount < 10000)

### Let's now look at how many cost reports are usually submitted by a provider?
submitted_df <- snf_df %>%
  
  # Utilization Codes refer to the level of Medicare utilization of filed cost report.
  # Codes "N" means "No Medicare Utilization" and "L" means "Low Medicare Utilization"
  # These codes don't have values in the cost reports, as such, we will
  # only focus on cost reports that have "F" ("Full Medicare Utilization").
  filter(UTIL_CD == "F",
         YEAR == 2017) %>%
  group_by(ProviderNumber) %>%
  summarise(CountSubmittedReports = n()) %>%
  arrange(desc(CountSubmittedReports)) %>%
  mutate(CountSubmittedReports = as.factor(CountSubmittedReports)) %>%
  
  # Remaining missing values may indicate a cost report was not submitted 
  # by that provider. Exclude these from the analysis. 
  filter(!is.na(ProviderNumber))

submitted_df %>%
  group_by(CountSubmittedReports) %>%
  summarise(CountProviders = n())

# Most SNFs submit only one cost report each fiscal/calendar year. 
# Some submit two cost reports. In this case, two SNFs submitted 
# three cost reports. Further analysis will need to confirm or investigate 
# these multiple cost report submissions. We know there are unique 
# cost reports submissions, but some submissions may still be under review
# and may thus need to be excluded.  

# One way to get around:
# 1) Multiple cost reporting periods
# 2) Cost reporting periods of differing lengths

# is to scale metrics by the number of days in a cost reporting period. 
# Let's see an example by analyzing Medicare Discharges.  

### Which state has the most Medicare Discharges?
# Medicare is the highest payer to senior living providers, 
# followed by Private Pay, then Medicaid. As such, we will focus on 
# Medicare metrics.

discharges_df <- snf_df %>%
  filter(!(State %in% ("PR")), # Exclude Puerto Rico
         !is.na(State),
         YEAR %in% c(2017)) %>%
  group_by(State) %>%
  summarise(SNF_MedicareDischarges = 
              sum(SNF_MedicareDischarges, na.rm = TRUE),
            DAYS_IN_COST_REPORTING_PERIOD = 
              sum(DAYS_IN_COST_REPORTING_PERIOD, na.rm = TRUE)) %>%
  
  # Scale by the days in cost reporting period to get an adjusted estimate to compare across states.
  mutate(SNF_MedicareDischargesPerDay = 
           SNF_MedicareDischarges/as.integer(DAYS_IN_COST_REPORTING_PERIOD),
         SNF_MedicareDischargesPerDay = 
           round(SNF_MedicareDischargesPerDay, digits = 2)) %>%
  arrange(desc(SNF_MedicareDischargesPerDay))

head(discharges_df)

# Michigan appears to have the most Medicare Discharges to SNF per day.
# That seems a little high. Let's see how Michigan compares
# to the other states.

discharges_df %>%
  ggplot(aes(x = DAYS_IN_COST_REPORTING_PERIOD, y = SNF_MedicareDischarges)) +
  geom_point() +
  geom_abline(slope = 0.5, intercept = 0, color = "red") +
  geom_abline(slope = 1, intercept = 0, color = "blue") +
  annotate("text", x = c(100000, 300000), y = c(200000, 200000), label = c("Blue Line = 1 Per Day", "Red Line = 0.5 Per Day")) +
  labs(x = "Days in Cost Reporting Period", 
       y = "Medicare Discharges to SNF") +
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(labels = scales::comma) +
  ggtitle("Many States Average Less Than 0.5 Medicare Discharges a Day") + # graph title
  theme(plot.title = element_text(hjust = 0.5), # center title
        panel.grid = element_blank(), # remove grid
        panel.background = element_blank() # remove background
  )

# The amount of days in Michigan's cost reporting periods seems reasonable, however, 
# the amount of Medicare Discharges seems very high. Also, we see here that is
# seems rare to average more than 1 Medicare Discharges a Day during a cost
# reporting period. 

### Let's now look at which counties in Michigan have the highest Medicare Discharges.

mi_df <- snf_df %>%
  filter(YEAR == 2017,
         State == "MI") %>%
  group_by(State, County) %>%
  summarise(SNF_MedicareDischarges = 
              sum(SNF_MedicareDischarges, na.rm = TRUE),
            DAYS_IN_COST_REPORTING_PERIOD = 
              sum(DAYS_IN_COST_REPORTING_PERIOD, na.rm = TRUE)) %>%
  # Scale by Bed Days Available to get an adjusted estimate to compare across states.
  mutate(SNF_MedicareDischargesPerDay = 
           SNF_MedicareDischarges/as.integer(DAYS_IN_COST_REPORTING_PERIOD)) %>%
  arrange(desc(SNF_MedicareDischargesPerDay))

# We're going to make a map to help see which Medicare Discharges are making an impact.
# Go ahead and pull in longitude and latitude coordinates for counties from 
# the `ggplot2` package, loaded in the `tidyverse` package. 

counties <- ggplot2::map_data("county")
states_unique <- as.vector(unique(counties$region))
states_abbr <- openintro::state2abbr(states_unique) # convert state names to abbreviations
states_unifier <- data.frame(State = states_unique, 
                             State_Abbr = states_abbr)
head(counties)
head(states_unifier)

# Join the state abbreviations to the counties dataset
counties <- left_join(x = counties, 
                      y = states_unifier, 
                      by = c("region" = "State"))

# The original dataset has the county name capitalized. 
# The counties dataset has the county names in lower case. 
# Let's make "County" in "mi_df" lower case.

mi_df$County <- tolower(mi_df$County)
head(mi_df)
head(counties)

mi_df <- left_join(x = counties, 
                   y = mi_df, 
                   by = c("State_Abbr" = "State", "subregion" = "County"))

# filter to Michigan only
mi_df <- mi_df %>%
  filter(State_Abbr == "MI")

# map the counties
ggplot(mi_df, aes(x = long, y = lat, 
                  fill = SNF_MedicareDischargesPerDay, 
                  group = group)) +
  geom_polygon(color = "gray") +
  scale_fill_gradient(low = "white", 
                      high = "red") +
  coord_map() +
  ggtitle("Many States Average Less Than One Medicare Discharge a Day") + # graph title
  theme(plot.title = element_text(hjust = 0.5), # center title
        panel.grid = element_blank(), # remove grid
        panel.background = element_blank(), # remove background
        axis.title = element_blank(), # remove axes title
        axis.text = element_blank(), # remove axes text
        axis.ticks = element_blank(), # remove axes tick marks
        legend.title = element_blank(), # removed legend title
        legend.position = "bottom"
        #legend.title.align = 0.5 # center legend title
  )

# The gray areas are counties that don't have cost report data.
# More likely than not, these counties don't have SNFs. 

# Livingston County seems to dominate the Medicare Discharge per Day ratio. 
# Let's see what the map looks like if we exclude Livingston County.

mi_df %>%
  filter(!(subregion %in% ("livingston"))) %>%
  ggplot(aes(x = long, y = lat, 
             fill = SNF_MedicareDischargesPerDay, 
             group = group)) +
  geom_polygon(color = "gray") +
  scale_fill_gradient(low = "white", 
                      high = "red") +
  coord_map() +
  ggtitle("Many States Average Less Than One Medicare Discharge a Day") + # graph title
  theme(plot.title = element_text(hjust = 0.5), # center title
        panel.grid = element_blank(), # remove grid
        panel.background = element_blank(), # remove background
        axis.title = element_blank(), # remove axes title
        axis.text = element_blank(), # remove axes text
        axis.ticks = element_blank(), # remove axes tick marks
        legend.title = element_blank(), # removed legend title
        legend.position = "bottom"
        #legend.title.align = 0.5 # center legend title
  )

# The map is much more colorful. The urban areas seem to receive the most
# Medicare Discharges per Day. Grand Traverse County is home to Traverse City.
# Eaton County is on the western portion of Lansing, the capital. 
# Oakland, Macomb, and St. Clair are in the northern metro area of Detroit.  

# Not only did we identify that Michigan seems to be an outlier for Medicare
# Discharges per Day, but it seems that Livingston County is driving 
# that misdirection. So, is Livingston County an outlier?

outlier_df <- snf_df %>%
  group_by(State, County, YEAR) %>%
  summarise(SNF_MedicareDischarges = 
              sum(SNF_MedicareDischarges, na.rm = TRUE),
            DAYS_IN_COST_REPORTING_PERIOD = 
              sum(DAYS_IN_COST_REPORTING_PERIOD, na.rm = TRUE)) %>%
  # Scale by Bed Days Available to get an adjusted estimate to compare across states.
  mutate(SNF_MedicareDischargesPerDay = 
           SNF_MedicareDischarges/as.integer(DAYS_IN_COST_REPORTING_PERIOD)) %>%
  arrange(desc(SNF_MedicareDischargesPerDay))

head(outlier_df, 20)

# Over 200 Medicare Discharges per Day seems astronomical when compared
# to other counties in the U.S. To be sure this observation is an outlier, 
# further investigation would need to be done to see which skilled nursing
# providers reside in Livingston County with high Medicare discharges, 
# look at Medicare discharges for SNFs in surrounding counties, and 
# examine hospital discharges funded by Medicare to SNFs. 
# However, with no state having a Medicare discharge per Day rate greater 
# than 11.6 over the last two or so years, this one stat for 
# Livingston County, MI in 2017 very well may be a user error by the provider.

### What is the average SNF occupancy rate in each state?
occupancy_df <- snf_df %>%
  filter(YEAR == 2017) %>%
  rowwise() %>%

  # compute the total Inpatient Days
  mutate(SNF_TotalInpatientDays = sum(SNF_MedicareInpatientDays, 
                                      SNF_MedicaidInpatientDays,
                                      SNF_OtherInpatientDays, na.rm = TRUE)) %>%
  group_by(State) %>%
  
  # a proxy calculation for Occupancy rate is Bed Days (Total Inpatient Days) 
  # divided by Total Available Bed Days
  summarise(Occupancy = 
              (sum(SNF_TotalInpatientDays, na.rm = TRUE)/
                 sum(SNF_BedDaysAvailable,na.rm = TRUE))*100) %>%
  arrange(desc(Occupancy))

head(occupancy_df)

# North Dakota has the highest SNF Occupancy Rate. Let's map these to 
# get a better idea of which state compare to each other.

states <- ggplot2::map_data("state")
states_unique <- as.vector(unique(states$region))
states_abbr <- openintro::state2abbr(states_unique) # convert state names to abbreviations
states_unifier <- data.frame(State = states_unique, 
                             State_Abbr = states_abbr)
states <- left_join(x = states, 
                    y = states_unifier, 
                    by = c("region" = "State"))

# join occupancy rates to each state
occupancy_df <- left_join(states, occupancy_df, by = c("State_Abbr" = "State"))

# plot map
ggplot(occupancy_df, aes(x = long, y = lat, 
                         fill = Occupancy, 
                         group = group)) +
  geom_polygon(color = "gray") +
  scale_fill_gradient(low = "white", 
                      high = "red") +
  coord_map() +
  ggtitle("Is it Surprising that North Dakota has the Highest Occupancy?") + # graph title
  theme(plot.title = element_text(hjust = 0.5), # center title
        panel.grid = element_blank(), # remove grid
        panel.background = element_blank(), # remove background
        axis.title = element_blank(), # remove axes title
        axis.text = element_blank(), # remove axes text
        axis.ticks = element_blank(), # remove axes tick marks
        legend.title = element_blank(), # removed legend title
        legend.position = "bottom"
        )
