# CMS-SNF-Cost-Reports

This walk-through downloads, transforms, and analyzes the publically available cost reports for Skilled Nursing Facilities (SNFs), available through the Centers for Medicare and Medicaid Services (CMS). 

The motivation behind this project is to maneuver through and extract insight from a complex dataset. 

The analysis intends to  explore the tip of the iceberg of some common metrics that could impact SNF behavior, and compare these metrics across states and counties. This analysis displays insights at an aggregate level. Finer details can be investigated by other users.

Note, RPT_REC_NUM are unique/distinct to each file. i.e. Records that are in the 2015 file but end in 2016 or 2014 are NOT double counted in the 2014 or 2016 files.

General information about the SNF Cost Reports can be found here:
<https://www.cms.gov/Research-Statistics-Data-and-Systems/Downloadable-Public-Use-Files/Cost-Reports/>

To get the cost reports by year, go to:
<https://www.cms.gov/Research-Statistics-Data-and-Systems/Downloadable-Public-Use-Files/Cost-Reports/Cost-Reports-by-Fiscal-Year.html>  
Note, a new SNF cost report form was implemented in 2010. Fiscal years beginning on or after December 1, 2010 use the new Cost Report form (2540-10). Before December 1, 2010, SNFs filled out the 2540-96 (first implemented in 1996).

SNF Cost Reports 2540-10 Form and 2540-10 Guide explaining the Cost Report:
<https://www.cms.gov/Regulations-and-Guidance/Guidance/Manuals/Downloads/P152_41.zip>

Metadata and data dictionaries can be found here:
https://www.cms.gov/Research-Statistics-Data-and-Systems/Downloadable-Public-Use-Files/Cost-Reports/SkilledNursingFaciilty-2010-form.html  
Specifically, download the zip file and open "HCRIS_DataDictionary.csv" for field descriptions
