
### Experiment using Vroom ----

library(vroom)
library(here)
library(purrr)
library(fs)

setwd(here( "data"))
files <- fs::dir_ls(glob = "susp*txt")

susp_vroom <- map_df(files, ~vroom(.x, .name_repair = ~ janitor::make_clean_names(., case = "upper_camel")))


setwd(here())

susp_all <- susp_vroom


import_files <- function(dir,globy){
  setwd(here(dir))

  files <- fs::dir_ls(glob = globy)
  
  print(files)
  
  output <- map_df(files, ~vroom(.x, .name_repair = ~ janitor::make_clean_names(., case = "upper_camel")))

  setwd(here())
  
  output
}

test <- import_files("data","exp*txt")



###  Main section ------

# 
# years <- c("1415","1516","1617","1718", "1819")
# 
# susp_smcjuhsd <- read_delim(here("data", "susp1819.txt"),delim = "\t" , col_types ="ccccccccccnnnnnnnnnnnn" ) %>% 
#   clean_names(case = "upper_camel") %>%
#   filter(FALSE)
# 
# 
# for (i in years) {
#   susp_new <- read_delim(here("data",  paste0("susp", i, ".txt")) ,delim = "\t", col_types ="ccccccccccnnnnnnnnnnnn")  %>% 
#     clean_names(case = "upper_camel") %>%
#     filter( DistrictCode == "66068"   )
# 
#   susp_smcjuhsd <- bind_rows(susp_all, susp_new)
# }
# 
# 
# 
# 
# 
# years <- c("1112","1213","1314","1415","1516","1617","1718", "1819")
# 
# susp_all <- read.delim(here("data", "susp1819.txt") ) %>% 
#   clean_names(case = "upper_camel") %>%
#   filter(FALSE)
# 
# 
# for (i in years) {
#   susp_new <- read.delim(here("data",  paste0("susp", i, ".txt") ) ) %>% 
#     clean_names(case = "upper_camel") 
#   
#   susp_all <- bind_rows(susp_all, susp_new)
# }



# All Students

susp_smcjuhsd <- susp_all %>%
  filter(DistrictCode == "66068",
    #      SchoolCode == "0" | is.na(SchoolCode),
          ReportingCategory == "TA",
#    AcademicYear == "2017-18",
    
         CharterYN == "N" | CharterYn %in% c("N", "No ")
         ) %>%
  mutate(burden = as.numeric(TotalSuspensions)*100/as.numeric(CumulativeEnrollment))


# Latino Students

susp_smcjuhsd <- susp_all %>%
  filter(DistrictCode == "66068",
         SchoolCode == "0" | is.na(SchoolCode),
         ReportingCategory == "RH",
         CharterYN == "N" | CharterYn %in% c("N", "No ")
  ) %>%
  mutate(burden = as.numeric(TotalSuspensions)*100/as.numeric(CumulativeEnrollment))


#  Statewide

susp_smcjuhsd <- susp_all %>%
  mutate_at(vars(CumulativeEnrollment:SuspensionCountOtherReasons, SuspensionCountOfStudentsSuspendedDefianceOnly), list(as.numeric)) %>%
  filter(# DistrictCode == "66068",
         SchoolCode == "0" | is.na(SchoolCode),
         ReportingCategory == "TA",
         AcademicYear == "2017-18",
         CharterYN == "N" | CharterYn %in% c("N", "No "),
         CumulativeEnrollment >= 500
  ) %>%
  mutate(burden = as.numeric(TotalSuspensions)*100/as.numeric(CumulativeEnrollment)) %>%
  arrange(desc(burden))





