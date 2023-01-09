#' ---
#' output: github_document
#' ---


# Socio-Economically Disadvantaged Students    [x]
# Student Homelessness    [x]
# Digital Divide
# Kindergarten Readiness
# Public School Enrollment   [x]
# Children Enrolled in Special Education   [x]
# English Language Arts  [x]
# English Language Learners   [x]
# Math CAASPP Scores  [x]
# Graduation Rates Over Time.   [x]
# Student Suspension Rates Over Time  [x]
# Student Drop Out Rate   [x]
# College Going Rate – are we removing this indictor? No    [x]


library(tidyverse)
library(janitor)
library(here)
library(scales)
library(ggthemes)
library(readxl)
library(vroom)
library(rvest)
library(xml2)
library(MCOE)
library(ggrepel)


year.folder <- "2023"

con <- mcoe_sql_con()

import_files <- function(dir,globy){
  setwd(dir)
  
  files <- fs::dir_ls(glob = globy)
  
  print(files)
  
  output <- map_df(files, ~vroom(.x, .name_repair = ~ janitor::make_clean_names(., case = "upper_camel")))
  
  setwd(here())
  
  output
}


# Note:  Should replace this with codebook function
susp.acron <- tribble(
  ~ReportingCategory, ~StudentGroup,
  "RB", "African American",
  "RI", "American Indian or Alaska Native",
  "RA", "Asian",
  "RF", "Filipino",
  "RH", "Hispanic or Latino",
  "RD", "Race Not Reported",
  "RP", "Pacific Islander",
  "RT", "Two or More Races",
  "RW", "White",
  "GM", "Male",
  "GF", "Female",
  "GX", "Non-Binary", 
  "GZ", "Missing Gender",
  "SE", "English Learners",
  "SD", "Students with Disabilities",
  "SS", "Socioeconomically Disadvantaged",
  "SM", "Migrant",
  "SF", "Foster",
  "SH", "Homeless",
  "TA", "Total")




### Graduation Rate ------

#  https://www.cde.ca.gov/ds/sd/sd/filesacgr.asp
# 4 year cohort rate.  

grad_all <- tbl(con,"GRAD_FOUR") %>%
  filter( (aggregate_level == "T"  |county_code == 27),
          is.na(district_code),
          reporting_category == "TA",
          charter_school =="All",
          dass == "All") %>%
  collect() %>% 
  mutate(Geo = if_else(aggregate_level == "T", "California" , county_name ))


  

grad_sub <-tbl(con,"GRAD_FOUR") %>%
  filter( (county_code == 27),
          is.na(district_code),
          academic_year == max(academic_year),
          #        ReportingCategory == "TA",
          charter_school =="All",
          dass == "All",
          !is.na(regular_hs_diploma_graduates_rate)) %>%
  collect() %>%
  left_join_codebook("SUSP", "reporting_category" ) %>%
  rename("StudentGroup" = "definition") %>%
  mutate(StudentGroupCategory = str_extract(reporting_category ,"[:alpha:]{1,1}"  )) %>%
  mutate(StudentGroupCategory = case_when(StudentGroupCategory == "G" ~ "Gender",
                                          StudentGroupCategory == "R" ~ "Race/Ethnicity",
                                          StudentGroupCategory == "S" ~ "Student Group")) %>%
  filter(reporting_category != "TA")


# grad_all2 <- grad_all %>%
#   mutate(Regular_HS_Diploma_Graduates_Rate = ifelse(AcademicYear == "2018-19", NA , Regular_HS_Diploma_Graduates_Rate))

#**********
ggplot(grad_all, aes(x = academic_year, y = regular_hs_diploma_graduates_rate/100 , group = Geo, color = Geo , linetype = Geo, label=percent(regular_hs_diploma_graduates_rate/100, digits = 0) )) +
  geom_line(size = 1.5) +
  geom_label_repel(data = grad_all %>% filter(academic_year == max(academic_year)) ,
                   size = 3,
                #  color = "black",
                   show.legend = FALSE) +
  theme_hc() +
  #        coord_flip() +
  scale_color_few() +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(.7,.9)) +
  scale_linetype_manual(values =  c("dashed", "solid")) +
  guides(linetype = "none") +
  labs(x = "",
       y = "",
       color ="",
       title = ("Graduation Rates Over Time"),
       caption = "Source: Adjusted Cohort Outcome Data \n https://www.cde.ca.gov/ds/sd/sd/filesacgr.asp") # +
# Added for pandemic drawing
 #     geom_vline(xintercept = "2019-20", linetype = "longdash" )



ggsave(here("figs",year.folder,"graduationRate.png"), width = 6, height = 5)



ggplot(grad_sub, aes( y = regular_hs_diploma_graduates_rate/100, x =fct_reorder(StudentGroup, regular_hs_diploma_graduates_rate) ,  label = percent(regular_hs_diploma_graduates_rate/100, accuracy = .1))) +
  geom_segment( aes(x=fct_reorder(StudentGroup, regular_hs_diploma_graduates_rate), xend=fct_reorder(StudentGroup, regular_hs_diploma_graduates_rate), y=0, yend=regular_hs_diploma_graduates_rate/100),
                color="orange",
                size =2 ) +
  geom_point( color="orange", size=5, alpha=0.6) +
  coord_flip() +
  geom_text(size = 3, color = "black") +
  facet_grid(facets = vars(StudentGroupCategory), scales = "free" , space = "free") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_hc() +
  mcoe_theme +
  labs(x = "",
       y = "",
       color ="",
       title = ("Graduation Rates by Student Group"),
       caption = "Source: Adjusted Cohort Outcome Data \n https://www.cde.ca.gov/ds/sd/sd/filesacgr.asp") 

ggsave(here("figs",year.folder,"grad-subgroup.png"), width = 6, height = 7)

### College Going Rate ------

### https://www.cde.ca.gov/ds/ad/filescgr12.asp

#************

cgr_all <- tbl(con, "CGR")  %>%
  filter( AggregateLevel %in% c("T", "C"),
          CountyCode %in% c("27","00"), 
          ReportingCategory == "TA",
          CharterSchool =="All",
          AlternativeSchoolAccountabilityStatus == "All",
          CompleterType =="TA") %>%
  collect() %>%
  mutate(Geo = if_else(AggregateLevel == "T", "California" , CountyName)) %>%
  mutate(CGR12 = College_Going_Rate_Total_12_Months,
         CGR12 = as.numeric(CGR12)) 


ggplot(cgr_all, aes(x = AcademicYear, y = CGR12/100, group = Geo, color = Geo , linetype = Geo, label= percent(CGR12/100, accuracy = .1)) ) +
  geom_line(size = 1.5) +
  geom_label(data = cgr_all %>% filter(AcademicYear == max(AcademicYear)) , size = 3, color = "black") +
  theme_hc() +
  #        coord_flip() +
  scale_color_few() +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(.5,.7)) +
  scale_linetype_manual(values =  c("dashed", "solid")) +
  guides(linetype = "none") +
  labs(x = "",
       y = "",
       color ="",
       title = ("College-Going Rate Over Time "),
       caption = "Source: College-Going Rate for HS Completers (12-month) \nhttps://www.cde.ca.gov/ds/ad/filescgr12.asp") 

ggsave(here("figs",year.folder,"CollegeGoingRate.png"), width = 6.5, height = 4)


### Dropout Rate ------

##  https://www.cde.ca.gov/ds/sd/sd/filesacgr.asp

#**********

ggplot(grad_all, aes(x = academic_year, y = dropout_rate/100, group = Geo, color = Geo , linetype = Geo, label=percent(dropout_rate/100, digits = 0) )) +
  geom_line(size = 1.5) +
  geom_label_repel(data = grad_all %>% filter(academic_year == max(academic_year)) ,
                   size = 3,
                   #  color = "black",
                   show.legend = FALSE) +
  theme_hc() +  
  #        coord_flip() +
  scale_color_few() +
  mcoe_theme +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0,.15)) +
  scale_linetype_manual(values =  c("dashed", "solid")) +
  guides(linetype = "none") +
  labs(x = "",
       y = "",
       color ="",
       title = ("Dropout Rates Over Time"),
       caption = "Source: Adjusted Cohort Outcome Data \n https://www.cde.ca.gov/ds/sd/sd/filesacgr.asp") # +
  # Added for pandemic drawing
#  geom_vline(xintercept = "2019-20", linetype = "longdash" )

ggsave(here("figs",year.folder,"dropout.png"), width = 6, height = 4)


ggplot(grad_sub, aes( y = dropout_rate/100, x =fct_reorder(StudentGroup, dropout_rate) ,  label = percent(dropout_rate/100, accuracy = .1))) +
  geom_segment( aes(x=fct_reorder(StudentGroup, dropout_rate), xend=fct_reorder(StudentGroup, dropout_rate), y=0, yend=dropout_rate/100),
                color="orange",
                size =2 ) +
  geom_point( color="orange", size=5, alpha=0.6) +
  coord_flip() +
  geom_text(size = 3, color = "black") +
  facet_grid(facets = vars(StudentGroupCategory), scales = "free" , space = "free") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_hc() +
  mcoe_theme +
  labs(x = "",
       y = "",
       color ="",
       title = ("Dropout Rates by Student Group"),
       caption = "Source: Adjusted Cohort Outcome Data \n https://www.cde.ca.gov/ds/sd/sd/filesacgr.asp") 

ggsave(here("figs",year.folder,"drop-subgroup.png"), width = 6, height = 7)


### Enrollment -------
# Unduplicated Pupil Count (UPC) of free or reduced price meal (FRPM) eligibility, English learner (EL), and foster youth data
# from the California Longitudinal Pupil Achievement Data System (CALPADS). These counts are the starting point for determining 
# the Unduplicated Pupil Percentage (UPP) used in the Local Control Funding Formula (LCFF) supplemental and concentration grant calculations.

## https://www.cde.ca.gov/ds/sd/sd/filescupc.asp




# cupc.mry <- tbl(con, "UPC") %>% 
#   filter(County_Code == 27) %>%
#   collect() %>%
#   group_by(academic_year)
# 
# 
# # Total Enrollment
# 
# enrollment <- function(enrolltype, lowlimit, highlimit,tit ){
#   cupc.mry %>%
#   summarise(countyenrollment = sum({{enrolltype}})) %>%
#   `colnames<-`(c("year","value")) %>%
#   ungroup() %>%
#   mutate(Geo = "Monterey County") %>%
#   ggplot( aes(x = year, y = value,  label=comma( value) , group = Geo)) +
#   geom_line(size = 1.5) +
#   geom_label( size = 3, color = "black") +
#   theme_hc() +
#   scale_color_few() +
#   scale_y_continuous(labels = comma_format(), limits = c(lowlimit,highlimit)) +
#   labs(x = "",
#        y = "",
#        color ="",
#        title = (tit),
#        caption = "Source: Unduplicated Pupil Count \n https://www.cde.ca.gov/ds/sd/sd/filescupc.asp")
# }
# 
# 
# 
# 
# #******
# enrollment(total_enrollment, 60000, 80000, "Total Enrollment in Monterey County")
# ggsave(here("figs",year.folder,"total enrollment.png"), width = 6, height = 4)
# 
# #******
# enrollment(homeless, 0, 10000, "Homeless Enrollment in Monterey County")
# ggsave(here("figs",year.folder,"homeless enrollment.png"), width = 6, height = 4)
# 
# #******
# enrollment(unduplicated_frpm_eligible_count, 50000, 60000, "Free/Reduced Price Meals Enrollment in Monterey County")
# ggsave(here("figs",year.folder,"frpm enrollment.png"), width = 6, height = 4)
# 
# 
# enrollment(english_learner_el, 25000, 35000, "English Learner Enrollment in Monterey County")
# ggsave(here("figs",year.folder,"EL enrollment.png"), width = 6, height = 4)
# 
# 
# 
# #  Special Education Enrollment
# 
# #********
# sped.count <- function(yr) {
#   
#   url <- paste0("https://data1.cde.ca.gov/dataquest/SpecEd/SEEnrEthDis2.asp?cChoice=SEEthDis2&cYear=",yr,"&TheCounty=27,MONTEREY&clevel=County&ReptCycle=December")
#   
#   
#   table <- xml2::read_html(url) %>%
#     html_nodes( xpath = "/html/body/center[3]/table") %>%
#     html_table(fill = TRUE) %>%
#     flatten() %>%
#     as_tibble(.name_repair = "unique")
#   
#   table[22,15] %>% 
#     simplify() 
# }  # Goes to URL, uses year to pull out total SPED kids that year 
# 
# 
# sped.count("2018-19")
# 
# sped <- tibble( year = c("2011-12", "2012-13" , "2013-14" ,"2014-15", "2015-16" , "2016-17", "2017-18", "2018-19", "2019-20", "2020-21", "2021-22"))
# 
# sped <- sped %>% mutate(value = map(year, sped.count) %>%   # use function for all the years 
#                           simplify() %>%    
#                           gsub(",","", .  ) %>%
#                           as.numeric()  ) %>%    # to get a number out rathe than a list or character with a comma
#   mutate(Geo = "Monterey County")  # create a group for graphing
# 
# 
# ggplot(sped, aes(x = year, y = value, group = Geo, label=comma( value) )) +
#   geom_line(size = 1.5) +
#   geom_label( size = 3, color = "black") +
#   theme_hc() +
#   scale_color_few() +
#   scale_y_continuous(labels = comma_format(), limits = c(5000,10000)) +
#   labs(x = "",
#        y = "",
#        color ="",
#        title = ("Special Education Enrollment in Monterey County"),
#        caption = "Source: DataQuest \n https://data1.cde.ca.gov/dataquest/SpecEd/SEEnrEthDis2.asp?cChoice=SEEthDis2&\ncYear=2018-19&TheCounty=27,MONTEREY&clevel=County&ReptCycle=December") 
# 
# ggsave(here("figs",year.folder,"SPED enrollment.png"), width = 6, height = 4)



#  Using Cumulative Enrollment Only



cenrol <- tbl(con, "CENROLLMENT") %>% 
  filter(county_code == "27",
         aggregate_level == "C") %>%
  collect() %>%
  group_by(academic_year) %>% 
  mutate(cumulative_enrollment = as.numeric(cumulative_enrollment))

cenrollment <- function(enrolltype, lowlimit, highlimit,tit ){
  cenrol %>% 
    filter(charter == "All",
           reporting_category == {{enrolltype}}) %>%
    mutate(Geo = "Monterey County") %>%
    ggplot( aes(x = academic_year, y =  cumulative_enrollment, group = county_name , label=comma( cumulative_enrollment) 
    )) +
    geom_line(size = 1.5) +
    geom_label( size = 3, color = "black") +
    theme_hc() +
    scale_color_few() +
    scale_y_continuous(labels = comma_format(), limits = c(lowlimit,highlimit)) +
    labs(x = "",
         y = "",
         color ="",
         title = (tit),
         caption = "Source: Cumulative Enrollment Data \n https://www.cde.ca.gov/ds/ad/filesenrcum.asp")
}

#******
cenrollment("TA", 75000, 85000, "Total Enrollment in Monterey County")
ggsave(here("figs",year.folder,"total enrollment c.png"), width = 6, height = 4)

#******
cenrollment("SH", 5000, 12000, "Homeless Enrollment in Monterey County")
ggsave(here("figs",year.folder,"homeless enrollment c.png"), width = 6, height = 4)

#******
cenrollment("SS", 55000, 65000, "Socioeconomically Disadvantaged Enrollment \nin Monterey County")
ggsave(here("figs",year.folder,"SocioEcon enrollment c.png"), width = 6, height = 4)

#******
cenrollment("SE", 25000, 35000, "English Learner Enrollment in Monterey County")
ggsave(here("figs",year.folder,"EL enrollment c.png"), width = 6, height = 4)


#******
cenrollment("SD", 7000, 11000, "Students with Disabilities Enrollment in Monterey County")
ggsave(here("figs",year.folder,"SWD enrollment c.png"), width = 6, height = 4)



### Suspension ------

# https://www.cde.ca.gov/ds/sd/sd/filessd.asp


#***********


susp_vroom <- tbl(con,"SUSP") %>% 
  filter(is.na(district_code),
          charter_yn == "All" | is.na(charter_yn) | charter_yn == "") %>%
  collect() %>%
  mutate(Geo = if_else(aggregate_level == "T", "California" ,county_name ))# %>%
#  mutate_at(vars(cumulative_enrollment:suspension_count_other_reasons), funs(as.numeric) )

susp_all <- susp_vroom %>%
  filter( (aggregate_level == "T"  |county_code == 27),
          reporting_category == "TA") 


susp_sub <- susp_vroom %>%  
  filter( 
    (county_code == 27),
    academic_year == max(academic_year)
 ) %>%
  rename(ReportingCategory = reporting_category) %>%
  left_join(susp.acron) %>%
  mutate(StudentGroupCategory = str_extract(ReportingCategory ,"[:alpha:]{1,1}"  )) %>%
  mutate(StudentGroupCategory = case_when(StudentGroupCategory == "G" ~ "Gender",
                                          StudentGroupCategory == "R" ~ "Race/Ethnicity",
                                          StudentGroupCategory == "S" ~ "Student Group")) %>%
  filter(ReportingCategory != "TA")
  


ggplot(susp_all, aes(x = academic_year, y = suspension_rate_total, group = Geo, color = Geo , linetype = Geo, label = percent(suspension_rate_total/100, accuracy = .01, digits = 1))) +
  geom_line(size = 1.5) +
  geom_label_repel(data = susp_all %>% filter(academic_year == max(academic_year))  , size = 3,# color = "black"
               ) +
  theme_hc() +
  #        coord_flip() +
  scale_color_few() +
  scale_linetype_manual(values =  c("dashed", "solid")) +
  guides(linetype = "none") +
  labs(x = "",
       y = "",
       color ="",
       title = ("K-12 Suspension Rates Over Time"), # fn("K-12 Suspension Rates Over Time"),
       caption = "Source: Suspension Data Files \n https://www.cde.ca.gov/ds/sd/sd/filessd.asp") # +
  # Added for pandemic drawing
#  geom_vline(xintercept = "2019-20", linetype = "longdash" )

ggsave(here("figs",year.folder,"suspension.png"), width = 7, height = 4)



ggplot(susp_sub, aes( y = suspension_rate_total/100, x =fct_reorder(StudentGroup, suspension_rate_total) ,  label = percent(suspension_rate_total/100, accuracy = .1))) +
  geom_segment( aes(x=fct_reorder(StudentGroup, suspension_rate_total), xend=fct_reorder(StudentGroup, suspension_rate_total), y=0, yend=suspension_rate_total/100),
                color="orange",
                size =2 ) +
  geom_point( color="orange", size=5, alpha=0.6) +
  facet_grid(facets = vars(StudentGroupCategory), scales = "free" ) +
  coord_flip() +
  geom_text(size = 3, color = "black") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_hc() +
  mcoe_theme +
  labs(x = "",
       y = "",
       color ="",
       title = ("K-12 Suspension Rates By Subgroup"), # fn("K-12 Suspension Rates Over Time"),
       caption = "Source: Suspension Data Files \n https://www.cde.ca.gov/ds/sd/sd/filessd.asp")

ggsave(here("figs",year.folder,"suspension-subgroup.png"), width = 6, height = 7)


### Expulsion -------

# https://www.cde.ca.gov/ds/sd/sd/filesed.asp

#************



exp_vroom <- tbl(con,"EXP") %>% 
  filter(is.na(district_code),
         charter_yn == "All" | is.na(charter_yn) | charter_yn == "") %>%
  collect() %>%
  mutate(Geo = if_else(aggregate_level == "T", "California" ,county_name )) %>%
#  mutate_at(vars(cumulative_enrollment:expulsion_count_other_reasons), funs(as.numeric) ) %>%
  mutate(rate = (1000*unduplicated_count_of_students_expelled_total/cumulative_enrollment))

exp_all <- exp_vroom %>%
  filter( (aggregate_level == "T"  |county_code == 27),
        reporting_category == "TA") 


exp_sub <- exp_vroom %>%
  filter( 
    (county_code == 27),
    academic_year == max(academic_year)
  ) %>%
  rename(ReportingCategory = reporting_category) %>%
  left_join(susp.acron) %>%
  mutate(StudentGroupCategory = str_extract(ReportingCategory ,"[:alpha:]{1,1}"  )) %>%
  mutate(StudentGroupCategory = case_when(StudentGroupCategory == "G" ~ "Gender",
                                          StudentGroupCategory == "R" ~ "Race/Ethnicity",
                                          StudentGroupCategory == "S" ~ "Student Group")) %>%
  filter(ReportingCategory != "TA")



ggplot(exp_all, aes(x = academic_year, y = rate, group = Geo, color = Geo , linetype = Geo, label = round2( rate, 2), accuracy = .01 )) +
  geom_line(size = 1.5) +
  geom_label_repel(data = exp_all %>% filter(academic_year == max(academic_year)) , size = 3) +
  theme_hc() +
  scale_color_few() +
 # scale_y_continuous(labels = scales::percent_format(accuracy = .01), limits = c(0,0.002)) +
  scale_linetype_manual(values =  c("dashed", "solid")) +
  guides(linetype = "none") +
  labs(x = "",
       y = "",
       color ="",
       title =  ("K-12 Expulsion Rates per 1,000 Over Time"), #fn("K-12 Expulsion Rates Over Time"),
       caption = "Source: Expulsion Data Files \n https://www.cde.ca.gov/ds/sd/sd/filesed.asp") #+
  # Added for pandemic drawing
#  geom_vline(xintercept = "2019-20", linetype = "longdash" )


ggsave(here("figs",year.folder,"expulsion.png"), width = 7, height = 4)




ggplot(exp_sub, aes( y = rate, x =fct_reorder(StudentGroup, rate) ,  label = round2( rate, 2) )) +
  geom_segment( aes(x=fct_reorder(StudentGroup, rate), xend=fct_reorder(StudentGroup, rate), y=0, yend=rate),
                color="orange",
                size =2 ) +
  geom_point( color="orange", size=5, alpha=0.6) +
  facet_grid(facets = vars(StudentGroupCategory), scales = "free" ) +
  coord_flip() +
  geom_text(size = 3, color = "black") +
  theme_hc() +
  mcoe_theme +
  labs(x = "",
       y = "",
       color ="",
       title = ("K-12 Expulsion Rates per 1,000 By Subgroup"), # fn("K-12 Suspension Rates Over Time"),
       caption = "Source: Expulsion Data Files \n https://www.cde.ca.gov/ds/sd/sd/filesed.asp")

ggsave(here("figs",year.folder,"expulsion-subgroup.png"), width = 6, height = 7)


### Physical Fitness -----

# https://www.cde.ca.gov/ta/tg/pf/pftresearch.asp

#*******
pft.years <- c("2018_19", "2014_15","2015_16","2016_17","2017_18" )

compile.pft <- function(yr) {
  
  read_delim(here::here("data", "pft" ,paste0( yr,"_ResearchFile.txt")), delim = ",", col_types = "nnnnnnncnnnnnnnnnnnnnnnn")  %>%
    filter(CO %in% c(0,27)) %>%
    filter(Level_Number %in% c(3,4)) %>% # district and county level
    filter(Report_Number == 0)  %>%  # All Students
    filter(Table_Number == 2) %>%
    filter(Line_Number %in% c(3,8)) %>% # 4 of 6 tests
    mutate(year = yr)
}

all.pft <- data_frame()

for(y in pft.years){

    print(y)
    
    try({
      temp <- compile.pft( y)
      
      all.pft <- bind_rows(all.pft, temp)
    })
}



pft.mry <- all.pft %>%
  select(year, CO, Line_Text, Line_Number, NoStud5, Perc5b, NoStud7, Perc7b, NoStud9, Perc9b)    %>%
  pivot_wider(id_cols = c(year, CO), names_from = Line_Number, values_from = c(NoStud5:Perc9b)) %>%
  select(-NoStud5_3, -NoStud7_3,-NoStud9_3, -Perc5b_8, -Perc7b_8, -Perc9b_8) %>%
  pivot_longer(cols = c(NoStud5_8:Perc9b_3)) %>% 
  mutate(GeoGrade = as.factor(paste0(
    if_else(CO == 0, "California", "Monterey"),
    " ",
    case_when(name == "Perc5b_3" ~ "5",
              name == "Perc7b_3" ~ "7",
              name == "Perc9b_3" ~ "9",
              name == "NoStud5_8" ~ "5",
              name == "NoStud7_8" ~ "7",
              name == "NoStud9_8" ~ "9"))))


pft.pal <- c("#5DA5DA", "#2A72A7", "#003F74", "#FAA43A",  "#C77107","#943E00")

pft.mry %>% 
  filter(str_detect(name, "Perc")) %>%
ggplot(aes(x= year, y = value/100, group = GeoGrade, color = GeoGrade, linetype =GeoGrade, label=percent(value/100, digits = 0))) +
  geom_line(size = 1.5) +
  geom_label_repel(data = pft.mry %>% 
              filter(str_detect(name, "Perc")) %>%
              filter(year == max(year)) , size = 3,
      #        color = "black"
      show.legend = FALSE
              ) +
  theme_hc() +
  scale_color_manual(values = pft.pal ) +
#    scale_color_few() +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(.50, .80)) +
  scale_linetype_manual(values =  c("dashed","dashed","dashed", "solid","solid","solid")) +
  guides(linetype = "none") +
  guides(color = guide_legend(nrow=3,byrow=FALSE)) +
  labs(x = "",
       y = "",
       color ="",
       title = ("Physical Fitness Over Time by Grade"),
       subtitle = "Percentage Meeting 4 or more of 6 Fitness Tests",
       caption = "Source: Physical Fitness Test Data \n https://www.cde.ca.gov/ta/tg/pf/pftresearch.asp")

ggsave(here("figs",year.folder,"physical.png"), width = 6, height = 5)


# ### Math and ELA scores ----

# https://caaspp-elpac.cde.ca.gov/caaspp/ResearchFileList?ps=true&lstTestYear=2019&lstTestType=B&lstGroup=1&lstGrade=13&lstSchoolType=A&lstCounty=00&lstDistrict=00000&lstSchool=0000000#dl

sbac.filtered <- tbl(con, "CAASPP") %>%
  filter(
    County_Code %in% c("00", "27"),
    District_Code == "00000",
    Grade == 13
    ) %>%
#  head(100) %>%
  collect() %>%
  mutate(Geo = if_else(County_Code == "00", "California", "Monterey County"),
         Percentage_Standard_Met_and_Above = as.numeric(Percentage_Standard_Met_and_Above),
         Subgroup_ID = as.character(Subgroup_ID)
         ) %>%
  left_join_codebook("CAASPP", "Subgroup_ID") %>%
  rename("StudentGroup" = "definition") %>%
  mutate(StudentGroupCategory = case_when( Subgroup_ID %in% c(128,99) ~ "Disability \nStatus",
                                           Subgroup_ID %in% c(31,111) ~ "Economic \nStatus",
                                           Subgroup_ID %in% c(6,7,8,120,142,160,243,180,170,190) ~ "English \nLanguage \nFluency",
                                           Subgroup_ID %in% c(75,76,74,77,78,79,80,144) ~ "Race and \nEthnicity",
                                           Subgroup_ID %in% c(201,202,200,203,204,205,206,207) ~ "Ethnicity for \nEconomically \nDisadvantaged",
                                           Subgroup_ID %in% c(221,222,220,223,224,225,226,227) ~ "Ethnicity for \nNot \nEconomically \nDisadvantaged",
                                           Subgroup_ID %in% c(3,4) ~ "Gender",
                                           Subgroup_ID %in% c(28,29) ~ "Migrant",
                                           Subgroup_ID %in% c(90,91,92,93,94,121) ~ "Parent \nEducation",
                                           Subgroup_ID %in% c(50,51) ~ "Military \nStatus",
                                           Subgroup_ID %in% c(52,53) ~ "Homeless \nStatus",
                                           Subgroup_ID %in% c(240,241) ~ "Foster \nStatus",
                                           
                                           Subgroup_ID %in% c(1) ~ "All \nStudents"
                                             ))
  

sbac.graphs <- function(test.id) {
  
  
  test.name <- if_else(test.id == 1, "ELA", "Math")

sbac.filtered %>%
  filter(Test_Id == test.id,
         Subgroup_ID == 1) %>%
ggplot( aes(x = Test_Year,
            y = Percentage_Standard_Met_and_Above/100,
            group = Geo, 
            color = Geo , 
            linetype = Geo, 
            label=percent(Percentage_Standard_Met_and_Above/100, digits = 0) )
        ) +
  geom_line(size = 1.5) +
  geom_point(size = 3)+
  geom_label(data = sbac.filtered %>%
              filter(Test_Id == test.id,
                     Subgroup_ID == 1) %>%
              filter(Test_Year == max(Test_Year)) , size = 3, color = "black") +
  theme_hc() +
  scale_color_few() +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0,.6)) +
  scale_linetype_manual(values =  c("dashed", "solid")) +
    guides(linetype = "none") +
  labs(x = "",
       y = "",
       color ="",
       title = (paste0(test.name, " Percentage Meeting and Exceeding Rates Over Time")),
       caption = "Source: CAASPP Research Files \n https://caaspp-elpac.cde.ca.gov/caaspp/ResearchFileList")

ggsave(here("figs",year.folder,paste0(test.name,".png")), width = 6, height = 4)


sbac.filtered %>%
  filter(Test_Id == test.id,
         Test_Year == max(Test_Year),
         County_Code == "27",
#         !str_detect(StudentGroup, "-"),
         !str_detect(StudentGroup, "TBD"),
         !str_detect(StudentGroupCategory, "Ethnicity for")
        ) %>% # `Demographic Name`
  ggplot( aes( y = Percentage_Standard_Met_and_Above/100, x =fct_reorder(StudentGroup, Percentage_Standard_Met_and_Above) ,  label = percent(Percentage_Standard_Met_and_Above/100, accuracy = .1))) +
  geom_segment( aes(x=fct_reorder(StudentGroup, Percentage_Standard_Met_and_Above/100), xend=fct_reorder(StudentGroup, Percentage_Standard_Met_and_Above/100), y=0, yend=Percentage_Standard_Met_and_Above/100),
                color="orange",
                size =2 ) +
  geom_point( color="orange", size=5, alpha=0.6) +
  coord_flip() +
  geom_text(size = 3, color = "black") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  facet_grid(facets = vars(StudentGroupCategory), scales = "free" ) +
  theme_hc() +
  mcoe_theme +
  labs(x = "",
       y = "",
       color ="",
       title = (paste0(test.name, " Percentage Meeting and Exceeding Rates by Student Group")),
       caption = "Source: CAASPP Research Files \n https://caaspp-elpac.cde.ca.gov/caaspp/ResearchFileList")

ggsave(here("figs",year.folder,paste0(test.name,"-sub.png")), width = 8, height = 11)

}


sbac.graphs(1)

sbac.graphs(2)

# ### EL Reclassification ----

# https://www.cde.ca.gov/ds/sd/sd/filesreclass.asp


el_vroom <- tbl(con,"RECLASS") %>% 
  filter(County == "Monterey"  ) %>%
  collect() 

el_dist <- el_vroom %>%
#  filter( str_detect(CDS,"^27"  )) %>%
  mutate(year = str_extract_all(YEAR, "\\d{1,2}") %>% simplify() %>% as.numeric()) %>%
  mutate(year = paste0("20",year-1,"-20",year)) %>%
#  filter(year >= "2014-2015") %>%
  group_by(year) %>%
  transmute(`Reclassified Students` = sum(Reclass),
            `EL Enrollment` = sum(EL)) %>%
  distinct() %>%
  pivot_longer(cols = c(`Reclassified Students`, `EL Enrollment`))


el_dist  %>%
  ggplot( aes(x = year, y = value,  label=comma( value, accuracy = 1) , color = name ,group = name)) +
  geom_line(size = 1.5) +
  geom_label( size = 3 , color = "black") +
  theme_hc() +
  scale_color_pander() +
  scale_y_continuous(labels = comma_format(), limits = c(0,35000)) +
  labs(x = "",
       y = "",
       color ="",
              title = ("EL Enrollment and Number of Reclassified Students by Year"),
              caption = "Source: EL Reclassification Data \n https://www.cde.ca.gov/ds/sd/sd/filesreclass.asp")


ggsave(here("figs",year.folder,"reclass.png"), width = 8, height = 5)

#  
# # 
# # el_enrol <- cupc.mry %>%
# #   summarise(countyenrollment = sum(EnglishLearnerEl)) %>%
# #   `colnames<-`(c("year","value")) %>%
# #   ungroup() %>%
# #   mutate(Geo = "EL Enrollment") 
# # 
# # el_enrol %>%
# #   bind_rows(el_dist) %>%
# #   ggplot( aes(x = year, y = value,  label=comma( value) , group = Geo, color = Geo)) +
# #   geom_line(size = 1.5) +
# #   geom_label( size = 3, color = "black") +
# #   theme_hc() +
# #   scale_color_pander() +
# #   scale_y_continuous(labels = comma_format(), limits = c(0,40000)) +
# #   labs(x = "",
# #        y = "",
# #        color ="",
# #        title = ("EL Enrollment and Number of Reclassified Students by Year"),
# #        caption = "Sources: Unduplicated Pupil Count \n https://www.cde.ca.gov/ds/sd/sd/filescupc.asp \nEL Reclassification Data \n https://www.cde.ca.gov/ds/sd/sd/filesreclass.asp") 
# # 
# 
# 
### End ----
devtools::session_info()

