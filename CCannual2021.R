#' ---
#' output: github_document
#' ---


# Free and Reduced Lunch Meal Program - Have
# Student Homelessness - Have
# Public School Enrollment - Have 
# Children Enrolled in Special Education - unknown date
# Physical Fitness - will not get (testing waived)
# Graduation Rates Over Time -  historically mid-December
# Student Suspension Rates Over Time -  historically mid-December
# Student Expulsion Rates Over Time  - historically mid-December
# Student Drop Out Rate -  have
# College Going Rate - unknown (released in July 2019 last)



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


con <- mcoe_sql_con()

import_files <- function(dir,globy){
  setwd(dir)
  
  files <- fs::dir_ls(glob = globy)
  
  print(files)
  
  output <- map_df(files, ~vroom(.x, .name_repair = ~ janitor::make_clean_names(., case = "upper_camel")))
  
  setwd(here())
  
  output
}



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
  filter( (AggregateLevel == "T"  |CountyCode == 27),
          is.na(DistrictCode),
          ReportingCategory == "TA",
          CharterSchool =="All",
          Dass == "All") %>%
  collect() %>% 
  mutate(Geo = if_else(AggregateLevel == "T", "California" ,CountyName ))


  

grad_sub <-tbl(con,"GRAD_FOUR") %>%
  filter( (CountyCode == 27),
          is.na(DistrictCode),
          AcademicYear == max(AcademicYear),
          #        ReportingCategory == "TA",
          CharterSchool =="All",
          Dass == "All") %>%
  collect() %>%
  left_join(susp.acron)  %>%
  mutate(StudentGroupCategory = str_extract(ReportingCategory ,"[:alpha:]{1,1}"  )) %>%
  mutate(StudentGroupCategory = case_when(StudentGroupCategory == "G" ~ "Gender",
                                          StudentGroupCategory == "R" ~ "Race/Ethnicity",
                                          StudentGroupCategory == "S" ~ "Student Group")) %>%
  filter(ReportingCategory != "TA")




#**********
ggplot(grad_all, aes(x = AcademicYear, y = Regular_HS_Diploma_Graduates_Rate/100 , group = Geo, color = Geo , linetype = Geo, label=percent(Regular_HS_Diploma_Graduates_Rate/100, digits = 0) )) +
  geom_line(size = 1.5) +
  geom_label_repel(data = grad_all %>% filter(AcademicYear == max(AcademicYear)) ,
                   size = 3,
                #  color = "black",
                   show.legend = FALSE) +
  theme_hc() +
  #        coord_flip() +
  scale_color_few() +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(.7,.9)) +
  scale_linetype_manual(values =  c("dashed", "solid")) +
  guides(linetype = FALSE) +
  labs(x = "",
       y = "",
       color ="",
       title = ("Graduation Rates Over Time"),
       caption = "Source: Adjusted Cohort Outcome Data \n https://www.cde.ca.gov/ds/sd/sd/filesacgr.asp") 

ggsave(here("figs","2021","graduationRate.png"), width = 6, height = 5)



ggplot(grad_sub, aes( y = Regular_HS_Diploma_Graduates_Rate/100, x =fct_reorder(StudentGroup, Regular_HS_Diploma_Graduates_Rate) ,  label = percent(Regular_HS_Diploma_Graduates_Rate/100, accuracy = .1))) +
  geom_segment( aes(x=fct_reorder(StudentGroup, Regular_HS_Diploma_Graduates_Rate), xend=fct_reorder(StudentGroup, Regular_HS_Diploma_Graduates_Rate), y=0, yend=Regular_HS_Diploma_Graduates_Rate/100),
                color="orange",
                size =2 ) +
  geom_point( color="orange", size=5, alpha=0.6) +
  coord_flip() +
  geom_text(size = 3, color = "black") +
  facet_grid(facets = vars(StudentGroupCategory), scales = "free" ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_hc() +
  mcoe_theme +
  labs(x = "",
       y = "",
       color ="",
       title = ("Graduation Rates by Student Group"),
       caption = "Source: Adjusted Cohort Outcome Data \n https://www.cde.ca.gov/ds/sd/sd/filesacgr.asp") 

ggsave(here("figs","2021","grad-subgroup.png"), width = 6, height = 7)

### College Going Rate ------

### https://www.cde.ca.gov/ds/sd/sd/filescgr12.asp

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
  guides(linetype = FALSE) +
  labs(x = "",
       y = "",
       color ="",
       title = ("College-Going Rate Over Time "),
       caption = "Source: College-Going Rate for HS Completers (12-month)  https://www.cde.ca.gov/ds/sd/sd/filescgr12.asp") 

ggsave(here("figs","2021","CollegeGoingRate.png"), width = 6.5, height = 4)


### Dropout Rate ------

##  https://www.cde.ca.gov/ds/sd/sd/filesacgr.asp

#**********

ggplot(grad_all, aes(x = AcademicYear, y = Dropout_Rate/100, group = Geo, color = Geo , linetype = Geo, label=percent(Dropout_Rate/100, digits = 0) )) +
  geom_line(size = 1.5) +
  geom_label_repel(data = grad_all %>% filter(AcademicYear == max(AcademicYear)) ,
                   size = 3,
                   #  color = "black",
                   show.legend = FALSE) +
  theme_hc() +  
  #        coord_flip() +
  scale_color_few() +
  mcoe_theme +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0,.15)) +
  scale_linetype_manual(values =  c("dashed", "solid")) +
  guides(linetype = FALSE) +
  labs(x = "",
       y = "",
       color ="",
       title = ("Dropout Rates Over Time"),
       caption = "Source: Adjusted Cohort Outcome Data \n https://www.cde.ca.gov/ds/sd/sd/filesacgr.asp") 

ggsave(here("figs","2021","dropout.png"), width = 6, height = 4)


ggplot(grad_sub, aes( y = Dropout_Rate/100, x =fct_reorder(StudentGroup, Dropout_Rate) ,  label = percent(Dropout_Rate/100, accuracy = .1))) +
  geom_segment( aes(x=fct_reorder(StudentGroup, Dropout_Rate), xend=fct_reorder(StudentGroup, Dropout_Rate), y=0, yend=Dropout_Rate/100),
                color="orange",
                size =2 ) +
  geom_point( color="orange", size=5, alpha=0.6) +
  coord_flip() +
  geom_text(size = 3, color = "black") +
  facet_grid(facets = vars(StudentGroupCategory), scales = "free" ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_hc() +
  mcoe_theme +
  labs(x = "",
       y = "",
       color ="",
       title = ("Dropout Rates by Student Group"),
       caption = "Source: Adjusted Cohort Outcome Data \n https://www.cde.ca.gov/ds/sd/sd/filesacgr.asp") 

ggsave(here("figs","2021","drop-subgroup.png"), width = 6, height = 7)


### Enrollment -------
# Unduplicated Pupil Count (UPC) of free or reduced price meal (FRPM) eligibility, English learner (EL), and foster youth data
# from the California Longitudinal Pupil Achievement Data System (CALPADS). These counts are the starting point for determining 
# the Unduplicated Pupil Percentage (UPP) used in the Local Control Funding Formula (LCFF) supplemental and concentration grant calculations.

## https://www.cde.ca.gov/ds/sd/sd/filescupc.asp


cupc.mry <- tbl(con, "UPC") %>% 
  filter(County_Code == 27) %>%
  collect() %>%
  group_by(academic_year)


# Total Enrollment

enrollment <- function(enrolltype, lowlimit, highlimit,tit ){
  cupc.mry %>%
  summarise(countyenrollment = sum({{enrolltype}})) %>%
  `colnames<-`(c("year","value")) %>%
  ungroup() %>%
  mutate(Geo = "Monterey County") %>%
  ggplot( aes(x = year, y = value,  label=comma( value) , group = Geo)) +
  geom_line(size = 1.5) +
  geom_label( size = 3, color = "black") +
  theme_hc() +
  scale_color_few() +
  scale_y_continuous(labels = comma_format(), limits = c(lowlimit,highlimit)) +
  labs(x = "",
       y = "",
       color ="",
       title = (tit),
       caption = "Source: Unduplicated Pupil Count \n https://www.cde.ca.gov/ds/sd/sd/filescupc.asp")
}




#******
enrollment(total_enrollment, 60000, 80000, "Total Enrollment in Monterey County")
ggsave(here("figs","2021","total enrollment.png"), width = 6, height = 4)

#******
enrollment(homeless, 0, 10000, "Homeless Enrollment in Monterey County")
ggsave(here("figs","2021","homeless enrollment.png"), width = 6, height = 4)

#******
enrollment(unduplicated_frpm_eligible_count, 50000, 60000, "Free/Reduced Price Meals Enrollment in Monterey County")
ggsave(here("figs","2021","frpm enrollment.png"), width = 6, height = 4)


enrollment(EnglishLearnerEl, 25000, 35000, "English Learner Enrollment in Monterey County")
ggsave(here("figs","2021","EL enrollment.png"), width = 6, height = 4)



#  Special Education Enrollment

#********
sped.count <- function(yr) {
  
  url <- paste0("https://data1.cde.ca.gov/dataquest/SpecEd/SEEnrEthDis2.asp?cChoice=SEEthDis2&cYear=",yr,"&TheCounty=27,MONTEREY&clevel=County&ReptCycle=December")
  
  
  table <- xml2::read_html(url) %>%
    html_nodes( xpath = "/html/body/center[3]/table") %>%
    html_table(fill = TRUE) %>%
    flatten() %>%
    as_tibble(.name_repair = "unique")
  
  table[22,15] %>% 
    simplify() 
}  # Goes to URL, uses year to pull out total SPED kids that year 



sped <- tibble( year = c("2011-12", "2012-13" , "2013-14" ,"2014-15", "2015-16" , "2016-17", "2017-18", "2018-19"))

sped <- sped %>% mutate(value = map(year, sped.count) %>%   # use function for all the years 
                          simplify() %>%    
                          gsub(",","", .  ) %>%
                          as.numeric()  ) %>%    # to get a number out rathe than a list or character with a comma
  mutate(Geo = "Monterey County")  # create a group for graphing


ggplot(sped, aes(x = year, y = value, group = Geo, label=comma( value) )) +
  geom_line(size = 1.5) +
  geom_label( size = 3, color = "black") +
  theme_hc() +
  scale_color_few() +
  scale_y_continuous(labels = comma_format(), limits = c(5000,10000)) +
  labs(x = "",
       y = "",
       color ="",
       title = ("Special Education Enrollment in Monterey County"),
       caption = "Source: DataQuest \n https://data1.cde.ca.gov/dataquest/SpecEd/SEEnrEthDis2.asp?cChoice=SEEthDis2&\ncYear=2018-19&TheCounty=27,MONTEREY&clevel=County&ReptCycle=December") 

ggsave(here("figs","2021","SPED enrollment.png"), width = 6, height = 4)


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
  geom_label(data = susp_all %>% filter(academic_year == max(academic_year)) , size = 3, color = "black") +
  theme_hc() +
  #        coord_flip() +
  scale_color_few() +
  scale_linetype_manual(values =  c("dashed", "solid")) +
  guides(linetype = FALSE) +
  labs(x = "",
       y = "",
       color ="",
       title = ("K-12 Suspension Rates Over Time"), # fn("K-12 Suspension Rates Over Time"),
       caption = "Source: Suspension Data Files \n https://www.cde.ca.gov/ds/sd/sd/filessd.asp")

ggsave(here("figs","2021","suspension.png"), width = 6, height = 4)



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

ggsave(here("figs","2021","suspension-subgroup.png"), width = 6, height = 7)


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
  geom_label(data = exp_all %>% filter(academic_year == max(academic_year)) , size = 3, color = "black") +
  theme_hc() +
  scale_color_few() +
 # scale_y_continuous(labels = scales::percent_format(accuracy = .01), limits = c(0,0.002)) +
  scale_linetype_manual(values =  c("dashed", "solid")) +
  guides(linetype = FALSE) +
  labs(x = "",
       y = "",
       color ="",
       title =  ("K-12 Expulsion Rates per 1,000 Over Time"), #fn("K-12 Expulsion Rates Over Time"),
       caption = "Source: Expulsion Data Files \n https://www.cde.ca.gov/ds/sd/sd/filesed.asp")

ggsave(here("figs","2021","expulsion.png"), width = 6, height = 4)




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

ggsave(here("figs","2021","expulsion-subgroup.png"), width = 6, height = 7)


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
  guides(linetype = FALSE) +
  guides(color = guide_legend(nrow=3,byrow=FALSE)) +
  labs(x = "",
       y = "",
       color ="",
       title = ("Physical Fitness Over Time by Grade"),
       subtitle = "Percentage Meeting 4 or more of 6 Fitness Tests",
       caption = "Source: Physical Fitness Test Data \n https://www.cde.ca.gov/ta/tg/pf/pftresearch.asp")

ggsave(here("figs","2021","physical.png"), width = 6, height = 5)


# ### Math and ELA scores ----
# 
# # https://caaspp-elpac.cde.ca.gov/caaspp/ResearchFileList?ps=true&lstTestYear=2019&lstTestType=B&lstGroup=1&lstGrade=13&lstSchoolType=A&lstCounty=00&lstDistrict=00000&lstSchool=0000000#dl
# 
# # Generated from the CAASPP repo
# sbac.all <-   read_rds(here("data","sbac-all.rds"))
# 
# sbac.all.multi <-   read_rds(here("data","sbac-all-multi.rds"))
# 
# sbac.filtered <- sbac.all.multi %>% 
#   filter(
#          `County Code` %in% c("00", "27"),
#          `District Code` == "00000",
#          Grade == "Overall") %>%
#   select(`Subgroup ID`, `Student Group` , `Demographic Name` ,`County Code`, TestID,  starts_with("Percentage Standard Met and")) %>%
#   pivot_longer(cols = `Percentage Standard Met and Above.19`:`Percentage Standard Met and Above.17`) %>%  # Can be 15 if not including the state
#   mutate(Geo = if_else(`County Code` == "00", "California", "Monterey County"),
#          Year = gsub("^.*\\.","",name)) %>%
#   mutate(`Year` = `Year`%>% recode(`17` = "2016-17", `18` = "2017-18", `19` = "2018-19"))
# 
# 
# test <- "ELA"
# 
# sbac.filtered %>%
#   filter(TestID == test,
#          `Subgroup ID` == 1) %>%
# ggplot( aes(x = Year, y = value/100, group = Geo, color = Geo , linetype = Geo, label=percent(value/100, digits = 0) )) +
#   geom_line(size = 1.5) +
#   geom_label(data = sbac.filtered %>%
#               filter(TestID == test,
#                      `Subgroup ID` == 1) %>%
#               filter(Year == max(Year)) , size = 3, color = "black") +
#   theme_hc() +
#   scale_color_few() +
#   scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0,.6)) +
#   scale_linetype_manual(values =  c("dashed", "solid")) +
#   guides(linetype = FALSE) +
#   labs(x = "",
#        y = "",
#        color ="",
#        title = (paste0(test, " Percentage Meeting and Exceeding Rates Over Time")),
#        caption = "Source: CAASPP Research Files \n https://caaspp-elpac.cde.ca.gov/caaspp/ResearchFileList")
# 
# ggsave(here("figs","2021",paste0(test,".png")), width = 6, height = 4)
# 
# 
# sbac.filtered %>%
#   filter(TestID == test,
#          Year == max(Year),
#          `County Code` == "27",
#         `Subgroup ID` < 200 &`Subgroup ID` %notin% c(190,212)) %>% # `Demographic Name`
#   ggplot( aes( y = value/100, x =fct_reorder(`Demographic Name`, value) ,  label = percent(value/100, accuracy = .1))) +
#   geom_segment( aes(x=fct_reorder(`Demographic Name`, value/100), xend=fct_reorder(`Demographic Name`, value/100), y=0, yend=value/100),
#                 color="orange",
#                 size =2 ) +
#   geom_point( color="orange", size=5, alpha=0.6) +
#   coord_flip() +
#   geom_text(size = 3, color = "black") +
#   scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
#   facet_grid(facets = vars(`Student Group`), scales = "free" ) +
#   theme_hc() +
#   mcoe_theme +
#   labs(x = "",
#        y = "",
#        color ="",
#        title = (paste0(test, " Percentage Meeting and Exceeding Rates by Student Group")),
#        caption = "Source: CAASPP Research Files \n https://caaspp-elpac.cde.ca.gov/caaspp/ResearchFileList")
# 
# ggsave(here("figs","2021",paste0(test,"-sub.png")), width = 8, height = 11)
# 
# 
# test <- "Math"
# 
# sbac.filtered %>%
#   filter(TestID == test,
#          `Subgroup ID` == 1) %>%
#   ggplot( aes(x = Year, y = value/100, group = Geo, color = Geo , linetype = Geo, label=percent(value/100, digits = 0) )) +
#   geom_line(size = 1.5) +
#   geom_label(data = sbac.filtered %>%
#               filter(TestID == test,
#                      `Subgroup ID` == 1) %>%
#               filter(Year == max(Year)) , size = 3, color = "black") +
#   theme_hc() +
#   scale_color_few() +
#   scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0,.6)) +
#   scale_linetype_manual(values =  c("dashed", "solid")) +
#   guides(linetype = FALSE) +
#   labs(x = "",
#        y = "",
#        color ="",
#        title = (paste0(test, " Percentage Meeting and Exceeding Rates Over Time")),
#        caption = "Source: CAASPP Research Files \n https://caaspp-elpac.cde.ca.gov/caaspp/ResearchFileList")
# 
# ggsave(here("figs","2021",paste0(test,".png")), width = 6, height = 4)
# 
# 
# sbac.filtered %>%
#   filter(TestID == test,
#          Year == max(Year),
#          `County Code` == "27",
#          `Subgroup ID` < 200 &`Subgroup ID` %notin% c(190,212)) %>% # `Demographic Name`
#   ggplot( aes( y = value/100, x =fct_reorder(`Demographic Name`, value) ,  label = percent(value/100, accuracy = .1))) +
#   geom_segment( aes(x=fct_reorder(`Demographic Name`, value/100), xend=fct_reorder(`Demographic Name`, value/100), y=0, yend=value/100),
#                 color="orange",
#                 size =2 ) +
#   geom_point( color="orange", size=5, alpha=0.6) +
#   coord_flip() +
#   geom_text(size = 3, color = "black") +
#   facet_grid(facets = vars(`Student Group`), scales = "free" ) +
#   scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
#   theme_hc() +
#   mcoe_theme +
#   labs(x = "",
#        y = "",
#        color ="",
#        title = (paste0(test, " Percentage Meeting and Exceeding Rates by Student Group")),
#        caption = "Source: CAASPP Research Files \n https://caaspp-elpac.cde.ca.gov/caaspp/ResearchFileList")
# 
# ggsave(here("figs","2021",paste0(test,"-sub.png")), width = 8, height = 11)
# 
# 
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


ggsave(here("figs","2021","reclass.png"), width = 7, height = 5)

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

