#' ---
#' output: github_document
#' ---


library(tidyverse)
library(janitor)
library(here)
library(scales)
library(ggthemes)
library(readxl)
library(vroom)
library(rvest)
library(xml2)

my_theme <- list(theme_hc(),
                 scale_fill_few() ,
      #           geom_text(size = 2, position = position_dodge(width = 1)),
                 theme(plot.title.position = "plot"),
                 labs(x = "",
                      y = "",
                      fill ="") )


`%notin%` <- Negate(`%in%`)


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

grad_vroom <- import_files(here("data","grad4"),"cohort*txt") 


grad_all <- grad_vroom %>% 
  mutate(Geo = if_else(AggregateLevel == "T", "California" ,CountyName )) %>%
  mutate_at(vars(CohortStudents:StillEnrolledRate), funs(as.numeric) ) %>%
  filter( (AggregateLevel == "T"  |CountyCode == 27),
          is.na(DistrictCode),
          ReportingCategory == "TA",
          CharterSchool =="All",
          Dass == "All")
  
  


ggplot(grad_all, aes(x = AcademicYear, y = RegularHsDiplomaGraduatesRate/100, group = Geo, color = Geo , label=percent(RegularHsDiplomaGraduatesRate/100, digits = 0) )) +
  geom_line(size = 1.5) +
  geom_text(data = grad_all %>% filter(AcademicYear == max(AcademicYear)) , size = 3, color = "black") +
  theme_hc() +
  #        coord_flip() +
  scale_color_few() +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(.7,.9)) +
  labs(x = "",
       y = "",
       color ="",
       title = ("Graduation Rates Over Time"),
       caption = "Source: Adjusted Cohort Outcome Data \n https://www.cde.ca.gov/ds/sd/sd/filesacgr.asp") 

ggsave(here("figs","2020","graduation.png"), width = 6, height = 4)

### Dropout Rate ------

##  https://www.cde.ca.gov/ds/sd/sd/filesacgr.asp


drop_sub <- grad_vroom %>% 
#  mutate(Geo = if_else(AggregateLevel == "T", "California" ,CountyName )) %>%
  mutate_at(vars(CohortStudents:StillEnrolledRate), funs(as.numeric) ) %>%
  filter( (CountyCode == 27),
          is.na(DistrictCode),
          AcademicYear == max(AcademicYear),
  #        ReportingCategory == "TA",
          CharterSchool =="All",
          Dass == "All") %>%
  left_join(susp.acron)





ggplot(grad_all, aes(x = AcademicYear, y = DropoutRate/100, group = Geo, color = Geo , label=percent(DropoutRate/100, digits = 0) )) +
  geom_line(size = 1.5) +
  geom_text(data = grad_all %>% filter(AcademicYear == max(AcademicYear)) , size = 3, color = "black") +
  theme_hc() +
  #        coord_flip() +
  scale_color_few() +
  my_theme +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0,.15)) +
  labs(x = "",
       y = "",
       color ="",
       title = ("Dropout Rates Over Time"),
       caption = "Source: Adjusted Cohort Outcome Data \n https://www.cde.ca.gov/ds/sd/sd/filesacgr.asp") 

ggsave(here("figs","2020","dropout.png"), width = 6, height = 4)


ggplot(drop_sub, aes( y = DropoutRate, x =fct_reorder(StudentGroup, DropoutRate) ,  label = percent(DropoutRate/100, accuracy = .1))) +
  geom_segment( aes(x=fct_reorder(StudentGroup, DropoutRate), xend=fct_reorder(StudentGroup, DropoutRate), y=0, yend=DropoutRate),
                color="orange",
                size =2 ) +
  geom_point( color="orange", size=5, alpha=0.6) +
  coord_flip() +
  geom_text(size = 3, color = "black") +
  theme_hc() +
  my_theme +
  labs(x = "",
       y = "",
       color ="",
       title = ("Dropout Rates by Student Group"),
       caption = "Source: Adjusted Cohort Outcome Data \n https://www.cde.ca.gov/ds/sd/sd/filesacgr.asp") 

ggsave(here("figs","2020","drop-subgroup.png"), width = 6, height = 7)


### Enrollment -------
# Unduplicated Pupil Count (UPC) of free or reduced price meal (FRPM) eligibility, English learner (EL), and foster youth data
# from the California Longitudinal Pupil Achievement Data System (CALPADS). These counts are the starting point for determining 
# the Unduplicated Pupil Percentage (UPP) used in the Local Control Funding Formula (LCFF) supplemental and concentration grant calculations.

## https://www.cde.ca.gov/ds/sd/sd/filescupc.asp


cupc1415 <- read_excel(here("data","cupc1415.xls"), sheet = "LEA-Level CALPADS UPC Data")%>% 
  clean_names(case = "upper_camel")
cupc1516 <- read_excel(here("data","cupc1516.xls"), sheet = "LEA-Level CALPADS UPC Data", range = "A3:AA2214")
cupc1617 <- read_excel(here("data","cupc1617-k12.xls"), sheet = "LEA-Level CALPADS UPC Data", range = "A3:AA2236")
cupc1718 <- read_excel(here("data","cupc1718-k12.xlsx"), sheet = "LEA-Level CALPADS UPC Data", range = "A3:AA2258")
cupc1819 <- read_excel(here("data","cupc1819-k12.xlsx"), sheet = "LEA-Level CALPADS UPC Data", range = "A3:AA2302")

colnames(cupc1516)<-colnames(cupc1415)
colnames(cupc1617)<-colnames(cupc1415)
colnames(cupc1718)<-colnames(cupc1415)
colnames(cupc1819)<-colnames(cupc1415)

cupc <- list(cupc1415, cupc1516, cupc1617, cupc1718, cupc1819) %>% bind_rows()

cupc.mry <- cupc %>% 
  filter(CountyCode == 27) %>%
  group_by(AcademicYear)

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

enrollment(TotalEnrollment, 60000, 80000, "Total Enrollment in Monterey County")
ggsave(here("figs","2020","total enrollment.png"), width = 6, height = 4)


enrollment(Homeless, 0, 10000, "Homeless Enrollment in Monterey County")
ggsave(here("figs","2020","homeless enrollment.png"), width = 6, height = 4)


enrollment(UnduplicatedFrpmEligibleCount, 50000, 60000, "Free/Reduced Price Meals Enrollment in Monterey County")
ggsave(here("figs","2020","frpm enrollment.png"), width = 6, height = 4)


enrollment(EnglishLearnerEl, 25000, 35000, "English Learner Enrollment in Monterey County")
ggsave(here("figs","2020","EL enrollment.png"), width = 6, height = 4)



#  Special Education Enrollment


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

ggsave(here("figs","2020","SPED enrollment.png"), width = 6, height = 4)

### Kindergarten Readiness -----


# In 2015, 21% of kindergartners had comprehensive mastery at kindergarten entry. Comprehensive mastery indicates that a child is well-prepared to
# enter kindergarten across all four developmental domains assessed by teachers using the Desired Results Developmental Profile–School Readiness
# (DRDP-SR). Teachers rated children’s competency on each item of the DRDP–SR using the following five-point scale: (1) Exploring, (2) Developing, 
# (3) Building, (4) Integrating, and (5) Applying. Scores of (4) Integrating and (5) Applying indicate mastery of that item. The term comprehensive 
# mastery is used to identify children with an average score of 4 or above across all items in the DRDP-SR excluding the English Language Development
# domain[^kra].
# [^kra]: Nurturing Success: A Portrait of Kindergarten Readiness in Monterey County.  First 5 Monterey County and Harder+Co.
# 





### Suspension ------

# https://www.cde.ca.gov/ds/sd/sd/filessd.asp



susp_vroom <- import_files(here("data","susp"),"sus*txt") %>% 
  filter( 
          is.na(DistrictCode),
          CharterYn == "All"|is.na(CharterYn)|CharterYn == "") %>%
  mutate(Geo = if_else(AggregateLevel == "T", "California" ,CountyName )) %>%
  mutate_at(vars(CumulativeEnrollment:SuspensionCountDefianceOnly), funs(as.numeric) ) %>%
  mutate(SuspensionCountOfStudentsSuspendedDefianceOnly2 =  if_else(is.na(SuspensionCountOfStudentsSuspendedDefianceOnly),
                                                                    as.numeric( SuspensionCountDefianceOnly ) , 
                                                                    as.numeric( SuspensionCountOfStudentsSuspendedDefianceOnly) )  )


susp_all <- susp_vroom %>%
  filter( (AggregateLevel == "T"  |CountyCode == 27),
          ReportingCategory == "TA") 


susp_sub <- susp_vroom %>%  
  filter( 
    (CountyCode == 27),
    AcademicYear == max(AcademicYear)
 ) %>%
  left_join(susp.acron)
  


ggplot(susp_all, aes(x = AcademicYear, y = SuspensionRateTotal, group = Geo, color = Geo , label = percent(SuspensionRateTotal/100, digits = 1))) +
  geom_line(size = 1.5) +
  geom_text(data = susp_all %>% filter(AcademicYear == max(AcademicYear)) , size = 3, color = "black") +
  theme_hc() +
  #        coord_flip() +
  scale_color_few() +
  labs(x = "",
       y = "",
       color ="",
       title = ("K-12 Suspension Rates Over Time"), # fn("K-12 Suspension Rates Over Time"),
       caption = "Source: Suspension Data Files \n https://www.cde.ca.gov/ds/sd/sd/filessd.asp")

ggsave(here("figs","2020","suspension.png"), width = 6, height = 4)



ggplot(susp_sub, aes( y = SuspensionRateTotal, x =fct_reorder(StudentGroup, SuspensionRateTotal) ,  label = percent(SuspensionRateTotal/100, accuracy = .1))) +
  geom_segment( aes(x=fct_reorder(StudentGroup, SuspensionRateTotal), xend=fct_reorder(StudentGroup, SuspensionRateTotal), y=0, yend=SuspensionRateTotal),
                color="orange",
                size =2 ) +
  geom_point( color="orange", size=5, alpha=0.6) +
  coord_flip() +
  geom_text(size = 3, color = "black") +
  theme_hc() +
  my_theme +
  labs(x = "",
       y = "",
       color ="",
       title = ("K-12 Suspension Rates By Subgroup"), # fn("K-12 Suspension Rates Over Time"),
       caption = "Source: Suspension Data Files \n https://www.cde.ca.gov/ds/sd/sd/filessd.asp")

ggsave(here("figs","2020","suspension-subgroup.png"), width = 6, height = 7)


### Expulsion -------

# https://www.cde.ca.gov/ds/sd/sd/filesed.asp


exp_vroom <- import_files(here("data","exp"),"exp*txt") %>%
  filter(CharterYn == "All"|is.na(CharterYn)|CharterYn == "") %>%
  mutate(Geo = if_else(AggregateLevel == "T", "California" ,CountyName )) %>%
  mutate_at(vars(CumulativeEnrollment:ExpulsionCountDefianceOnly), funs(as.numeric) ) %>%
  mutate(rate = UnduplicatedCountOfStudentsExpelledTotal/CumulativeEnrollment)

exp_all <- exp_vroom %>%
  filter( (AggregateLevel == "T"  |CountyCode == 27),
          is.na(DistrictCode),
          ReportingCategory == "TA",
          AcademicYear == max(AcademicYear)
  ) 


exp_sub <- exp_vroom %>%
  filter( (CountyCode == 27),
          is.na(DistrictCode),
          AcademicYear == max(AcademicYear)
  )   %>%
  left_join(susp.acron)



ggplot(exp_all, aes(x = AcademicYear, y = rate, group = Geo, color = Geo , label = percent(rate, digits = 2) )) +
  geom_line(size = 1.5) +
  geom_text(data = exp_all %>% filter(AcademicYear == max(AcademicYear)) , size = 3, color = "black") +
  theme_hc() +
  scale_color_few() +
  scale_y_continuous(labels = scales::percent_format(accuracy = .01), limits = c(0,0.002)) +
  labs(x = "",
       y = "",
       color ="",
       title =  ("K-12 Expulsion Rates Over Time"), #fn("K-12 Expulsion Rates Over Time"),
       caption = "Source: Expulsion Data Files \n https://www.cde.ca.gov/ds/sd/sd/filesed.asp")

ggsave(here("figs","2020","expulsion.png"), width = 6, height = 4)




ggplot(exp_sub, aes( y = rate, x =fct_reorder(StudentGroup, rate) ,  label = percent(rate, accuracy = .01))) +
  geom_segment( aes(x=fct_reorder(StudentGroup, rate), xend=fct_reorder(StudentGroup, rate), y=0, yend=rate),
                color="orange",
                size =2 ) +
  geom_point( color="orange", size=5, alpha=0.6) +
  coord_flip() +
  geom_text(size = 3, color = "black") +
  theme_hc() +
  my_theme +
  labs(x = "",
       y = "",
       color ="",
       title = ("K-12 Expulsion Rates By Subgroup"), # fn("K-12 Suspension Rates Over Time"),
       caption = "Source: Expulsion Data Files \n https://www.cde.ca.gov/ds/sd/sd/filesed.asp")

ggsave(here("figs","2020","expulsion-subgroup.png"), width = 6, height = 7)


### Physical Fitness -----

# https://www.cde.ca.gov/ta/tg/pf/pftresearch.asp


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
ggplot(aes(x= year, y = value/100, group = GeoGrade, color = GeoGrade, label=percent(value/100, digits = 0))) +
  geom_line(size = 1.5) +
  geom_text(data = pft.mry %>% 
              filter(str_detect(name, "Perc")) %>%
              filter(year == max(year)) , size = 3, color = "black") +
  theme_hc() +
  scale_color_manual(values = pft.pal ) +
#    scale_color_few() +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(.50, .80)) +
  guides(color = guide_legend(nrow=3,byrow=FALSE)) +
  labs(x = "",
       y = "",
       color ="",
       title = ("Physical Fitness Over Time by Grade"),
       subtitle = "Percentage Meeting 4 or more of 6 Fitness Tests",
       caption = "Source: Physical Fitness Test Data \n https://www.cde.ca.gov/ta/tg/pf/pftresearch.asp")

ggsave(here("figs","2020","physical.png"), width = 6, height = 6)


### Math and ELA scores ----

# https://caaspp-elpac.cde.ca.gov/caaspp/ResearchFileList?ps=true&lstTestYear=2019&lstTestType=B&lstGroup=1&lstGrade=13&lstSchoolType=A&lstCounty=00&lstDistrict=00000&lstSchool=0000000#dl

# Generated from the CAASPP repo
sbac.all <-   read_rds(here("data","sbac-all.rds"))

sbac.all.multi <-   read_rds(here("data","sbac-all-multi.rds"))

sbac.filtered <- sbac.all.multi %>% 
  filter(
         `County Code` %in% c("00", "27"),
         `District Code` == "00000",
         Grade == "Overall") %>%
  select(`Subgroup ID`, `Student Group` , `Demographic Name` ,`County Code`, TestID,  starts_with("Percentage Standard Met and")) %>%
  pivot_longer(cols = `Percentage Standard Met and Above.19`:`Percentage Standard Met and Above.17`) %>%  # Can be 15 if not including the state
  mutate(Geo = if_else(`County Code` == "00", "California", "Monterey County"),
         Year = gsub("^.*\\.","",name))


test <- "ELA"

sbac.filtered %>%
  filter(TestID == test,
         `Subgroup ID` == 1) %>%
ggplot( aes(x = Year, y = value/100, group = Geo, color = Geo , label=percent(value/100, digits = 0) )) +
  geom_line(size = 1.5) +
  geom_text(data = sbac.filtered %>%
              filter(TestID == test,
                     `Subgroup ID` == 1) %>%
              filter(Year == max(Year)) , size = 3, color = "black") +
  theme_hc() +
  scale_color_few() +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0,.6)) +
  labs(x = "",
       y = "",
       color ="",
       title = (paste0(test, " Percentage Meeting and Exceeding Rates Over Time")),
       caption = "Source: CAASPP Research Files \n https://caaspp-elpac.cde.ca.gov/caaspp/ResearchFileList")

ggsave(here("figs","2020",paste0(test,".png")), width = 6, height = 4)


sbac.filtered %>%
  filter(TestID == test,
         Year == max(Year),
         `County Code` == "00",
        `Subgroup ID` < 200 &`Subgroup ID` %notin% c(190,212)) %>% # `Demographic Name`
  ggplot( aes( y = value/100, x =fct_reorder(`Demographic Name`, value) ,  label = percent(value/100, accuracy = .1))) +
  geom_segment( aes(x=fct_reorder(`Demographic Name`, value/100), xend=fct_reorder(`Demographic Name`, value/100), y=0, yend=value/100),
                color="orange",
                size =2 ) +
  geom_point( color="orange", size=5, alpha=0.6) +
  coord_flip() +
  geom_text(size = 3, color = "black") +
  theme_hc() +
  my_theme +
  labs(x = "",
       y = "",
       color ="",
       title = (paste0(test, " Percentage Meeting and Exceeding Rates by Student Group")),
       caption = "Source: CAASPP Research Files \n https://caaspp-elpac.cde.ca.gov/caaspp/ResearchFileList")

ggsave(here("figs","2020",paste0(test,"-sub.png")), width = 8, height = 8)


test <- "Math"

sbac.filtered %>%
  filter(TestID == test,
         `Subgroup ID` == 1) %>%
  ggplot( aes(x = Year, y = value/100, group = Geo, color = Geo , label=percent(value/100, digits = 0) )) +
  geom_line(size = 1.5) +
  geom_text(data = sbac.filtered %>%
              filter(TestID == test,
                     `Subgroup ID` == 1) %>%
              filter(Year == max(Year)) , size = 3, color = "black") +
  theme_hc() +
  scale_color_few() +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0,.6)) +
  labs(x = "",
       y = "",
       color ="",
       title = (paste0(test, " Percentage Meeting and Exceeding Rates Over Time")),
       caption = "Source: CAASPP Research Files \n https://caaspp-elpac.cde.ca.gov/caaspp/ResearchFileList")

ggsave(here("figs","2020",paste0(test,".png")), width = 6, height = 4)


sbac.filtered %>%
  filter(TestID == test,
         Year == max(Year),
         `County Code` == "00",
         `Subgroup ID` < 200 &`Subgroup ID` %notin% c(190,212)) %>% # `Demographic Name`
  ggplot( aes( y = value/100, x =fct_reorder(`Demographic Name`, value) ,  label = percent(value/100, accuracy = .1))) +
  geom_segment( aes(x=fct_reorder(`Demographic Name`, value/100), xend=fct_reorder(`Demographic Name`, value/100), y=0, yend=value/100),
                color="orange",
                size =2 ) +
  geom_point( color="orange", size=5, alpha=0.6) +
  coord_flip() +
  geom_text(size = 3, color = "black") +
  theme_hc() +
  my_theme +
  labs(x = "",
       y = "",
       color ="",
       title = (paste0(test, " Percentage Meeting and Exceeding Rates by Student Group")),
       caption = "Source: CAASPP Research Files \n https://caaspp-elpac.cde.ca.gov/caaspp/ResearchFileList")

ggsave(here("figs","2020",paste0(test,"-sub.png")), width = 8, height = 8)


### EL Reclassification ----

# https://www.cde.ca.gov/ds/sd/sd/filesreclass.asp

setwd(here("data","reclass"))

files <- fs::dir_ls()

el_vroom <- vroom(files, id = "year")

setwd(here())

el_dist <- el_vroom %>%
  filter( str_detect(CDS,"^27"  )) %>%
  mutate(year = str_extract_all(year, "\\d{1,2}") %>% simplify() %>% as.numeric()) %>%
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
  

ggsave(here("figs","2020","reclass.png"), width = 7, height = 5)

 
# 
# el_enrol <- cupc.mry %>%
#   summarise(countyenrollment = sum(EnglishLearnerEl)) %>%
#   `colnames<-`(c("year","value")) %>%
#   ungroup() %>%
#   mutate(Geo = "EL Enrollment") 
# 
# el_enrol %>%
#   bind_rows(el_dist) %>%
#   ggplot( aes(x = year, y = value,  label=comma( value) , group = Geo, color = Geo)) +
#   geom_line(size = 1.5) +
#   geom_label( size = 3, color = "black") +
#   theme_hc() +
#   scale_color_pander() +
#   scale_y_continuous(labels = comma_format(), limits = c(0,40000)) +
#   labs(x = "",
#        y = "",
#        color ="",
#        title = ("EL Enrollment and Number of Reclassified Students by Year"),
#        caption = "Sources: Unduplicated Pupil Count \n https://www.cde.ca.gov/ds/sd/sd/filescupc.asp \nEL Reclassification Data \n https://www.cde.ca.gov/ds/sd/sd/filesreclass.asp") 
# 


### End ----
devtools::session_info()

