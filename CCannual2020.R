
library(tidyverse)
library(janitor)
library(here)
library(scales)
library(ggthemes)
library(readxl)

my_theme <- list(theme_hc(),
                 scale_fill_few() ,
      #           geom_text(size = 2, position = position_dodge(width = 1)),
                 theme(plot.title.position = "plot"),
                 labs(x = "",
                      y = "",
                      fill ="") )

### Graduation Rate ------

##  https://www.cde.ca.gov/ds/sd/sd/filesacgr.asp

# 4 year cohort rate.  

years <- c("1819","1617","1718")

grad_all <- read.delim(here("data", "cohort1819.txt") ) %>% 
  clean_names(case = "upper_camel") %>%
  filter(FALSE)


for (i in years) {
  grad_new <- read.delim(here("data",  paste0("cohort", i, ".txt") ) ) %>% 
    clean_names(case = "upper_camel") %>%
    filter( (AggregateLevel == "T"  |CountyCode == 27),
            is.na(DistrictCode),
            ReportingCategory == "TA",
            CharterSchool =="All",
            Dass == "All")
  #               filter( str_detect(CountyName,"Monterey")  )
  
  grad_all <- bind_rows(grad_all, grad_new)
}

grad_all <- grad_all %>% 
  mutate(Geo = if_else(AggregateLevel == "T", "California" ,CountyName )) %>%
  mutate_at(vars(CohortStudents:StillEnrolledRate), funs(as.numeric) ) 
  
  


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


years <- c("1112","1213","1314","1415","1516","1617","1718", "1819")

susp_all <- read.delim(here("data", "susp1819.txt") ) %>% 
  clean_names(case = "upper_camel") %>%
  filter(FALSE)


for (i in years) {
  susp_new <- read.delim(here("data",  paste0("susp", i, ".txt") ) ) %>% 
    clean_names(case = "upper_camel") %>%
    filter( (AggregateLevel == "T"  |CountyCode == 27),
            is.na(DistrictCode),
            ReportingCategory == "TA")
  #               filter( str_detect(CountyName,"Monterey")  )
  
  susp_all <- bind_rows(susp_all, susp_new)
}


susp_all <- susp_all %>%
  filter(CharterYn == "All"|is.na(CharterYn)|CharterYn == "") %>%
  mutate(Geo = if_else(AggregateLevel == "T", "California" ,CountyName )) %>%
  mutate_at(vars(CumulativeEnrollment:SuspensionCountDefianceOnly), funs(as.numeric) ) %>%
  mutate(SuspensionCountOfStudentsSuspendedDefianceOnly2 =  if_else(is.na(SuspensionCountOfStudentsSuspendedDefianceOnly),
                                                                    as.numeric( SuspensionCountDefianceOnly ) , 
                                                                    as.numeric( SuspensionCountOfStudentsSuspendedDefianceOnly) )  )




ggplot(susp_all, aes(x = AcademicYear, y = SuspensionRateTotal, group = Geo, color = Geo , label = percent(SuspensionRateTotal/100, digits = 1))) +
  geom_line(size = 1.5) +
  geom_text(data = susp_all %>% filter(AcademicYear == max(AcademicYear)) , size = 3, color = "black") +
  theme_hc() +
  #        coord_flip() +
  scale_color_few() +
#  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(.03,.07)) +
  labs(x = "",
       y = "",
       color ="",
       title = ("K-12 Suspension Rates Over Time"), # fn("K-12 Suspension Rates Over Time"),
       caption = "Source: Suspension Data Files \n https://www.cde.ca.gov/ds/sd/sd/filessd.asp")

ggsave(here("figs","2020","suspension.png"), width = 6, height = 4)


### Expulsion -------

# https://www.cde.ca.gov/ds/sd/sd/filesed.asp



years <- c("1112","1213","1314","1415","1516","1617","1718", "1819")

exp_all <- read.delim(here("data", "exp1819.txt") ) %>% 
  clean_names(case = "upper_camel") %>%
  filter(FALSE)


for (i in years) {
  exp_new <- read.delim(here("data", paste0("exp", i, ".txt") ) ) %>% 
    clean_names(case = "upper_camel") %>%
    filter( (AggregateLevel == "T"  |CountyCode == 27),
            is.na(DistrictCode),
            ReportingCategory == "TA",
            )
  #               filter( str_detect(CountyName,"Monterey")  )
  
  exp_all <- bind_rows(exp_all, exp_new)
}


exp_all <- exp_all %>%
  filter(CharterYn == "All"|is.na(CharterYn)|CharterYn == "") %>%
  mutate(Geo = if_else(AggregateLevel == "T", "California" ,CountyName )) %>%
  mutate_at(vars(CumulativeEnrollment:ExpulsionCountDefianceOnly), funs(as.numeric) ) %>%
  mutate(rate = UnduplicatedCountOfStudentsExpelledTotal/CumulativeEnrollment)



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



pft.mry %>% 
  filter(str_detect(name, "Perc")) %>%
ggplot(aes(x= year, y = value/100, group = GeoGrade, color = GeoGrade, label=percent(value/100, digits = 0))) +
  geom_line(size = 1.5) +
  geom_text(data = pft.mry %>% 
              filter(str_detect(name, "Perc")) %>%
              filter(year == max(year)) , size = 3, color = "black") +
  theme_hc() +
  scale_color_few() +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(.50, .80)) +
  labs(x = "",
       y = "",
       color ="",
       title = ("Percentage Meeting 4 or more of 6 Physical Fitness Tests Over Time"),
       caption = "Source: Physical Fitness Test Data \n https://www.cde.ca.gov/ta/tg/pf/pftresearch.asp")

ggsave(here("figs","2020","physical.png"), width = 6, height = 4)


# Works but needs theming and colors so that MRY and CA are distinguished 



### Math and ELA scores ----

# https://caaspp-elpac.cde.ca.gov/caaspp/ResearchFileList?ps=true&lstTestYear=2019&lstTestType=B&lstGroup=1&lstGrade=13&lstSchoolType=A&lstCounty=00&lstDistrict=00000&lstSchool=0000000#dl

# Generated from the CAASPP repo
sbac.all <-   read_rds(here("data","sbac-all.rds"))

sbac.all.multi <-   read_rds(here("data","sbac-all-multi.rds"))

sbac.filtered <- sbac.all.multi %>% 
  filter(`Subgroup ID` == 1,
         `County Code` %in% c("00", "27"),
         `District Code` == "00000",
         Grade == "Overall") %>%
  select(`County Code`, TestID,  starts_with("Percentage Standard Met and")) %>%
  pivot_longer(cols = `Percentage Standard Met and Above.19`:`Percentage Standard Met and Above.17`) %>%  # Can be 15 if not including the state
  mutate(Geo = if_else(`County Code` == "00", "California", "Monterey County"),
         Year = gsub("^.*\\.","",name))


test <- "ELA"

sbac.filtered %>%
  filter(TestID == test) %>%
ggplot( aes(x = Year, y = value/100, group = Geo, color = Geo , label=percent(value/100, digits = 0) )) +
  geom_line(size = 1.5) +
  geom_text(data = sbac.filtered %>%
              filter(TestID == test) %>%
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


test <- "Math"

sbac.filtered %>%
  filter(TestID == test) %>%
  ggplot( aes(x = Year, y = value/100, group = Geo, color = Geo , label=percent(value/100, digits = 0) )) +
  geom_line(size = 1.5) +
  geom_text(data = sbac.filtered %>%
              filter(TestID == test) %>%
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


### End ----

