#load packages
library(tidyverse)
library(janitor)
library(sf)

# 1. Data Wrangling Challenge

# read data & clean names
household_data <- read_csv("L4H_household_baseline_sample.csv", na="---")%>%
  clean_names()

individual_data <- read_csv("L4H_individual_baseline_sample.csv", na="---")%>%
  clean_names()

mother_data <- read_csv("L4H_mother_baseline_sample.csv", na="---")%>%
  clean_names()

# filter eligible hhs
household_data1 <- household_data%>%
  filter(hh_eligible%in%"1")

# merge datasets
mother_individual_merged <- individual_data%>%
  full_join(mother_data, by=c("number"="number_0"))

household_merged_data <- household_data%>%
  full_join(mother_individual_merged, by="household_id")

# recode variables
household_merged_data1 <- household_merged_data%>%
  mutate_at(vars(reason_for_ineligibility, rspntgndr, h_hfrml_eductn, rspndtmarital, rspndt_edctn, maincme), funs(factor))%>%
  mutate(reason_for_ineligibility=recode(reason_for_ineligibility, "1"="No adult occupier >16 years", "2"="Withdrawal", "3"="Other reason"))%>%
  mutate(rspntgndr=recode(rspntgndr, "1"="Male", "2"="Female"))%>%
  mutate(h_hfrml_eductn=recode(h_hfrml_eductn, "1"="Not completed Primary school", "2"="Primary school", "3"="Secondary school", "4"="College-graduate", "5"="Madrassa", "6"="Other"))%>%
  mutate(rspndtmarital=recode(rspndtmarital, "1"="Single", "2"="Married monogamous", "3"="Married polygamous", "4"="Divorced/ separated", "5"="Widow(er)"))%>%
  mutate(rspndt_edctn=recode(rspndt_edctn, "1"="No formal education", "2"="Primary School", "3"="Secondary school", "4"="College-graduate", "5"="Madrassa", "6"="Other"))%>%
  mutate(maincme=recode(maincme, "1"="Sale of livestock & livestock products", "2"="Sale of crops", "3"="Trading/business", "4"="Employment (salaried income)", "5"="Sale of personal assets", "6"="Remittance", "7"="Other"))

# separate
household_merged_data2 <- household_merged_data1%>%
  separate(lvstckown, c("lvstckown1", "lvstckown2", "lvstckown3", "lvstckown4", "lvstckown5", "lvstckown6", "lvstckown7", "lvstckown8", "lvstckown9", "lvstckown10", "lvstckown11", "lvstckown12", "lvstckown13", "lvstckown14", "lvstckown15"), " ")%>%
  separate(herdynamics, c("herdynamics1", "herdynamics2", "herdynamics3", "herdynamics4", "herdynamics5", "herdynamics6", "herdynamics7"), " ")

# new column
household_merged_data3 <- household_merged_data2%>%
  mutate(study_arm = ifelse(village.x%in%c("Lependera", "Saale-Sambakah", "Namarei", "Manyatta Lengima", "Lokoshula", "TubchaDakhane", "Rengumo-Gargule"), "Study arm 1", ifelse(  village.x%in%c("Uyam  village", "Rongumo_kurkum", "Manyatta K.A.G", "Ltepes Ooodo", "Lbaarok1"), "Study arm 2", "Study arm 3")))

# new object
herd_dynamics <- household_merged_data3%>%
  select(interview_date.x, household_id, study_arm, cwsbrth, shpbrth, goatsbrth, cmlsbrth, calves_death, bulls_death, cows_death, sheep_death, msheep_death, fsheep_death, goats_death, mgoats_death, fgoats_death, camels_death, mcamels_death, fcamels_death, cowsgft, sheepgfts, goatsgft, cmlsgft, cowsgvnout, sheepgvnout, goatsgvnout, cmlsgvnout)

# new column
herd_dynamics1 <- herd_dynamics%>%
  separate(interview_date.x, c("year", "month", "day"), "-")%>%
  mutate(monthyear=paste0(year, "-", month))

#
herd_dynamics2 <- herd_dynamics1%>%
  group_by(study_arm, monthyear)%>%
  mutate(cow_births=sum(cwsbrth, na.rm = T))%>%
  mutate(shp_births=sum(shpbrth, na.rm = T))%>%
  mutate(gt_births=sum(goatsbrth, na.rm = T))%>%
  mutate(cml_births=sum(cmlsbrth, na.rm = T))%>%
  mutate(cow_deaths=sum(c(calves_death, bulls_death, cows_death), na.rm = T))%>%
  mutate(shp_deaths=sum(c(sheep_death, msheep_death, fsheep_death), na.rm = T))%>%
  mutate(gt_deaths=sum(c(goats_death, mgoats_death, fgoats_death), na.rm = T))%>%
  mutate(cml_deaths=sum(c(camels_death, mcamels_death, fcamels_death), na.rm = T))%>%
  mutate(cow_gifts=sum(cowsgft, na.rm = T))%>%
  mutate(shp_gifts=sum(sheepgfts, na.rm = T))%>%
  mutate(gt_gifts=sum(goatsgft, na.rm = T))%>%
  mutate(cml_gifts=sum(cmlsgft, na.rm = T))%>%
  mutate(cow_givenout=sum(cowsgvnout, na.rm = T))%>%
  mutate(shp_givenout=sum(sheepgvnout, na.rm = T))%>%
  mutate(gt_givenout=sum(goatsgvnout, na.rm = T))%>%
  mutate(cml_givenout=sum(cmlsgvnout, na.rm = T))%>%
  ungroup()

#subset
herd_dynamics3 <- herd_dynamics2%>%
  select(study_arm, monthyear, cow_births, shp_births, gt_births, cml_births, cow_deaths, shp_deaths, gt_deaths, cml_deaths, cow_gifts, shp_gifts, gt_gifts, cml_gifts, cow_givenout, shp_givenout, gt_givenout, cml_givenout)%>%
  distinct(study_arm, monthyear, .keep_all=T)

# 2. Data Visualization Challenge

#prep data
herd_dynamics4 <- herd_dynamics3%>%
  pivot_longer(cols = c(cow_births:cml_givenout), names_to = "category", values_to = "number")%>%
  mutate(dynamics=ifelse(grepl("births", category), "Births", ifelse(grepl("deaths", category), "Deaths", ifelse(grepl("gifts", category), "Gifts", "Given Out"))))%>%
  mutate(species=ifelse(grepl("cow", category), "Cows", ifelse(grepl("shp", category), "Sheep", ifelse(grepl( "gt", category), "Goats", "Camels"))))

# visuals 1
herd_dynamics_plot <- ggplot(herd_dynamics4,aes(x=monthyear, y=number, fill=species))+
  geom_col(position="dodge", stat="identity")+
  theme_bw()+
  facet_grid(rows=vars(dynamics))+
  labs( y="Number of animals", title="Herd Dynamics", x="Time period (year-month)", fill="Species")+
  scale_fill_manual(values = c("#d7191c","#fdae61","#ffffbf","#2c7bb6"))+
  theme(text=element_text(size=12))
herd_dynamics_plot

