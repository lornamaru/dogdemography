# creating object
object1<-c(1,2,3,4,5,6,7,8,9,10)
#getting mean
mean(object1)
#getting summary stats
summary(object1)
#installing packages
install.packages("tidyverse")
#load packages
libr
# reading data
dat1<- read.csv("http://stats.idre.ucla.edu/stat/data/binary.csv")
head(dat1)
# getting first 6 rows
head(dat1)

# getting data structure
str(dat1)
# data frame
class(dat1$gre)

#record the admit column(0-deny, 1-accept)
dat2<-dat1%>%
  mutate(admit=recode(admit, "0"="deny", "1"="accept"))\
head(dat2)
#rename columns
dat3<-dat1%>%
  rename(position=rank)

# load more data
ideal1<-read.csv("https://raw.githubusercontent.com/cema-uonbi/R_course/main/ideal1.csv")
str(ideal1)
#recode calfSex column (1-male, 2-female)
ideal1a<-ideal1%>%
mutate(CalfSex=recode(CalfSex,"1"="Male","2"="Female"))
# check column names
colnames(ideal1)
#clean column names (rename CADOB=Calf_date_of_birth)
ideal1b<-ideal1%>%
  rename(Calf_date_of_birth=CADOB)
# clean column names (janitor)
install.packages("janitor")
library(janitor)
ideal1c<-ideal1b %>%
  clean_names()
install.packages("lubridate")
library(lubridate)
 ideal1d<-ideal1c%>%
   mutate(calf_date_of_birth=as.Date(calf_date_of_birth))
 filtered_kidera<-ideal1c%>%filter(sublocation=="Kidera") 
ideal2<-read_csv("https://raw.githubusercontent.com/ThumbiMwangi/R_sources/master/ideal2.csv")
Convert visitDate into date format
ideal2a<-ideal2%>%
  mutate(VisitDate=as.Date(VisitDate))

ideal3 <- ideal1%>%
  left_join(ideal2, by="CalfID")
ideal4<-ideal1%>%
  full_join(ideal2,by="CalfID")
# subset data using columns (select)
ideal3a<-ideal3%>%
  select(CalfID,VisitID,VisitDate,Theileria.spp.,ELISA_mutans,ELISA_parva)
# pivot (make data longer)
ideal3b<-ideal3a %>%
  pivot_longer(cols=c(Theileria.spp.,ELISA_mutans,ELISA_parva),
               (names_to = "tests",values_to = "outcome")
        # Grouping Data (group-by)
ideal3c<-ideal3%>%
          group_by(CalfID)%>%
        mutate(avrg_weight=mean(Weight,na.rm=TRUE)
jgymhtdogdemography<-read_csv("https://raw.githubusercontent.com/cema-uonbi/R_course/main/DogcohortDemographics.csv")
# Rename columns
dogdemography1<-dogdemography %>%
  mutate(rename Household_ID=Household_ID)

dogdemography1<-dogdemography%>%
  rename(HouseholdID=HousehldID)

dogdemography1<-dogdemography%>%
  rename(Interview_Date=IntDate)
dogdemography1<-dogdemography%>%
  rename(Interview_Date=IntDate, HouseholdID=HousehldID, VillageID=VillageID, Household_Members=HhMmbrs, Own_Dogs=OwnDogs,Num_Dogs_Owned=DgsOwnd,Adult_Dogs_Owned=AdltDgs,Puppies_Owned=Puppies,Dogs_Died_Past_Month=DogDied,Num_Dogs_Died_Past_Month=NumDd,Dos_Bitess_Past_Month=DogBite)
dogdemography1b<-dogdemography1c%>%
  mutate(recode(Own_Dogs, "0"="No", "1"="Yes"))
dogdemography1c<-dogdemography1%>%
  mutate(Interview_Date=as.Date(Interview_Date, format="%m/%d/%y"))
dogdemography1d<-dogdemography1c%>%
group_by(VillageID)%>%
  mutate(avrg_Household_Members=round(mean(Household_Members)))
IdealA<-read_csv("https://raw.githubusercontent.com/ThumbiMwangi/R_sources/master/ideal3a.csv")
# Summarise ReasonsLoss
table(IdealA$ReasonsLoss1)
library(tidyverse)
install.packages("ggplot2")
library(ggplot2)# plotting
# plot

ggplot(data=IdealA,aes(x=ReasonsLoss1))+
  geom_bar()+#specify type of graph
  theme_bw()+#change background theme
  labs(x="Calf status at the end of the Study",
       y="Number of calves",
       title = "Graph showing calf status"
    )
# get frequency(summaries)
calves_sublocation<-IdealA %>%
  select(sublocation)%>%
  group_by(sublocation)%>%
  summarise(frequency=n())
library(tidyverse)
#plot frequency
ggplot(calves_sublocation,aes(x=sublocation,y=frequency))+
  geom_col()+
  theme_bw()+
  labs(x="Sublocation",y="Frequency")+
  coord_flip()#flip our graph
#get exact number of calves per sublocation(by removing duplicates)
calves_sublocation<-IdealA%>%
  select(CalfID,sublocation)%>%
  distinct()%>% #remove duplicates
group_by(sublocation)%>%
summarise(number=n())
calves_gender<-IdealA%>%
  select(sublocation,CalfSex)%>%
  mutate(CalfSex=recode(CalfSex,"1"="Male", "2"="Female"))%>%
  group_by(sublocation,CalfSex)%>%
  summarise(frequency=n())
  
ggplot(calves_gender,aes(x=sublocation,y=frequency,fill=CalfSex))+
  geom_col()+
  theme_bw()+
  scale_fill_manual(values = c("#e41a1c","#4daf4a"))+
  labs(x="Sublocation",y="Frequency")+
  coord_flip()
Research<-read_csv("https://raw.githubusercontent.com/ThumbiMwangi/R_sources/master/ideal3a.csv")
Research1<-Research %>%
rename(Calf_ID=CalfID,Calf_Sex=CalfSex,sublocation=sublocation,Calf_Date_of_Birth)
Research1<-Research %>%
  rename("Calf_Date_of_Birth"="CADOB")%>%
  mutate("Calf_Date_of_Birth"=as.Date(Calf_Date_of_Birth, format(%D/ %M %Y))
         Research2<-Research1%>%
           mutate(Calf_Date_of_Birth=as.Date(Calf_Date_of_Birth, format="%m/%d/%y"))