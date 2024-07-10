#packages
library(readr)
library(tidyr)
library(tidyverse)
library(dplyr)
library(ggplot2)
#========================================================================================================================
#importing data
customer_satisfaction<-read.csv("C:\\Users\\user\\Desktop\\Datasets\\CustomerSatisfaction.csv")
#variables
names(customer_satisfaction)
#=======================================================================================================================
#Exploratory Data Analysis
df<-customer_satisfaction %>%
  select_all() %>%
  rename("Baby_gender"=Baby.s.gender,"Baby_DOB"=Baby.s.date.of.birth,"Babys_weight"=Baby.s.weight..in.grams.,
         "Delivery_mode"=Mode.of.delivery,"DOA_at_NBU"=Date.of.admission.to.NBU,"DOD_at_NBU"=Date.of.discharge.from.NBU,"Mothers_Age"=Age.of.baby.s.mother,"Education_level"=Education.level,"Employment_status"=Employment.status,"Medical_insurance"=Do.you.have.a.medical.insurance.cover.,"Which_one"=If.yes..which.one.,"specify_others"=Other..specify.,"Born_at_KNH"=Was.the.baby.born.at.KNH.,"Current_accomodation_of_mother"=Where.is.the.baby.s.mother.accommodated.currently.,
         "Other_accomodation"=Other.accommodation..specify.,"Baby_safety_NBU"=Do.you.feel.your.baby.was.safe.in.NBU.,"Give_reasons"=Please.explain,"Lighting_corridors"=a..Lighting.on.the.corridors,"Incubator_space"=b..Incubator.space,"Baby_cot_space"=c...Baby.cot.space,"cleanliness_expressing_room"=d..Cleanliness.of.the.expressing.room,"privacy_expressing_room"=e..Privacy.in.the.expressing.room,"Baby_wrappers"=a..Baby.wrappers,
         "Mother_gowns"=b..Mother.gowns,"Expressing_bowls"=c..Expressing.bowls,"Washing_soap"=d..Hand.washing.soap,"Washing_basins"=e..Hand.washing.basins,"Hand_sanitizers"=f..Hand.sanitizers,"Shown_first_day"=a..Shown.around.the.unit.on.the.1st.day,
         "Explanation_given_baby"=b..Explanation.on.care.given.to.the.baby,"Responding_on_time"=c..Responding.on.time.when.called,"Explanation_after_discharge"=d..Explanation.on.care.after.discharge,"Courtesy"=e..Courtesy,"Baby_progress"=a..Explanation.on.condition..progress.of.the.baby,"Investigation_baby"=b..Explanation.on.investigations.carried.out.on.the.baby,"follow_discharge"=c..Information.on.follow.up.care.after.discharge,
         " Courtesy"=e..Courtesy,"Overall_care_NBU"=Overall.care.received.in.NBU,"Services_liked"=What.did.you.like.most.about.our.services,"services_to_improve"=Which.services.would.you.like.us.to.improve.on.,"Would_you_seek_services_in_future"=In.future..would.you.seek.health.care.services.in.this.hospital.,"Refer_friend"=Would.you.refer.a.friend.relative.to.Kenyatta.National.Hospital.,"Hand_sanitizers"=f..Hand.sanitizers) 

#=======================================================================================================================
#Removing unwanted columns
unwanted_col<-c("Record.ID","Research.Assistant.Initials","Complete.","Baby_DOB" )

df_filtered<-df %>%
  select(-unwanted_col)
#=======================================================================================================================
#checking the values of variable names
unique(df_filtered$Baby_gender)
unique(df_filtered$Babys_weight)
unique(df_filtered$Delivery_mode)
unique(df_filtered$Education_level)
unique(df_filtered$Medical_insurance)
unique(df_filtered$Employment_status)
unique(df_filtered$Which_one)
#formatting character to date
fomat<-"%d-%m-%Y"
df_filtered$DOA_at_NBU<-as.Date(df_filtered$DOA_at_NBU,format = fomat)
df_filtered$DOD_at_NBU<-as.Date(df_filtered$DOD_at_NBU,format = fomat)
class(df_filtered$DOA_at_NBU)
#Missing values
unwanted_dat<-c("DOA_at_NBU", "DOD_at_NBU" )
df_filtered_dat<-df_filtered %>%
  select(-unwanted_dat)
#drop missing values
dfdat<-df_filtered_dat %>%
  select_all() %>%
  filter(complete.cases(.)) %>%
  view()
#=======================================================================================================================
#data visualization
#change into factors
dfdat$Baby_gender=as_factor(dfdat$Baby_gender)
dfdat$Babys_weight=as_factor(dfdat$Babys_weight)
dfdat$Delivery_mode=as_factor(dfdat$Delivery_mode)
plotpie<-dfdat %>%
  select(Baby_gender,Babys_weight,Delivery_mode) %>%
  view()
str(plotpie)

#=======================================================================================================================
#plot pie chat for Gender of the Baby
my_table<-table(plotpie$Baby_gender)
pie(my_table, labels = paste(names (my_table),"\n", my_table,
                             sep =""), main = "A pie chart showing gender of the baby",
    col = c("green", "blue"))
legend("topright",c("Female","Male"), cex = 0.9,
       fill =c("green","blue"))
#Add percentages to pie chart
pie_percent<-paste0(round(100*prop.table(my_table),2), "%")
pie(my_table, labels = pie_percent, main = "A pie chart showing gender of the baby",
    col =c("green","blue"))
legend("topright", c("Female","male"), cex = 0.9,
       fill =c("green","blue"))
#=======================================================================================================================
##plot pie chat for Delivery Mode

my_table<-table(plotpie$Delivery_mode)
pie(my_table, labels = paste(names (my_table),"\n", my_table,
                             sep =""), main = "A pie chart showing Delivery Mode of the Mother",
    col = c("green", "blue"))
legend("bottomright",c("Vaginal delivery","Caeserian section"), cex = 0.9,
       fill =c("green","blue"))

#Add percentages to pie chart

pie_percent<-paste0(round(100*prop.table(my_table),2), "%")
pie(my_table, labels = pie_percent, main = "A pie chart showing Delivery Mode of the Mother",
    col =c("green","blue"))
legend("bottomright", c("Vaginal delivery","Caeserian section"), cex = 0.9,
       fill =c("green","blue"))
#=======================================================================================================================

#Bar Graph of Baby's weight
Table_weight<-table(plotpie$Babys_weight)
Table_weight_prop<-prop.table(Table_weight) #proportions
Table_weight_prop_percentages<-round(Table_weight_prop*100,2)

#Generate the Bar Plot
Bar_Babyweight <- barplot(Table_weight_prop_percentages,
              xlab = "Weight Category", ylab = "Percentage (%) Baby weight",
              main = "Bar plot of Baby weight",
              col = c("blue"), 
              beside = TRUE,
              ylim = c(0, 60))
#Display the percentages on the Bar Plot

text(Bar_Babyweight,0, Table_weight_prop_percentages, pos=3)
#======================================================================================================================
#plotting pie and bar graphs

dfdat$Education_level<-as_factor(dfdat$Education_level)
dfdat$Employment_status<-as_factor(dfdat$Employment_status)
dfdat$Medical_insurance<-as_factor(dfdat$Medical_insurance)
#creating data
pl<-dfdat %>%
  select(Education_level,Employment_status,Medical_insurance) %>%
  view()
str(pl)
#Medical insurance

my_table<-table(pl$Medical_insurance)
pie(my_table, labels = paste(names (my_table),"\n", my_table,
                             sep =""), main = "A pie chart showing Whether the baby's mother has medical insurance",
    col = c("green", "blue"))
legend("topright",c("Yes","No"), cex = 0.9,
       fill =c("green","blue"))

#Add percentages to pie chart

pie_percent<-paste0(round(100*prop.table(my_table),2), "%")
pie(my_table, labels = pie_percent, main = "A pie chart showing Whether the baby's mother has medical insurance",
    col =c("green","blue"))
legend("topright", c("Yes","No"), cex = 0.9,
       fill =c("green","blue"))
#======================================================================================================================

#Education Level(Bar graph)
Table_Edu<-table(pl$Education_level)
Table_Edu_prop<-prop.table(Table_Edu)
Table_Edu_prop_Per<-round(Table_Edu_prop*100,2)

Bar_Edu<-barplot(Table_Edu_prop_Per,xlab = "Mother's Level of Education",
                 ylab = "Percentage(%)",main = "Bar plot showing level of Education of Mothers",
                 col = "red",beside = TRUE,ylim =c(0,50) )
text(Bar_Edu,0,Table_Edu_prop_Per,pos = 3) #Display percentages on the bar chat
#======================================================================================================================
#Employment status
Table_Empl<-table(pl$Employment_status)
Table_Empl_prop<-prop.table(Table_Empl)
Table_Empl_per<-round(Table_Empl_prop*100,2)
 
Bar_Empl<-barplot(Table_Empl_per,xlab = "Mothers Employment Status",ylab = "Percentage(%)",
                  main = "Bar plot showing Mothers Employment Status",col="brown",beside = TRUE,ylim = c(0,50))
text(Bar_Empl,0,Table_Empl_per,pos=3)#display percentages
#===================================================================================================================
dfdat$Which_one<-as_factor(dfdat$Which_one)
dfdat$specify_others<-as_factor(dfdat$specify_others)
dfdat$Born_at_KNH<-as_factor(dfdat$Born_at_KNH)
dfdat$Current_accomodation_of_mother<-as_factor(dfdat$Current_accomodation_of_mother)
dfdat$Other_accomodation<-as_factor(dfdat$Other_accomodation)

dfwhich<-dfdat %>%
  select(Which_one,specify_others,Born_at_KNH,Current_accomodation_of_mother,Other_accomodation) %>%
  view()

Table_nhif<-table(dfwhich$Which_one)
Table_nhif_prop<-prop.table(Table_nhif)
Table_nhif_per<-round(Table_nhif_prop*100,2)

Bar_nhif<-barplot(Table_nhif,xlab = "Type of health insurance",ylab = "Percentage(%)",
                  main = "A bar graph showing type of Health insurance",col = "green",beside = TRUE,ylim = c(0,60))
text(Bar_nhif,0,Table_nhif_per,pos = 3)
#=====================================================================================================================
#Born at KNH
my_table<-table(dfwhich$Born_at_KNH)
pie(my_table, labels = paste(names (my_table),"\n", my_table,
                             sep =""), main = "A pie chart showing Baby's born at KNH",
    col = c("green", "blue"))
legend("topright",c("Yes","No"), cex = 0.9,
       fill =c("green","blue"))

#Add percentages to pie chart

pie_percent<-paste0(round(100*prop.table(my_table),2), "%")
pie(my_table, labels = pie_percent, main = "A pie chart showing Baby's born at KNH",
    col =c("green","blue"))
legend("topright", c("Yes","No"), cex = 0.9,
       fill =c("green","blue"))

#=======================================================================================================================
#current accommodation
Table_accom<-table(dfwhich$Current_accomodation_of_mother)
Table_accom_prop<-prop.table(Table_accom)
Table_accom_per<-round(Table_accom_prop*100,2)

Bar_accom<-barplot(Table_accom,xlab = "Room currently accomodated",ylab = "Percentage(%)",
                  main = "A bar graph Baby's current accomodation",col = "brown",beside = TRUE,ylim = c(0,60))
text(Bar_accom,0,Table_accom_per,pos = 3)
#converting into factors
dfdat$Baby_safety_NBU<-as_factor(dfdat$Baby_safety_NBU)
dfdat$Lighting_corridors<-as_factor(dfdat$Lighting_corridors)
dfdat$Incubator_space<-as_factor(dfdat$Incubator_space)
dfdat$Baby_cot_space<-as_factor(dfdat$Baby_cot_space)
dfdat$cleanliness_expressing_room<-as_factor(dfdat$cleanliness_expressing_room)
dfdat$privacy_expressing_room<-as_factor(dfdat$privacy_expressing_room)
dfdat$Baby_wrappers<-as_factor(dfdat$Baby_wrappers)
dfdat$Mother_gowns<-as_factor(dfdat$Mother_gowns)
dfdat$Expressing_bowls<-as_factor(dfdat$Expressing_bowls)
#=======================================================================================================================
#bar graphs
Table_saf<-table(dfdat$Baby_safety_NBU)
Table_saf_prop<-prop.table(Table_saf)
Table_saf_per<-round(Table_saf_prop*100,2)

Bar_safety<-barplot(Table_saf,xlab = "Room currently accomodated",ylab = "Percentage(%)",
                   main = "A bar graph Baby's current accomodation",col = "brown",beside = TRUE,ylim = c(0,60))
text(Bar_safety,0,Table_saf_per,pos = 3)





















