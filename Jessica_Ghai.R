## 1st Consulting Project. Jessica Ghai, Animal Assisted Therapy for Children with Autism
#--- load lib's here, separate by "," you could add whatever you need here
pacman::p_load(tidyverse,reshape2)

# All the weird codes you might find are just me trying functions, Run the Highlighted code you will get the idea:
#
 
## Data Cleaning and EDA part
# ---Import Data, On the github you will need to download the two csvs and the xlsx file to make it work
NumericDataRaw = read_csv("Numeric.csv")
TextDataRaw = read_csv("ChoiceText.csv")

# Import Client specified IDs
ExcludedIDs = readxl::read_xlsx(path = "Need to be excluded IDs.xlsx",sheet = 3,col_names = TRUE,col_types = "text")%>%select(1)%>%pull()
print(paste0("Number of IDs Client would like to exclude: ",as.character(length(ExcludedIDs))))
## --- Import ends here


## --- Tag Original Dataset with ID valid status, Valid ID will be tagged "Valid" in the column:"ValidReponse", Invalid ones will be tagged "Invalid"
TextDataRaw = TextDataRaw%>%mutate(ValidResponse = if_else(ResponseId %in% ExcludedIDs,"Invalid","Valid"),ValidResponse = as.factor(ValidResponse),Q103 = as.factor(Q103))
NumericDataRaw = NumericDataRaw%>%mutate(ValidResponse = if_else(ResponseId %in% ExcludedIDs,"Invalid","Valid"),ValidResponse = as.factor(ValidResponse),Q103 = as.factor(Q103))

# Split them just in case
TextRawWithoutExIDs = filter(TextDataRaw,!ResponseId %in% ExcludedIDs)
TextRawofExIDs = filter(TextDataRaw,ResponseId %in% ExcludedIDs)
## --- Tag and split ends here

## --- below just some basic visuals
## try a bit on geographic locations
geo=select(TextDataRaw,Q103_1_x:Q103,ValidResponse)%>%slice(-(1:2))%>%mutate(Q103 = as.factor(Q103),Q103_1_x = as.numeric(Q103_1_x),Q103_1_y = as.numeric(Q103_1_y))

ggplot(geo)+aes(x = Q103_1_x,y = Q103_1_y,color = Q103)+geom_point()+facet_wrap(geo$ValidResponse)
# What's the map actually used in the survey?? 


## Check counts in each region
ggplot(geo) + aes(x = Q103) + geom_bar()+geom_text(stat='count', aes(label=..count..), vjust=-1)+facet_wrap(geo$ValidResponse)
# We have 76 NA values in geo questions. the population in this category is larger than Midwest and Southwest. Can we simply excluding them?

## --- Initial visual ends here



## Let's Start to actually see what the data looks like

## --- Seperate Demographics
# Seperate the Demographic part, setting them as adequate type
DemoText = select(TextDataRaw,Q10:Q34,ValidResponse,ResponseId)%>%slice(-(1:2))%>%mutate_at(.vars = c(1:4,8:9,12:13,16:23), .funs = as.factor)%>%mutate_at(.vars = 5:6, .funs = as.numeric)%>%mutate(Q13 = as.list(strsplit(Q13, ",")),Q18 = (as.list(strsplit(Q18, ","))))

# Combine "Day care" with"Daycare"
DemoText = DemoText%>%mutate(Q13_6_TEXT = if_else(DemoText$Q13_6_TEXT == "Daycare","Day care",DemoText$Q13_6_TEXT))

# Right now, two lists are in the result, need to separate them into columns

# --- testing part ignore plz
select(DemoText,Q18)%>%slice(1)%>%pull()
summary(DemoText)
DemoNum = select(NumericDataRaw,Q1:Q28,ValidResponse) %>% slice(-(1:2))
summary(DemoNum)
# --- testing ends

## --- Separation For Q13
Q13Text = select(DemoText,Q13,ValidResponse,ResponseId)%>%unnest(Q13)%>%spread(key = Q13,value = Q13)%>%mutate_at(3:9, ~replace(., !is.na(.), 1.))%>%mutate_at(3:9, ~replace(., is.na(.), 0.))
## --- Sep for Q13 ends here


## --- Similarily Separation for Q18
Q18Text = select(DemoText,Q18,ValidResponse,ResponseId)%>%unnest(Q18)%>%spread(key = Q18,value = Q18)%>%select(-3)%>%mutate_at(3:25, ~replace(., !is.na(.), 1.))%>%mutate_at(3:25, ~replace(., is.na(.), 0.))
# Bird's colname is buggy replace it
colnames(Q18Text)[colnames(Q18Text)=="Bird (i.e."] <- "Bird"
## --- Sep for Q18 ends here

## At this point, we will have DemoText as the demographic part cleaned. Q13Text and Q18Text as the suppliment table to those two questions.
## Be aware that a lot of the input had translated into factors, which depends on the graph you would like to plot, may results in some issues.
## Have fun!!

##########################################################################################
## For the last question
LastQText = select(TextDataRaw,Q32_1:Q32_4,ValidResponse,ResponseId)%>%slice(-(1:2))

Q32_1Text = select(LastQText,Q32_1,ValidResponse,ResponseId)%>%unnest(Q32_1)%>%spread(key = Q32_1,value = Q32_1)%>%mutate_at(3:8, ~replace(., !is.na(.), 1.))%>%mutate_at(3:8, ~replace(., is.na(.), 0.))
Q32_1Text = Q32_1Text[,c(1,2,6,3,5,4,7,8)]

Q32_2Text = select(LastQText,Q32_2,ValidResponse,ResponseId)%>%unnest(Q32_2)%>%spread(key = Q32_2,value = Q32_2)%>%mutate_at(3:8, ~replace(., !is.na(.), 1.))%>%mutate_at(3:8, ~replace(., is.na(.), 0.))
Q32_2Text = Q32_2Text[,c(1,2,6,3,5,4,7,8)]

Q32_3Text = select(LastQText,Q32_3,ValidResponse,ResponseId)%>%unnest(Q32_3)%>%spread(key = Q32_3,value = Q32_3)%>%mutate_at(3:8, ~replace(., !is.na(.), 1.))%>%mutate_at(3:8, ~replace(., is.na(.), 0.))
Q32_3Text = Q32_3Text[,c(1,2,6,3,5,4,7,8)]

Q32_4Text = select(LastQText,Q32_4,ValidResponse,ResponseId)%>%unnest(Q32_4)%>%spread(key = Q32_4,value = Q32_4)%>%mutate_at(3:8, ~replace(., !is.na(.), 1.))%>%mutate_at(3:8, ~replace(., is.na(.), 0.))
Q32_4Text = Q32_4Text[,c(1,2,6,3,5,4,7,8)]

## Counts for each response
## Q55a.1.I believe a majority of the target population outlined above would respond positively to therapeutic services
## (e.g., ABA services) that incorporate animals. 
LastQText$Q32_1 <- factor(LastQText$Q32_1,levels = c("Strongly Agree", "Agree", "Neither Agree or Disagree", "Disagree", "Strongly Disagree"))
ggplot(LastQText) + aes(Q32_1,fill = Q32_1)+geom_bar()+geom_text(stat='count', aes(label=..count..), vjust=-1)+facet_wrap(Q32_1Text$ValidResponse)

## 55b.2.I believe the presence of an animal can have a calming effect on many individuals within the target population outlined above
LastQText$Q32_2 <- factor(LastQText$Q32_2,levels = c("Strongly Agree", "Agree", "Neither Agree or Disagree", "Disagree", "Strongly Disagree"))
ggplot(LastQText) + aes(Q32_2,fill = Q32_2)+geom_bar()+geom_text(stat='count', aes(label=..count..), vjust=-1)+facet_wrap(Q32_2Text$ValidResponse)

## 55c.3.I believe the presence of an animal can increase the frequency of social interaction opportunities between individuals within the target 
## population outlined above and other people.
LastQText$Q32_3 <- factor(LastQText$Q32_3,levels = c("Strongly Agree", "Agree", "Neither Agree or Disagree", "Disagree", "Strongly Disagree"))
ggplot(LastQText) + aes(Q32_3,fill = Q32_3)+geom_bar()+geom_text(stat='count', aes(label=..count..), vjust=-1)+facet_wrap(Q32_3Text$ValidResponse)

## 55d.4.I believe animals can aid in effectively teaching social interaction and communication skills associated with ASD symptomatology
## for a majority of the target population outlined above. 
LastQText$Q32_4 <- factor(LastQText$Q32_4,levels = c("Strongly Agree", "Agree", "Neither Agree or Disagree", "Disagree", "Strongly Disagree"))
ggplot(LastQText) + aes(Q32_4,fill = Q32_4)+geom_bar()+geom_text(stat='count', aes(label=..count..), vjust=-1)+facet_wrap(Q32_4Text$ValidResponse)


## Some expoloring among the Last Question's responses
#codinganswers = function(x){
#  return(case_when(x=="Strongly Agree"~1.0,x=="Agree"~2.0,x=="Neither Agree or Disagree",3.0,x=="Disagree"~4.0,x=="Strongly Disagree"~5.0))}
#LastQTextNumeric <- LastQText%>%mutate(Q32_1 = case_when(Q32_1=="Strongly Agree"~1.0,
#                                                         Q32_1=="Agree"~2.0,
#                                                         Q32_1=="Neither Agree or Disagree",3.0,
#                                                         Q32_1=="Disagree"~4.0,
#                                                         Q32_1=="Strongly Disagree"~5.0))
unique(LastQText$Q32_1)
#Check if the factor is adequatly transfered into integers
LastQTextTEST <- LastQText%>%mutate_at(.var = 1:4,.funs = as.factor)%>%mutate_at(.var = 1:4,.funs = as.numeric)
## Calculate 
#meanLQValid <-  LastQTextTEST%>%filter(ValidResponse == "Valid")%>%select(1:4)%>%drop_na()%>%colMeans()
#sdLQValid <-  LastQTextTEST%>%filter(ValidResponse == "Valid")%>%select(1:4)%>%drop_na()%>%apply(2,sd)
#nLQValid <- LastQTextTEST%>%filter(ValidResponse == "Valid")%>%select(1:4)%>%drop_na()%>%apply(2,length)

#meanLQInvalid <-  LastQTextTEST%>%filter(ValidResponse == "Invalid")%>%select(1:4)%>%drop_na()%>%colMeans()
#sdLQInvalid <-  LastQTextTEST%>%filter(ValidResponse == "Invalid")%>%select(1:4)%>%drop_na()%>%apply(2,sd)                                                                                 
#nLQInvalid <- LastQTextTEST%>%filter(ValidResponse == "Invalid")%>%select(1:4)%>%drop_na()%>%apply(2,length)

LQvalid <-  LastQTextTEST%>%filter(ValidResponse == "Valid")%>%select(1:4)%>%drop_na()
LQInvalid<-  LastQTextTEST%>%filter(ValidResponse == "Invalid")%>%select(1:4)%>%drop_na()
## plot the hist 1st creating tables for plotting hists
LQvalid1 <- LQvalid %>% melt(measure.vars = 1:4)
ggplot(LQvalid1)+aes(x = value)+geom_histogram(bins = 10,aes(y=..density..))+facet_wrap(LQvalid1$variable,nrow = 2)
LQInvalid1 <- LQInvalid %>% melt(measure.vars = 1:4)
ggplot(LQInvalid1)+aes(x = value)+geom_histogram(bins = 10,aes(y=..density..))+facet_wrap(LQInvalid1$variable,nrow = 2)

## Q: The histogram does not show a normal distribution, can we still use t test? 
for(i in 1:4){
  print(t.test(select(LQInvalid,i)%>%pull(),select(LQvalid,i)%>%pull()))
}

## Judging from the t statistics, it seems that the id excluded were appeared to have high probability that they comes from the same population? Can we safely exclude them? Or does it matters if we exclude them or not?
