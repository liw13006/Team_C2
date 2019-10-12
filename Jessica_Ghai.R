## 1st Consulting Project. Jessica Ghai, Animal Assisted Therapy for Children with Autism
#--- load lib's here, separate by "," you could add whatever you need here
pacman::p_load(tidyverse)

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
DemoText = select(TextDataRaw,Q10:Q28,ValidResponse,ResponseId)%>%slice(-(1:2))%>%mutate_at(.vars = c(1:4,8:9,12:13,16:21), .funs = as.factor)%>%mutate_at(.vars = 5:6, .funs = as.numeric)%>%mutate(Q13 = as.list(strsplit(Q13, ",")),Q18 = (as.list(strsplit(Q18, ","))))

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