# Import all the necessary packages 
library(abind)
library(dplyr)
library(forcats)
library(ggplot2)
library(haven)
library(purrr)
library(stringr)
library(tibble)
library(tidyr)
library(tidyselect)

# Read the data : we create a function which in imput "i" (index of the 
# position of file in the folder), set the path, take the "dta" extension
# and read the files in the folder "data" which have the extension "dta".

Read_dta_data <-function(k){
  setwd("/media/mydata/courses/Data analysis/Assigment1
        /GbsR31K79_Data_Analysis_Using_R/data")
  data=list.files( pattern = "dta")
  read_dta(data[k])
}
# Import all the data and put them in a list : we create a for loop which 
# help us to import all our dataset and put them in a list

# empty list
list_data_year=list()

for (k in 1:9) {
  a <- Read_dta_data(k)
  list_data_year[[k]]<-a
}
# The choice of our variable for years

good_vars_1<-c("name","cntry","happy","vote","domicil","chldhm","pdwrk",
               "uemp3m","dsbldp","wkhtot")

# The first time, we use only the data about the year (it is because we 
#have "1:8"). We select the variable that we had choose.


small_list<-map(list_data_year[1:8],function(x){
  select(x,good_vars_1)
})

# Merge all the data of year

data_1<-reduce(small_list,rbind)


# The choice of our variable for country

good_vars_2<-c("cntry","c_gnipc_2012","c_hditr_2012")

# take only the variable that we had choose in the data about "country"

data_country<-select(list_data_year[[9]],good_vars_2)

# Checking if keys (cntry) are unique in table

data_country %>%
count(cntry) %>%
filter(n > 1)

# Join "data_1" with "data_country" by "cntry"

data_2<-(full_join(data_1,data_country, by = "cntry"))


# Change the name of variable "name" in "Years"

colnames(data_2)[1]<-"Years"

# Change the name of elements in "Years" by "2002, ..."
# We use a while loop

list_old_names<-list("ESS1e06_6", "ESS2e03_6", "ESS3e03_7", "ESS4e04_5",
                     "ESS5e03_4", "ESS6e02_4", "ESS7e02_2", "ESS8e02_1")
list_new_names<-list("2002", "2004","2006", "2008", "2010", "2012",
                     "2014", "2016")

i<-1
while (i < 9) {
  data_2$Years[data_2$Years==list_old_names[[i]]]<-list_new_names[[i]] 
  print(data_2)
  i<-i+1
}


# Checking variables to know the categorical variables

l2=list()
for (i in 1:12) {
  b<-attributes(data_2)$names
  l2<-table(data_2[[b[[i]]]])
  print(l2)
}

# Coding the categorical variable as factor
# We have 9 categorical variable but, we are coding only 7 because 
# the variable "Years" and "cntry" have already the code.

# The first time, we put all the label of each variable in a vector

happyVec<-c("level0","level1","level2","level3","level4","level5",
            "level6","level7","level8","level9","level10")
voteVec<-c("Yes","No","Not eligible to vote")
domicilVec<-c("A big city","The suburbs or outskirts of a big city",
              "A town or a small city","A country village",
              "A farm or home in the countryside")
chldhmVec<-c("Yes","No")
pdwrkVec<-c("No","Yes")
uemp3mVec<-c("Yes","No")
dsbldpVec<-c("No","Yes")

# We put all those vector in one list and we use "for loop" to coding 
# those variable as factor

list_labels<- list(happyVec,voteVec,domicilVec,chldhmVec,pdwrkVec,
                   uemp3mVec,dsbldpVec)
for (j in 1:7) {
  data_2[[b[[j+2]]]] <- factor(data_2[[b[[j+2]]]], labels = list_labels[[j]])
}
data_2$Years <- factor(data_2$Years)
data_2$cntry <- factor(data_2$cntry)
# Output data_2

data_2

# Coding continuous variables 

qplot(data_2$wkhtot)
qplot(data_2$c_gnipc_2012)
qplot(data_2$c_hditr_2012)


# Summary

summary(data_2)

# Remove the missing values

data_2<-data_2%>%filter(complete.cases(.))

# Output data_2

data_2

# Barchart of "happy"
       
ggplot(data_2, aes(happy)) + geom_bar()

#Pie of "vote"

ggplot(data=data_2,mapping = aes(fill=vote,x=""))+geom_bar(width = 1)+
  theme_grey()+coord_polar(theta = "y")

# Comparing "Years" and "happy" using “dodge”

ggplot(data_2, aes(Years, fill = happy)) + geom_bar(position = "dodge")

ggplot(data_2, aes(Years, fill = cntry)) + geom_bar(position = "dodge")


# Comparing "Years" and "vote" using “dodge”

ggplot(data_2, aes(Years, fill = vote)) + geom_bar(position = "dodge")

# Comparing "happy" and "pdwrk" using “dodge”

ggplot(data_2, aes(happy, fill = pdwrk)) + geom_bar(position = "dodge")


# Comparing "happy" and "pdwrk" using “dodge”

ggplot(data_2, aes(happy, fill = dsbldp)) + geom_bar(position = "dodge")

# Comparing "happy" and "chldhm" using “dodge”

ggplot(data_2, aes(happy, fill = chldhm)) + geom_bar(position = "dodge")

# Comparing "vote" and "chldhm" using “dodge”

ggplot(data_2, aes(vote, fill = uemp3m)) + geom_bar(position = "dodge")

# Comparing proportions of vote per years 

ggplot(data_2, aes(Years, fill = vote)) + geom_bar(position = "fill")

# Comparing proportions of vote per cntry 

ggplot(data_2, aes(cntry, fill = vote)) + geom_bar(position = "fill")

# Comparing proportions of vote per cntry

ggplot(data_2, aes(cntry, fill = happy)) + geom_bar(position = "fill")

# "happy" and "c_gnipc" using Density plot 

ggplot(data=data_2,mapping = aes(x=c_gnipc_2012,colour=happy))+
  geom_density(mapping = aes(y=stat(count)))+theme_grey()

# "happy" and "c_gnipc" using Density plot 

ggplot(data=data_2,mapping = aes(x=c_gnipc_2012,colour=happy))+
  geom_density(mapping = aes(y=stat(count)))+theme_grey()

ggplot(data=data_2,mapping = aes(x=c_gnipc_2012,colour=cntry))+
  geom_density(mapping = aes(y=stat(count)))+theme_grey()

# "happy" and "c_gnipc" using Density plot 

ggplot(data=data_2,mapping = aes(x=c_hditr_2012,colour=vote))+
  geom_density(mapping = aes(y=stat(count)))+theme_grey()

ggplot(data=data_2,mapping = aes(x=c_hditr_2012,colour=cntry))+
  geom_density(mapping = aes(y=stat(count)))+theme_grey()

# "happy" and "dsbldp" using Bar Chat

ggplot(data=data_2,mapping = aes(x=dsbldp,fill=happy))+geom_bar()+
  theme_grey()+theme(axis.text.x = element_text())+
  facet_wrap(facets = ~happy,dir = "h")

# "happy" and "wkhtot" using Boxplot

ggplot(data_2, aes(reorder(happy, wkhtot, FUN = median), wkhtot)) +
  geom_boxplot()
