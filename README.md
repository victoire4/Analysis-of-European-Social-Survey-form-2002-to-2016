# Analysis-of-European-Social-Survey-form-2002-to-2016
## Download data: 
From the ESS website, we download each wave sep-
arately from: http://www.europeansocialsurvey.org/data/round- index.html.
For country level data, we download the country level contextual data from 2016 from here: http://www.europeansocialsurvey.org/data/multilevel/ guide/bulk.html.
We downloaded the Stata files.
## Importing data: 
For this, we use the ”haven” package and one ”for loop”.
## Merging data: 
To do this, we select the variables we want from each wave and the country level. We use ”rbind()” to merge the wave data and ”full join()”.
## Cleaning data: 
We are checking variables to know the categorical variables and continuous variables. After, we are coding the categorical
variables as factor and coding too the continuous variables. And the end, we removed the missing values.
## Plotting graphs: 
We are plotting some graphs to answer our research questions.
