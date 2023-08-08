
#Pie Chart

#everasthma
Tableever <- table(clean_asthma$everasthma)
row.names(Tableever) <- c("Yes", "No")
ever_distribution <- round(Tableever/8781*100)
names <- paste(names(Tableever), "-", ever_distribution, "%", sep = " ")

pie(Tableever, main = "Have you ever been told by a
doctor, nurse or other health
professional that you had asthma?", col = c(4,2), labels = names)

pie3D(Tableever, main = "Have you ever been told by a
doctor, nurse or other health
professional that you had asthma?", labels = names, col = c("lightpink","cornsilk"), border = "white", radius = 1 )


#currentasthma20
Tablecurrent <- table(clean_asthma$currentasthma20)
row.names(Tablecurrent) <- c("Yes", "No")
current_distribution <- round(Tablecurrent/8781*100)
names <- paste(names(Tablecurrent), "-", current_distribution, "%", sep = " ")


pie(Tablecurrent, main = "In the last 12 months, have you
had an episode of asthma or an
asthma attack?", col = c("red","blue"), labels = names)

pie3D(Tablecurrent, main = "In the last 12 months, have you
had an episode of asthma or an
asthma attack?", labels = names, col = c("lightpink","cornsilk"), border = "white", radius = 1 )



#stillasthmaall
Tablestill <- table(clean_asthma$stillasthmaall)
row.names(Tablestill) <- c("Yes", "No")
still_distribution <- round(Tablestill/8781*100)
names <- paste(names(Tablestill), "-", still_distribution, "%", sep = " ")



library(plotrix)
library(ggpubr)
pie(Tablestill, main = "Do you still have asthma? Among
all adults.", col = c("red","blue"), labels = names)

pie3D(Tablestill, main = "Do you still have asthma? Among
all adults.", labels = names, col = c("lightpink","cornsilk"), border = "white", radius = 1 )



#18-24 year olds everasthma

envi_asthma <- select(data, agegroup6, birthsex, newrace6_b, usborn,
                      maritalstatus20, education, child, employment20,
                      emp3, imputed_povertygroup, imputed_povgroup3, imputed_neighpovgroup4_1519,
                      everasthma, currentasthma20, stillasthmaall, rodentsstreet, 
                      helpneighbors20_q1, discussissues, helpcommproj, trustkeys)

clean_envi_asthma <- na.omit(envi_asthma)

age18_24_yes_asthma <- clean_envi_asthma|> filter(agegroup6 == "1")

#table for the pie chart

table18_24ever <- table(age18_24_yes_asthma$everasthma)
row.names(table18_24ever) <- c("Yes", "No")
ever18_24_distribution <- round(table18_24ever/305*100)
names <- paste(names(table18_24ever), "-", ever18_24_distribution, "%", sep = " ")


pie3D(table18_24ever, main = "Have you ever been told by a
doctor, nurse or other health
professional that you had asthma? Among
18-24 year old.", labels = names, col = c("lightpink","cornsilk"), border = "white", radius = 1 )


#table for pie chart comparing foreign born and everasthma

#filter for foreign born surveyors 
foreignasthma <- clean_envi_asthma|> filter(usborn == "2")

tablefborn_ever <- table(foreignasthma$everasthma)
row.names(tablefborn_ever) <- c("Yes", "No")
tablefborn_ever_distribution <- round(tablefborn_ever/1665*100)
names <- paste(names(tablefborn_ever), "-", tablefborn_ever_distribution, "%", sep = " ")


pie3D(tablefborn_ever, main = "Have you ever been told by a
doctor, nurse or other health
professional that you had asthma? Among
foreign.", labels = names, col = c("lightpink","cornsilk"), border = "white", radius = 1 )


#table for pie chart comparing usborn and everasthma

#filter for foreign born surveyors 
usasthma <- clean_envi_asthma|> filter(usborn == "1")

tableusborn_ever <- table(usasthma$everasthma)
row.names(tableusborn_ever) <- c("Yes", "No")
tableusborn_ever_distribution <- round(tableusborn_ever/2123*100)
names <- paste(names(tableusborn_ever), "-", tableusborn_ever_distribution, "%", sep = " ")


pie3D(tablefborn_ever, main = "Have you ever been told by a
doctor, nurse or other health professional 
that you had asthma? Asked among surveyors 
born in the U.S", labels = names, col = c("lightpink","cornsilk"), border = "white", radius = 1 )











