#smoking <- select(data, everasthma, currentasthma20, firsttoldasthma, stillasthmall)

healthdata_new <- filter(data, wt21_dual_q1 != "NA", strata_q1 != "NA", 
                         age40new != "NA", age45up != "NA", age50up != "NA",
                         age18_64 != "NA", skiprxcost != "NA", 
                         toldprescription20 != "NA", takingmeds20 != "NA", checkedbp20_q1 != "NA", ageatdiabetes != "NA",
                         diabcntrlmeds != "NA", toohighblsugar != "NA", 
                         firsttoldasthma != "NA", workingac_q1 != "NA", helpneighbors20_q1 != "NA", everyday != "NA",
                         numberperdaya != "NA", cpd20a != "NA", 
                         heavysmoker20a != "NA", everydaycpda != "NA", mentholcigs20 != "NA", sourcelastcig != "NA", 
                         cost20cigarettes != "NA", cigpurchase20 != "NA",
                         cigarillo20_q1 != "NA", smokeecig12m20_q1 != "NA", smokeecig30days20_q1 != "NA", likedecigsflavs_q1 != "NA",
                         smokehookah12m_q1 != "NA", smellcigsmoke20_q1 != "NA", 
                         sexualid20 != "NA", bmi != "NA", whereflu20 != "NA", swim != "NA", evercolon20 != "NA", colonoscopy10yr20 != "NA",
                         condom20 != "NA", analsex != "NA", analsexcondomuse20 != "NA",
                         sexbehav_active20 != "NA", wsw != "NA", wswexclusive != "NA", everheardofprep != "NA", everusedprep20 != "NA", msm != "NA",
                         msmexclusive != "NA", bthcontrollastsex20_q1 != "NA",
                         condomusetrend != "NA", insultipv != "NA")


data$toohighblsugar <- as.numeric(gsub("\\00:00:0","", data$toohighblsugar))

data$toohighblsugar[is.na(data$toohighblsugar)]<-mean(data$toohighblsugar,na.rm=TRUE)

data$toohighblsugar <- round(data$toohighblsugar, digits = 1)

view(iris)

ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, color = Species)) +
  geom_point() +
  scale_color_manual(values = c("red", "pink", "purple"))



install.packages(ggplotly)

#comepare private, medicare and medicade in the insturction part

ggplot(data, aes(x = everasthma, y = Int_didntget20_data)) +
  geom_point() +
  theme_minimal()
