library(ggplot2)

# Logistic Regression
ggplot(test, aes(x = pred_lp, y = test$fluvaccineshot, color = pred_lp)) +
  geom_jitter() + labs(x = "Predicted",
                       y = "Observed",
                       title = "Logistic Regression")

# Boosted Tree
ggplot(test, aes(x = pred_bp, y = test$fluvaccineshot, color = pred_bp)) +
  geom_jitter() + labs(x = "Predicted",
                      y = "Observed",
                      title = "Boosted Tree")

# Random Forest
ggplot(test, aes(x = pred_fp, y = test$fluvaccineshot, color = pred_fp)) +
  geom_jitter() + labs(x = "Predicted",
                       y = "Observed",
                       title = "Random Forest")


df_accuracy <- data.frame(n_comp=c('Logistic Regression','Boosted Tree','Random Forest',
                                   'KNN','Support Vector Machine'),
                          no_vaccine=c(0.579339,0.54739,0.5569755,0.5271565,0.5750798),
                          yes_vaccine=c(0.69138,0.68582,0.658943,0.6997219,0.67933271),
                          stringsAsFactors = F)

df_accuracy <- df_accuracy %>% pivot_longer(cols = -n_comp)

library(RColorBrewer)
coul <- brewer.pal(10, "Set2") 

ggplot(df_accuracy,aes(x=name,y=value,fill=n_comp,
                       label=paste0(100*round(value,3),'%')))+
  geom_bar(stat='identity',position = 'dodge')+
  geom_text(position = position_dodge(0.9),vjust=-0.25)+
  scale_fill_manual(values=c("#FAF0D7",
                             "#D4E2D4",
                             "#FFCACC",
                             "#DBC4F0",
                             "#8CC0DE")) +
  scale_linetype_manual(values=lines, name="Indices") + theme_bw() +
  labs(x='Vaccine Receival', y='Accuracy',title="Comparing model accuracies for predicting flu vaccine receival") +
  guides(fill=guide_legend(title="Model Type"))




# variety of dark colors
# values=c("#D27685",
#          "#F9E2AF",
#          "#210062",
#          "#009FBD",
#          "#77037B")

# shades of blue
# values=c("#073748",
#          "#0B666A",
#          "#35A29F",
#          "#64CCC5",
#          "#DAFFFB")
