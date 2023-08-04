filter(healthdata2, mood1 == "1", mood1 == "2", mood2 == "1",
       mood2 == "2", mood3 == "1", mood3 == "2", mood4 == "1",
       mood5 == "1", mood5 == "2", mood6 == "1", mood6 == "2",
       imputed_povertygroup == "3", imputed_povertygroup == "4")

mentalhealth <- select(data, mood1, mood2, 
                           mood3, mood4, mood5, mood6)
lowpov <- healthdata2 |> filter(imputed_povertygroup == "1")
highpov <- healthdata2 |> filter(imputed_povertygroup == "4")

d
library(tibble)
library(ggplot2)
add_column(healthdata2, before = lowpov)

filter(healthdata2, mood1 == "1") 
filter(healthdata2, insured == "1", insured == "2")
## Bar Plot ###
ggplot(data = healthdata2, aes(x = imputed_neighpovgroup4_1519, y = mood2, fill = imputed_neighpovgroup4_1519)) +
  geom_col(position = "fill") +
  labs(x = "Poverty Rates",
      y = "Nervousness")

ggplot(data = healthdata2, aes(x = imputed_neighpovgroup4_1519, y = mood6, fill = imputed_neighpovgroup4_1519)) +
  geom_col(stat = "stack") +
  labs(x = "Poverty Rates",
       y = "Worthless")
ggplot(data = healthdata2, aes(x = imputed_neighpovgroup4_1519, y = mood11, fill = imputed_neighpovgroup4_1519)) +
  geom_col(position = "fil") +
  scale_y_continuous(labels = scales::percent)

ggplot(healthdata2, aes(x = imputed_neighpovgroup4_1519, y = mood1, fill = mood11)) +
  geom_col()

pov2 <- healthdata2[sample(nrow(healthdata2$imputed_neighpovgroup4_1519 == 2), 1500),]
View(pov2)
pov2_new <- sample_n(pov2, 1500)

sample_n(healthdata2, 1100)

ggplot(data = healthdata2, aes(x = imputed_neighpovgroup4_1519, y = mood2, fill = imputed_neighpovgroup4_1519)) +
  geom_col(stat = "identity") +
  labs(x = "Poverty Rates",
       y = "Nervousness")
