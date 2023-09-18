library(tidyverse)
library(readxl)

#observed white children of native-born parents
children <- read_excel("Births Estimates/White Births to Native-Born Parents, 1880 -2019.xlsx")
#observed native-born whites
population <- read_excel("Births Estimates/White Native-Born Population 1860 - 2019.xlsx")
#I need to use the synthetic survival rates to reverse survival back to birth
synthetic_qx_table <- read.csv("Death Estimates/qx_lookup_table.csv")

births <- children

print("Reverse survivaling to estimate native-born of native births...")
#reverse survival the observed white children to estimate the number of births
for(row in 1:nrow(births)) {
  while(births[row,]$Age > 0) {
    births[row,] <- births[row,] %>%
      mutate(Age = Age - 1,
             Year = Year - 1) %>%
      left_join(synthetic_qx_table, by = c("Age", "Sex", "Year")) %>%
      mutate(N = N/(1-qx_hat_2)) %>%
      select(-observed_qx, -qx_hat, -qx_hat_2)
  }
}

births <- rename(births, Births = N)

#visualize the survival-ing process
children %>%
  mutate(birth_year = Year - Age) %>%
  ggplot() +
  geom_line(aes(x=birth_year, y=N, linetype = "Survived", color = Source)) +
  geom_line(data = births, aes(x=Year, y=Births, linetype = "Born", color = Source)) + 
  scale_y_continuous(labels = scales::comma) + 
  facet_wrap(~Sex) + 
  theme(axis.title = element_blank()) + 
  labs(linetype = "")

ggsave("img/plot5.png", width = 8, height = 5)

print("Modeling native-born white population...")
#Interpolate the white native-born population
population_model <- loess(Population ~ Year, population, control=loess.control(surface="direct"))
synthetic_population <- data.frame(Year = 1860:2019) %>%
  mutate(Population = predict(population_model, newdata = .))

#visualize the interpolation aaginst observations
ggplot(synthetic_population, aes(x=Year, y=Population)) + 
  geom_line() +
  geom_point(data = population)

print("Constructing crude birth rates..")
#here's the estimates of crude birth rates!
CBR <- births %>%
  group_by(Year) %>%
  summarise(Births = sum(Births)) %>%
  left_join(synthetic_population) %>%
  mutate(CBR = Births/Population * 1000)

#write it out
write.csv(CBR, "Births Estimates/White_Native_Born_CBRs.csv", row.names = F)

#visualize it as compared to 
bh <- read.csv("Births Estimates/observed_CBRs.csv") %>%
  mutate(Source = ifelse(Year < 2005, "Haines 2008", "CDC")) 

CBR %>%
  ggplot(aes(x=Year, y=CBR), shape = 21) +
  geom_line() +
  geom_point(data = bh, aes(x=Year, y= White_CBR, fill = Source), shape = 21) + 
  theme(axis.title = element_blank())

ggsave("img/plot6.png", width = 8, height = 5)