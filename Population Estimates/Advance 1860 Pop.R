library(tidyverse)

starting_pop <- read.csv("Population Estimates/1860_Non-Black_Population_Age_Sex.csv")
death_lookup <- read.csv("Death Estimates/qx_lookup_table.csv")
birth_lookup <- read.csv("Births Estimates/White_Native_Born_CBRs.csv")

#this is the helper function to advance a population table in a year by one year
survive_population <- function(year, population_table) {
  population_table %>%
    mutate(Year = year) %>%
    #merge in the qxs
    left_join(death_lookup, by = c("Age", "Sex", "Year")) %>%
    mutate(
      #kill off an age-period appropriate number
      Survived_Pop = N * (1 - qx_hat_2),
      #Increment age so that the living pop moves up in age
      Age = Age + 1
      ) %>%
    #drop any ages whose size have fallen below 1, or anyone over the age of 100
    filter(Survived_Pop > 1, Age <= 100) %>%
    select(-observed_qx, -qx_hat, -qx_hat_2, -Year, -N) %>%
    rename(N = Survived_Pop) %>%
    return()
}

#this is the helper function to add births to a population table in a given year
add_births <- function(year, population_table) {
  total_pop <- sum(population_table$N)
  CBR <- filter(birth_lookup, Year == year) %>% pull(CBR) 
  births = CBR * (total_pop/1000)
  #Birth ratio is taken to be 105 boys per 100 girls, as per 
  #https://ourworldindata.org/gender-ratio
  bind_rows(data.frame(Age = c(0, 0), Sex = c("Male", "Female"), N = c(births * (105/(100 + 105)), births * (1 - (105/(100 + 105))))),
            population_table) %>%
    return()
}

current_pop <- starting_pop
total <- data.frame()

print("Advancing population from 1860...")

for(year in 1860:2019) {
  print(year)
  total <- current_pop %>%
    mutate(Year = year) %>%
    bind_rows(total)
  
  survive_population(year, current_pop) %>%
    add_births(year, .) -> current_pop
}

#These are from IPUMS
# Steven Ruggles, Sarah Flood, Matthew Sobek, Danika Brockman, Grace Cooper,  Stephanie Richards, and Megan Schouweiler. IPUMS USA: Version 13.0 [dataset]. Minneapolis, MN: IPUMS, 2023. https://doi.org/10.18128/D010.V13.0 
actual_pop <- data.frame(Year = c(seq(1860, 2010, 10), 2019), Observed_Population = c(26865639, 33539519, 43598295, 55459038, 67503502, 82277192, 95521031, 111438606, 117659312, 137020806, 160399484, 180521300, 200142900, 218343277, 247017159, 270423196, 286196492))

nh_white_pop <- data.frame(Year = c(seq(1860, 2010, 10), 2019), NH_White_Population = c(26636505.90, 33245878.10, 43134772.70, 55101258, 66529129.80, 80950643.10, 93641964.70, 108470840.20, 115030830.00, 133167185.00, 153141905.00, 169486900.00, 180546700.00, 188072058.00, 194433424.00, 196931448.00, 196795251.00))

#visualize the synthetic vs. observed population
total %>%
  group_by(Year) %>%
  summarise(Population = sum(N)) %>%
  filter(!is.na(Year)) %>%
  left_join(actual_pop, by = "Year") %>%
  left_join(nh_white_pop, by = "Year") %>%
  ggplot(aes(x=Year, y=Population)) + 
  geom_line(aes(color = 'Simulated')) +
  geom_line(data = . %>% drop_na(), aes(y=NH_White_Population, color = "Actual N.H. White")) +
  geom_line(data = . %>% drop_na(), aes(y=Observed_Population, color = "Actual")) +
  geom_point(data = . %>% filter(Year %in% c(seq(1860, 2010, 10), 2019)),
             aes(color = "Simulated")) +
  geom_point(data = . %>% drop_na(),
             aes(y = NH_White_Population, color = "Actual N.H. White")) +
  geom_point(data = . %>% drop_na(),
             aes(y = Observed_Population, color = "Actual")) +
  geom_text(data = . %>% filter(Year %in% c(seq(1860, 2010, 10), 2019)),
             aes(color = "Simulated", y = Population - 7500000, 
                 label = paste(round(Population/1000000, 0.1), "M", sep = "")),
            size = 3, fontface = 'bold', show.legend = F) +
  geom_text(data = . %>% drop_na(),
            aes(color = "Actual", y = Observed_Population + 7500000, 
                label = paste(round(Observed_Population/1000000, 0.1), "M", sep = "")),
            size = 3, fontface = 'bold', show.legend = F) +
  geom_text(data = . %>% drop_na() %>% filter(Year >= 1890),
            aes(color = "Actual N.H. White", y = NH_White_Population - 7500000, 
                label = paste(round(NH_White_Population/1000000, 0.1), "M", sep = "")),
            size = 3, fontface = 'bold', show.legend = F) +
  scale_y_continuous(labels = scales::comma_format(suffix = "M", scale = 1/1000000)) + 
  theme(axis.title = element_blank(),
        legend.title = element_blank())

ggsave("img/plot7.png", width = 8, height = 5)