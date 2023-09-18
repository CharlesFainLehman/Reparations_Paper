library(tidyverse)
library(readxl)

lt <- read_excel("Death Estimates/White Life Tables 1860 - 2019.xlsx")

#helper function to query the life tables
get_qx <- function(sex, age, year) {
  qx_row <- lt %>%
    filter(Sex == sex,
           year >= Year_1 & year <= Year_2,
           age >= Age & age < Age + Increment)
 
 if (nrow(qx_row) == 0) {
   return(NA)
 } else {
    qx_row %>% mutate(qx = qx/Increment) %>% pull(qx) %>% return()
 }
}

#Building a table of all the values I want, and then get observed values for all of them that I can
qx_table <- data.frame()

print("Building sex-year-age table...")

#There might be a neater way to do this than a series of nested FOR loops, but this works!
for (sex in c("Male", "Female")) {
  for(year in 1860:2019) {
    for(age in 0:100) {
      qx_table <- bind_rows(qx_table, data.frame(Sex = sex, Year = year, Age = age))
    }
  }
}

print("adding observed qx values...")

qx_table <- qx_table %>%
  mutate(observed_qx = mapply(get_qx, Sex, Age, Year)) %>%
  #The Hacker tables hard stop at 80, so I want to simulate what should be there instead
  mutate(observed_qx = ifelse(Year < 1900 & Age >= 80, NA, observed_qx))

#Heatmap of observed values
ggplot(qx_table, aes(x=Year, y=Age)) + 
  geom_tile(aes(fill = observed_qx)) +
  facet_wrap(~Sex)

#I want age-specific LOESS models for qx ~ year, because if I do qx ~ year + age I end up with some wonky fits. So, I write a function to return a LOESS model.
qx_model <- function(qsex, qage) {
  qx_table %>%
    filter(Sex == qsex, Age == qage) %>%
    loess(data = ., formula = observed_qx ~ Year, na.action = na.omit) %>%
    return()
}

qx_loess <- loess(observed_qx ~ Year + Age, qx_table)

#I could probably do this in a Tidy fashion but it's a paint, so instead I do it the old fashioned way.
qx_hat <- numeric()

print("Getting model qx values...")
#nrow(qx_table)
for(row in 1:nrow(qx_table)) {
  qx_table_values <- qx_table[row,]
  qx_hat <- c(qx_hat, 
              predict(qx_model(qx_table_values$Sex, qx_table_values$Age), newdata = qx_table_values))
}

qx_table %>%
  mutate(qx_hat = qx_hat) -> qx_table_hat

#visualize interpolated values
ggplot(qx_table_hat, aes(x=Year, y=Age)) + 
  geom_tile(aes(fill = qx_hat)) +
  facet_wrap(~Sex)

#Okay, now let's backfill the values for the dropped Hacker ages/years
print("Creating model of missing Hacker values...")
hacker_model <- loess(qx_hat ~ Year + Age, data = qx_table_hat)

print("Backfilling missing Hacker values...")
qx_table_hat %>%
  filter(is.na(qx_hat)) %>%
  mutate(qx_hat_2 = predict(hacker_model, newdata = .)) %>%
  right_join(qx_table_hat, by = c('Sex', 'Year', 'Age', 'observed_qx', 'qx_hat')) %>% 
  mutate(qx_hat_2 = ifelse(is.na(qx_hat_2), qx_hat, qx_hat_2)) -> qx_table_hat_2

print("Done!")

#visualize predictions with implied Hacker rates
ggplot(qx_table_hat_2, aes(x=Year, y=Age)) + 
  geom_tile(aes(fill = qx_hat_2)) +
  facet_wrap(~Sex)

#three-panel version
qx_table_hat_2 %>%
  pivot_longer(cols = c(observed_qx, qx_hat, qx_hat_2)) %>%
  mutate(name = case_when(name == "observed_qx" ~ "Panel 1",
                          name == "qx_hat" ~ "Panel 2",
                          name == "qx_hat_2" ~ "Panel 3")) %>%
  ggplot(aes(x=Year, y=Age, fill = value)) +
  geom_tile() +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_fill_gradient(labels = scales::percent, breaks = c(0, .2, .4), high = 'white', low = '#2fa8ff') +
  facet_grid(rows = vars(Sex), cols = vars(name)) + 
  labs(fill = "Death Risk")

ggsave("img/plot3.png", width = 8, height = 5)

#check model fit
qx_table_hat_2 %>%
  filter(Age == 40) %>%
  ggplot(aes(x=Year)) +
  geom_point(aes(y=observed_qx)) + 
  geom_line(aes(y=qx_hat)) +
  facet_wrap(~Sex) + 
  scale_color_manual() +
  scale_y_continuous(labels = scales::percent) + 
  labs(y = "Death Risk") + 
  theme(axis.title.x = element_blank())

ggsave("img/plot4.png", width = 8, height = 5)

# finally, save the qx lookup table
qx_table_hat_2 %>%
  write.csv("Death Estimates/qx_lookup_table.csv", row.names = F)