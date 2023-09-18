theme_set(theme_minimal())

print("Constructing death estimates..")
source("Death Estimates/construct qx table.R")
print("Constructing birth estimates...")
source("Births Estimates/Construct White Native-Born CBRs With Observed Pop.R")
print("Simulating population growth...")
source("Population Estimates/Advance 1860 Pop.R")