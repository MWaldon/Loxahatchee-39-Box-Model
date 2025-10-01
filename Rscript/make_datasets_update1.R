# Update datasets from 2009/07/01 to 2025
library(readxl)
# ET - evapotranspiration (mm/day)
  ET <- read_excel("DataSets/09_25/ET/ET.xlsx", 
                   sheet = "ET", range = "A1:C11079")
  names(ET) <- c('day', 'date', 'ET')

# Rain (mm/day)
  
  
# Structure Flows (m3/day)
  
  
# Stage (m)
  
  

