library(data.table)
setwd("data/")
if(file.exists("emp_rent.csv")){
  unemployment <- fread("emp_rent.csv")
} else{
  url <- paste0("https://www.bls.gov/lau/laucnty", 11:19, ".txt")
  dest_files <- paste0("Unemployment_20", 11:19, ".txt")
  for(i in 1L:9L){
    download.file(url = url[i], destfile = dest_files[i])
  }
  
  k <- data.table(GeoName = c("Alabama", "Arizona", "Arkansas", "California", "Colorado", 
                              "Connecticut", "Delaware", "District of Columbia", "Florida", "Georgia", 
                              "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", 
                              "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan", 
                              "Minnesota", "Mississippi", "Missouri", "Montana", "Nebraska", 
                              "Nevada", "New Hampshire", "New Jersey", "New Mexico", "New York", 
                              "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Oregon", 
                              "Pennsylvania", "Rhode Island", "South Carolina", "South Dakota", 
                              "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington", 
                              "West Virginia", "Wisconsin", "Wyoming"),
                  county_state = c("AL", "AZ", "AR", "CA", "CO", "CT", "DE", "DC", "FL", "GA", "ID",
                             "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO",
                             "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR",
                             "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY"))
  cols <- readr::fwf_cols(LAUS_code = 15, code = 6, kode = 7, county_state = 53,
                          Year = 4, Labour_Force = 14, Employed = 13,
                          unemployed_level = 11, unemployment_rate = 9)
  for(i in 1L:9L){
    d <- readr::read_fwf(dest_files[i], skip = 6, col_positions = cols)
    d[c(1:3, 6, 7, 8)] <- NULL
    setDT(d)
    d[, (1) := unlist(lapply(strsplit(d$county_state, ', ', fixed = TRUE), '[', 2))]
    d[320, 1] <- "DC"
    k[d[, -2], as.character(unique(d$Year)) := i.unemployment_rate, on = "county_state"]
  }
  unemployment <- melt(k, id.vars = c("GeoName", "county_state"), measure.vars = as.character(2011:2019))
  rm(cols, d, k, dest_files, i, url);gc()
  
  ## Housing
  housing <- fread("rent_ppp.csv", skip = 4, drop = c(1, 3:5))
  housing <- melt(housing, id.vars = "GeoName", measure.vars = as.character(2011:2019))
  unemployment[housing, rent_ppp := i.value, on = c(GeoName = "GeoName",
                                                    variable = "variable")]
  fwrite(unemployment, "./emp_rent.csv")
}

ggplot(data = unemployment, aes(x = county_state, y = value, fill = variable))+
  geom_bar(stat = "identity")+
  labs(x = "State", y = "Unemployed")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5,
                                   size = 10, face = "bold"))
ggplot(data = unemployment, aes(x = county_state, y = rent_ppp, fill = variable))+
  geom_bar(stat = "identity")+
  labs(x = "State", y = "Unemployed")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5,
                                   size = 10, face = "bold"))

prices <- fread("./Price_ppp.csv", skip = 4)
