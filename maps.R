library(ggplot2)
library(spdep)
library(data.table)

data_full <- fread("./data/data_full.csv", drop = 4:52)
data_full[, (2) := as.character(Year)]
data_full[, In_breaks := cut(In_Mig, quantile(In_Mig, 0:4/4), labels = FALSE, include.lowest = TRUE), by = Year]
data_full[, Out_breaks := cut(Out_Mig, quantile(Out_Mig, 0:4/4), labels = FALSE, include.lowest = TRUE), by = Year]
abb <- data.frame(GeoName = c("Alabama", "Arizona", "Arkansas", "California", "Colorado", 
                              "Connecticut", "Delaware", "District of Columbia", "Florida", "Georgia", 
                              "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", 
                              "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan", 
                              "Minnesota", "Mississippi", "Missouri", "Montana", "Nebraska", 
                              "Nevada", "New Hampshire", "New Jersey", "New Mexico", "New York", 
                              "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Oregon", 
                              "Pennsylvania", "Rhode Island", "South Carolina", "South Dakota", 
                              "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington", 
                              "West Virginia", "Wisconsin", "Wyoming"),
                  GeoAbb = c("AL", "AZ", "AR", "CA", "CO", "CT", "DE", "DC", "FL", "GA", "ID",
                             "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO",
                             "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR",
                             "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY"))
shp_orig <- rgdal::readOGR("./data/shp/", "US", verbose = FALSE)
shp_orig@data$id <- 1:nrow(shp_orig@data)

for(i in 2011:2019){
  shp <- shp_orig
  shp@data <- merge(merge(shp@data, abb, by = "GeoName"), data_full[data_full$Year == i, c(1, 11, 12)],
                    by = "GeoName")
  shp@data <- shp@data[order(shp@data$id), ]
  shp_data <- fortify(shp, region = "id")
  shpdf <- merge(shp_data, shp@data, by = "id")
  
  p <- ggplot(data = shpdf, aes(x = long, y = lat, group = group, fill = In_breaks))+
    geom_polygon()+ 
    scale_color_gradient(breaks = 0:4/4)+
    geom_path(color = "white", size = 0.2)+
    labs(fill = "In-Migration Quantiles")+
    coord_equal()+
    theme(panel.background = element_blank(), legend.position = "right")+
    theme(axis.title = element_blank(), axis.text = element_blank(),
          axis.ticks = element_blank())+
    labs(title = paste0("In-Migration from US states to US states (", i, ")"))
  ggsave(paste0("In migration ", i, ".jpeg"), plot = p, width = 30, height = 16.875, units = "cm")
}

for(i in 2011:2019){
  shp <- shp_orig
  shp@data <- merge(merge(shp@data, abb, by = "GeoName"), data_full[data_full$Year == i, c(1, 12)],
                    by = "GeoName")
  shp@data <- shp@data[order(shp@data$id), ]
  shp_data <- fortify(shp, region = "id")
  shpdf <- merge(shp_data, shp@data, by = "id")
  
  p <- ggplot(data = shpdf, aes(x = long, y = lat, group = group, fill = Out_breaks))+
    geom_polygon()+ 
    scale_color_gradient(breaks = 0:4/4)+
    geom_path(color = "white", size = 0.2)+
    labs(fill = "Out-Migration Quantiles")+
    coord_equal()+
    theme(panel.background = element_blank(), legend.position = "right")+
    theme(axis.title = element_blank(), axis.text = element_blank(),
          axis.ticks = element_blank())+
    labs(title = paste0("Out-Migration from US states to US states (", i, ")"))
  ggsave(paste0("Out migration ", i, ".jpeg"), plot = p, width = 30, height = 16.875, units = "cm")
}

emp <- fread("./data/emp_rent.csv", drop = 2,
             col.names = c("GeoName", "Year", "unemployment_rate", "rent_ppp"),
             colClasses = c("character", "character", "character", "double", "double"))
data_full[, (10:12) := NULL]
data_full <- data_full[emp, on = c(GeoName = "GeoName", Year = "Year")]
data_full[, rent_ppp := (rent_ppp*100)/(rent_ppp[1])] # Alabama 2011 is the base
data_full[, unemp_rate_breaks := cut(unemployment_rate, quantile(unemployment_rate, 0:4/4), labels = FALSE, include.lowest = TRUE), by = Year]
data_full[, rent_ppp_breaks := cut(rent_ppp, quantile(rent_ppp, 0:4/4), labels = FALSE, include.lowest = TRUE), by = Year]

for(i in 2011:2019){
  shp <- shp_orig
  shp@data <- merge(merge(shp@data, abb, by = "GeoName"), data_full[data_full$Year == i, c(1, 12)],
                    by = "GeoName")
  shp@data <- shp@data[order(shp@data$id), ]
  shp_data <- fortify(shp, region = "id")
  shpdf <- merge(shp_data, shp@data, by = "id")
  
  p <- ggplot(data = shpdf, aes(x = long, y = lat, group = group, fill = unemp_rate_breaks))+
    geom_polygon()+ 
    scale_color_gradient(breaks = 0:4/4)+
    geom_path(color = "white", size = 0.2)+
    labs(fill = "Unemployment Rate Quantiles")+
    coord_equal()+
    theme(panel.background = element_blank(), legend.position = "right")+
    theme(axis.title = element_blank(), axis.text = element_blank(),
          axis.ticks = element_blank())+
    labs(title = paste0("Unemployment Rate in US states (", i, ")"))
  ggsave(paste0("Unemployment Rate ", i, ".jpeg"), plot = p, width = 30, height = 16.875, units = "cm")
}

for(i in 2011:2019){
  shp <- shp_orig
  shp@data <- merge(merge(shp@data, abb, by = "GeoName"), data_full[data_full$Year == i, c(1, 13)],
                    by = "GeoName")
  shp@data <- shp@data[order(shp@data$id), ]
  shp_data <- fortify(shp, region = "id")
  shpdf <- merge(shp_data, shp@data, by = "id")
  
  p <- ggplot(data = shpdf, aes(x = long, y = lat, group = group, fill = rent_ppp_breaks))+
    geom_polygon()+ 
    scale_color_gradient(breaks = 0:4/4)+
    geom_path(color = "white", size = 0.2)+
    labs(fill = "Rent PPP Quantiles")+
    coord_equal()+
    theme(panel.background = element_blank(), legend.position = "right")+
    theme(axis.title = element_blank(), axis.text = element_blank(),
          axis.ticks = element_blank())+
    labs(title = paste0("Rent PPP in US states (", i, ")"))
  ggsave(paste0("Rent PPP ", i, ".jpeg"), plot = p, width = 30, height = 16.875, units = "cm")
}

# Population
data_full[, cor(pop, In_Mig), by = GeoName]
shp <- shp_orig
shp@data <- merge(merge(shp@data, abb, by = "GeoName"), data_full[, cor(pop, In_Mig), by = GeoName],
                  by = "GeoName")
shp@data <- shp@data[order(shp@data$id), ]
shp_data <- fortify(shp, region = "id")
shpdf <- merge(shp_data, shp@data, by = "id")
ggplot(data = shpdf, aes(x = long, y = lat, group = group, fill = V1))+
  scale_fill_gradient2()+
  geom_polygon()+ 
  geom_path(color = "black", size = 0.2)+
  coord_equal()+
  theme(panel.background = element_blank(), legend.position = "right")+
  theme(axis.title = element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank())+
  labs(title = paste0("Correlation of Population with In-Migration in US states"))

# Per Capita income
data_full[, cor(per_inc, In_Mig), by = GeoName]
shp <- shp_orig
shp@data <- merge(merge(shp@data, abb, by = "GeoName"), data_full[, cor(per_cap_per_inc, In_Mig), by = GeoName],
                  by = "GeoName")
shp@data <- shp@data[order(shp@data$id), ]
shp_data <- fortify(shp, region = "id")
shpdf <- merge(shp_data, shp@data, by = "id")
ggplot(data = shpdf, aes(x = long, y = lat, group = group, fill = V1))+
  scale_fill_gradient2()+
  geom_polygon()+ 
  geom_path(color = "black", size = 0.2)+
  coord_equal()+
  theme(panel.background = element_blank(), legend.position = "right")+
  theme(axis.title = element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank())+
  labs(title = paste0("Correlation of Per capita income with In-Migration in US states"))

# Rent PPP
data_full[, cor(unemployment_rate, In_Mig), by = GeoName]
shp <- shp_orig
shp@data <- merge(merge(shp@data, abb, by = "GeoName"), data_full[, cor(unemployment_rate, In_Mig), by = GeoName],
                  by = "GeoName")
shp@data <- shp@data[order(shp@data$id), ]
shp_data <- fortify(shp, region = "id")
shpdf <- merge(shp_data, shp@data, by = "id")
ggplot(data = shpdf, aes(x = long, y = lat, group = group, fill = V1))+
  scale_fill_gradient2()+
  geom_polygon()+
  geom_path(color = "black", size = 0.2)+
  coord_equal()+
  theme(panel.background = element_blank(), legend.position = "right")+
  theme(axis.title = element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank())+
  labs(title = paste0("Correlation of Unemployment Rate with In-Migration in US states over 9 Years"))


data_full[, cor(rent_ppp, In_Mig), by = GeoName]
shp <- shp_orig
shp@data <- merge(merge(shp@data, abb, by = "GeoName"), data_full[, cor(rent_ppp, In_Mig), by = GeoName],
                  by = "GeoName")
shp@data <- shp@data[order(shp@data$id), ]
shp_data <- fortify(shp, region = "id")
shpdf <- merge(shp_data, shp@data, by = "id")
ggplot(data = shpdf, aes(x = long, y = lat, group = group, fill = V1))+
  scale_fill_gradient2()+
  geom_polygon()+
  geom_path(color = "black", size = 0.2)+
  coord_equal()+
  theme(panel.background = element_blank(), legend.position = "right")+
  theme(axis.title = element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank())+
  labs(title = paste0("Correlation of Rent PPP with In-Migration in US states over 9 Years"))

#### Out-migration

# Population
data_full[, cor(pop, Out_Mig), by = GeoName]
shp <- shp_orig
shp@data <- merge(merge(shp@data, abb, by = "GeoName"), data_full[, cor(pop, Out_Mig), by = GeoName],
                  by = "GeoName")
shp@data <- shp@data[order(shp@data$id), ]
shp_data <- fortify(shp, region = "id")
shpdf <- merge(shp_data, shp@data, by = "id")
ggplot(data = shpdf, aes(x = long, y = lat, group = group, fill = V1))+
  scale_fill_gradient2()+
  geom_polygon()+ 
  geom_path(color = "black", size = 0.2)+
  coord_equal()+
  theme(panel.background = element_blank(), legend.position = "right")+
  theme(axis.title = element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank())+
  labs(title = paste0("Correlation of Population with Out-Migration in US states"))

# Per Capita income
data_full[, cor(per_inc, Out_Mig), by = GeoName]
shp <- shp_orig
shp@data <- merge(merge(shp@data, abb, by = "GeoName"), data_full[, cor(per_cap_per_inc, Out_Mig), by = GeoName],
                  by = "GeoName")
shp@data <- shp@data[order(shp@data$id), ]
shp_data <- fortify(shp, region = "id")
shpdf <- merge(shp_data, shp@data, by = "id")
ggplot(data = shpdf, aes(x = long, y = lat, group = group, fill = V1))+
  scale_fill_gradient2()+
  geom_polygon()+ 
  geom_path(color = "black", size = 0.2)+
  coord_equal()+
  theme(panel.background = element_blank(), legend.position = "right")+
  theme(axis.title = element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank())+
  labs(title = paste0("Correlation of Per capita income with Out-Migration in US states"))

# Rent PPP
data_full[, cor(unemployment_rate, Out_Mig), by = GeoName]
shp <- shp_orig
shp@data <- merge(merge(shp@data, abb, by = "GeoName"), data_full[, cor(unemployment_rate, Out_Mig), by = GeoName],
                  by = "GeoName")
shp@data <- shp@data[order(shp@data$id), ]
shp_data <- fortify(shp, region = "id")
shpdf <- merge(shp_data, shp@data, by = "id")
ggplot(data = shpdf, aes(x = long, y = lat, group = group, fill = V1))+
  scale_fill_gradient2()+
  geom_polygon()+
  geom_path(color = "black", size = 0.2)+
  coord_equal()+
  theme(panel.background = element_blank(), legend.position = "right")+
  theme(axis.title = element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank())+
  labs(title = paste0("Correlation of Unemployment Rate with In-Migration in US states over 9 Years"))


data_full[, cor(rent_ppp, Out_Mig), by = GeoName]
shp <- shp_orig
shp@data <- merge(merge(shp@data, abb, by = "GeoName"), data_full[, cor(rent_ppp, Out_Mig), by = GeoName],
                  by = "GeoName")
shp@data <- shp@data[order(shp@data$id), ]
shp_data <- fortify(shp, region = "id")
shpdf <- merge(shp_data, shp@data, by = "id")
ggplot(data = shpdf, aes(x = long, y = lat, group = group, fill = V1))+
  scale_fill_gradient2()+
  geom_polygon()+
  geom_path(color = "black", size = 0.2)+
  coord_equal()+
  theme(panel.background = element_blank(), legend.position = "right")+
  theme(axis.title = element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank())+
  labs(title = paste0("Correlation of Rent PPP with In-Migration in US states over 9 Years"))

shp_orig@data <- cbind(shp_orig@data, coordinates(shp_orig))
data_full <- merge(data_full, shp_orig@data[, -2], by = "GeoName")
