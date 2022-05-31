library(data.table)

if(file.exists("./data/data_full.csv")){
  data_full <- fread("./data/data_full.csv")
} else {
  data <- readxl::read_xlsx("./data/Book1.xlsx",
                            col_types = c("text", rep("numeric", times = 113)))
  setDT(data)
  colnames(data)[1:2] <- c("GeoName", "In_Mig")
  # removing the moe estimates
  data[, colnames(data)[seq(from = 3, to = 113, by = 2)]:=NULL]
  data[, (54:57) := NULL]
  
  emp <- fread("data/emp.csv", drop = c("GeoFips", "LineCode"))
  rm_emp <- seq(from = 1, to = 360, by = 6)
  rm_emp2 <- seq(from = 5, to = 360, by = 6)
  rm_emp <- c(rm_emp,rm_emp2)
  emp <- emp[-rm_emp, ]
  emp <- dcast(melt(setDT(emp), measure.vars = as.character(2011:2019)),
               GeoName+variable~Description, value.var = "value")
  colnames(emp) <- c("GeoName", "Year", "per_cap_per_inc", "per_inc", "pop",
                     "Tot_emp")
  emp[, Year := as.numeric(as.character(Year))]
  data_full <- merge.data.table(data, emp, by = c("GeoName", "Year"))
  data_full[, (2) := as.character(Year)]
  data_full[, (c(3:55, 57, 58)) := lapply(.SD, as.integer),
            .SDcols = c(3:55, 57, 58)]
  data_full[, (3) := In_Mig - Alaska - Hawaii]
  data_full[, (c(5, 15)) := NULL]
  
  #[, .(Year = Year, GeoName = GeoName, Out_Mig = V1)]
  out <- melt(data_full, id.vars = "Year", measure.vars = colnames(data_full)[4:52])[, sum(value), .(Year, GeoName=variable)]
  setnames(out, "V1", "Out_Mig")
  # Merging the out-migration with whole data
  data_full <- merge(data_full, out, by = c("GeoName", "Year"))
  data_full[, Net_Mig := In_Mig - Out_Mig]
  state_abb <- c("AL", "AZ", "AR","CA","CO","CT","DE","DC","FL","GA",
                 "ID","IL","IN","IA","KS","KY","LA","ME","MD","MA","MI","MN","MS",
                 "MO","MT","NE","NV","NH","NJ","NM","NY","NC","ND","OH","OK","OR",
                 "PA","RI","SC","SD","TN","TX","UT","VT","VA","WA","WV","WI","WY")
  data_full[, GeoAbb := rep(state_abb, each = 9)]
  rm(emp, out, data, rm_emp, rm_emp2)
  gc()
  fwrite(data_full, "./data/data_full.csv")
}
data_full[, (2) := as.character(Year)]
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

library(ggplot2)
ggplot(data = data_full, aes(x = GeoAbb, y = In_Mig, fill = Year))+
  geom_bar(stat = "identity")+
  labs(x = "State", y = "In-Migration")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5,
                                   size = 10, face = "bold"))
ggplot(data = data_full, aes(x = GeoAbb, y = Out_Mig, fill = Year))+
  geom_bar(stat = "identity")+
  labs(x = "State", y = "Out-Migration")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5,
                                   size = 10, face = "bold"))

ggplot(data = data_full, aes(x = GeoAbb, y = Net_Mig, fill = Year))+
  geom_bar(stat = "identity")+
  labs(x = "State", y = "Net Migration")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5,
                                   size = 10, face = "bold"))


library(spdep)
shp <- rgdal::readOGR("./data/shp/", "US", verbose = FALSE)
shp@data$id <- 1:nrow(shp@data)
shp@data <- merge(merge(shp@data, abb, by = "GeoName"), data_full[data_full$Year == 2011, c(1, 3, 57, 58)],
                  by = "GeoName")
shp@data$In_breaks <- factor(.bincode(shp@data$In_Mig,
                               breaks = quantile(shp@data$In_Mig, 0:4/4),
                               include.lowest = TRUE),
                             labels = c("[19740, 55572)", "[55572, 107899)",
                                        "[107899, 189097)", "[189097, Inf)"))
shp@data$Out_breaks <- factor(.bincode(shp@data$Out_Mig,
                               breaks = quantile(shp@data$Out_Mig, 0:4/4),
                               include.lowest = TRUE),
                              labels = c("[18104, 57244)", "[57244, 105200)",
                                         "[105200, 184431)", "[184431, Inf)"))
shp@data$Net_breaks <- factor(.bincode(shp@data$Net_Mig,
                               breaks = quantile(shp@data$Net_Mig, 0:4/4),
                               include.lowest = TRUE),
                              labels = c("[-97866, -6764)", "[-6764, 782)",
                                         "[782, 12825)", "[12825, Inf)"))
shp@data <- shp@data[order(shp@data$id), ]
shp@data[, 10:11] <- as.data.frame(coordinates(shp))
shp_data <- fortify(shp, region = "id")
shpdf <- merge(shp_data, shp@data, by = "id")

ggplot(data = shpdf, aes(x = long, y = lat, group = group, fill = In_breaks))+
  geom_polygon()+
  geom_path(color = "white", size = 0.2)
  

ggplot(data = shpdf, aes(x = long, y = lat, group = group, fill = In_breaks))+
  geom_polygon()+ 
  scale_color_gradient(breaks = 0:4/4)+
  geom_path(color = "white", size = 0.2)+
  labs(fill = "In-Migration
Quantiles")+
  coord_equal()+
  theme(panel.background = element_blank(), legend.position = "right")+
  theme(axis.title = element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank())+
  labs(title = "In-Migration from US states to US states (2011)")

ggplot(data = shpdf, aes(x = long, y = lat, group = group, fill = Out_breaks))+
  geom_polygon()+
  geom_path(color = "white", size = 0.2)+
  scale_color_gradient(breaks = 0:4/4)+
  labs(fill = "Out-Migration
Quantiles")+
  geom_text(aes(label = GeoAbb, x = V1, y = V2), color = "black")+
  coord_equal()+
  theme(panel.background = element_blank(), legend.position = "right")+
  theme(axis.title = element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank())+
  labs(title = "Out-Migration from US states to US states (2011)")

ggplot(data = shpdf, aes(x = long, y = lat, group = group, fill = Net_breaks))+
  geom_polygon()+
  geom_path(color = "white", size = 0.2)+
  scale_color_gradient(breaks = 1:4/4)+
  geom_text(aes(label = GeoAbb, x = V1, y = V2), color = "black ")+
  labs(fill = "Net-Migration
Quantiles")+
  coord_equal()+
  theme(panel.background = element_blank(), legend.position = "right")+
  theme(axis.title = element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank())+
  labs(title = "Net-Migration from US states to US states (2011)")
rm(shp_data);gc()

library(geofacet)
my_us_grid <- us_state_grid1[c(-2, -11), ]
dat_plot <- data_full[, c(1:3, 57:58)]
dat_plot <- melt(dat_plot, id.vars = c("GeoName", "Year"),
                 measure.vars = c("In_Mig", "Out_Mig", "Net_Mig"))

ggplot(dat_plot, aes(group = variable, x = Year, color = variable))+
  geom_line(aes(y = value), show.legend = TRUE)+
  scale_color_manual(labels = c("In-Migration", "Out-Migration", "Net-Migration"),
                     values = c("red", "green", "blue"))+
  scale_y_continuous(labels = function(x) format(x, scientific = TRUE))+
  facet_geo(~ GeoName, scales = "free_y", grid = my_us_grid)+
  scale_x_discrete(breaks = seq(2011, 2019, length.out = 5),
                   labels = c("'11", "'13", "'15", "'17", "'19"))+
  labs(x = "Year", y = "Number of people",
       title = "Different types of migration in US states (2011-2019)",
       caption = "Data source: US Bureau of Economic Analysis, US Census Bureau")

ggplot(dat_plot, aes(group = variable, x = Year, color = variable))+
  geom_line(aes(y = value), show.legend = TRUE)+
  scale_color_manual(labels = c("In-Migration", "Out-Migration", "Net-Migration"),
                     values = c("red", "green", "blue"))+
  facet_geo(~ GeoName, grid = my_us_grid)+
  scale_x_discrete(breaks = seq(2011, 2019, length.out = 5),
                   labels = c("'11", "'13", "'15", "'17", "'19"))+
  labs(x = "Year", y = "Number of people",
       title = "Different types of migration in US states (2011-2019)",
       caption = "Data source: US Bureau of Economic Analysis, US Census Bureau")
data_full[, empvspop := Tot_emp/pop]

ggplot(data_full, aes(group = GeoName, x = Year))+
  geom_line(aes(y = empvspop), show.legend = TRUE)+
  facet_geo(~ GeoName, grid = my_us_grid)+
  scale_x_discrete(breaks = seq(2011, 2019, length.out = 5),
                   labels = c("'11", "'13", "'15", "'17", "'19"))+
  labs(x = "Year", y = "Ratio",
       title = "Employment to Population Ratio",
       caption = "Data source: US Bureau of Economic Analysis, US Census Bureau")

# clear plots
dev.off(dev.list()["RStudioGD"])
rm(abb, dat_plot, shp_data, shpdf);gc()

dat_plot <- data_full[, c(1:2, 55:56)]
dat_plot <- melt(dat_plot, id.vars = c("GeoName", "Year"),
                 measure.vars = c("pop", "Tot_emp"))
ggplot(dat_plot, aes(group = variable, x = Year, color = variable))+
  geom_line(aes(y = value), show.legend = TRUE)+
  scale_y_continuous(labels = function(x) format(x, scientific = TRUE))+
  scale_color_manual(labels = c("Population", "Employment"),
                     values = c("red", "green"))+
  facet_geo(~ GeoName, scales = "free_y", grid = my_us_grid)+
  scale_x_discrete(breaks = seq(2011, 2019, length.out = 5),
                   labels = c("'11", "'13", "'15", "'17", "'19"))+
  labs(x = "Year", y = "Number of people",
       title = "Population vs Employment (2011-2019)",
       caption = "Data source: US Bureau of Economic Analysis, US Census Bureau")

ggplot(dat_plot, aes(group = variable, x = Year, color = variable))+
  geom_line(aes(y = value), show.legend = TRUE)+
  scale_color_manual(labels = c("Population", "Employment"),
                     values = c("red", "green"))+
  facet_geo(~ GeoName, grid = my_us_grid)+
  scale_x_discrete(breaks = seq(2011, 2019, length.out = 5),
                   labels = c("'11", "'13", "'15", "'17", "'19"))+
  labs(x = "Year", y = "Number of people",
       title = "Population vs Employment (2011-2019)",
       caption = "Data source: US Bureau of Economic Analysis, US Census Bureau")

detach(package:ggplot2)
detach(package:geofacet)

shp@data[, 4:9] <- NULL

#mean of variables
data_full[, c(2, 3, 53, 55, 56)][, lapply(.SD, mean), by = "Year", .SDcols = 2:5]

data_full[, c(2, 3, 53, 55, 56)][, lapply(.SD, quantile, probs = 0.25), by = "Year", .SDcols = 2:5]
data_full[, c(2, 3, 53, 55, 56)][, lapply(.SD, quantile, probs = 0.5), by = "Year", .SDcols = 2:5]
data_full[, c(2, 3, 53, 55, 56)][, lapply(.SD, quantile, probs = 0.75), by = "Year", .SDcols = 2:5]
data_full[, c(2, 3, 53, 55, 56)][, lapply(.SD, quantile, probs = 1), by = "Year", .SDcols = 2:5]

data_max <- data_full[, c(1, 2, 3)][, .SD[which.max(In_Mig)], by = Year]
data_max <- merge(data_max, data_full[, c(1, 2, 57)]
                  [, .SD[which.max(Out_Mig)],by = Year], by = "Year")
data_max <- merge(data_max, data_full[, c(1, 2, 58)]
                  [, .SD[which.max(Net_Mig)], by = Year], by = "Year")

data_min <- data_full[, c(1, 2, 3)][, .SD[which.min(In_Mig)], by = Year]
data_min <- merge(data_min, data_full[, c(1, 2, 57)]
                  [, .SD[which.min(Out_Mig)],by = Year], by = "Year")
data_min <- merge(data_min, data_full[, c(1, 2, 58)]
                  [, .SD[which.min(Net_Mig)], by = Year], by = "Year")

data_max <- data_full[, c(1, 2, 56)][, .SD[which.max(Tot_emp)], by = Year]




data_mig <- melt.data.table(data_full, id.vars = c("GeoName", "Year"),
                 measure.vars = colnames(data_full)[4:52],
                 variable.factor = FALSE)
data_mig <- data_mig[GeoName != variable, ]
data_mig[data_full[, c(1, 2, 55)], pop_i := i.pop,
         on = c(GeoName = "GeoName", Year = "Year")]
data_mig[data_full[, c(1, 2, 55)], pop_j := i.pop,
         on = c(variable = "GeoName", Year = "Year")]

distance <- unique(data_mig[, c(1, 3)])
distance <- distance[shp@data[, c(1, 4, 5)], on = "GeoName"]
distance <- distance[shp@data[, c(1, 4, 5)], on = c(variable = "GeoName")]
distance[, euc_dist := sqrt((i.V1 - V1)^2 + (i.V2 - V2)^2)]
distance[, (3:6) := NULL]
data_mig <- merge(data_mig, distance, by = c("GeoName", "variable"))
rm(distance, abb, shpdf)

data_mig <- merge(data_mig, data_full[, c(1:3, 53, 56:58)], by = c("GeoName", "Year"))
data_mig[data_full[, c(1, 2, 53)], per_cap_per_inc_j := i.per_cap_per_inc,
         on = c(variable = "GeoName", Year = "Year")]
####
data_mig[data_full[, c(1:2, 56)], Tot_emp_j := i.Tot_emp, on = c(variable = "GeoName", Year = "Year")]
log_data_mig <- data_mig
log_data_mig[value == 0, "value"] <- 1
log_data_mig[, (c(4:7, 9:10, 13, 14)) := lapply(.SD, log), .SDcols = c(4:7, 9:10, 13, 14)]
log_data_mig[, GeoName := paste0(GeoName, "-", variable)]
log_data_mig[, tot := (Tot_emp - Tot_emp_j)/Tot_emp]
log_data_mig[, variable := NULL]
library(plm)
log_data_panel <- pdata.frame(log_data_mig, index = c("GeoName", "Year"),
                              drop.index = TRUE)

#best model - log-normal random panel model
log_norm_mod <- plm(value ~ pop_i + pop_j + per_cap_per_inc + per_cap_per_inc_j + 
                      euc_dist + Tot_emp + Tot_emp_j, data = log_data_panel, model = "random",
                    effect = "individual")

log_norm_mod <- plm(value ~ pop_i + pop_j + per_cap_per_inc + per_cap_per_inc_j + 
                      euc_dist + Tot_emp + Tot_emp_j, data = log_data_panel, model = "random",
                    effect = "time")
summary(log_norm_mod)
phtest(value ~ pop_i + pop_j + per_cap_per_inc + per_cap_per_inc_j + 
         euc_dist + Tot_emp + Tot_emp_j, data = log_data_panel)

library(pglm)
data_panel <- copy(data_mig)
data_panel[, GeoName := paste0(GeoName, "-", variable)]
data_panel[, variable := NULL]
data_panel[, (c(8, 10:12)) := lapply(.SD, function(x) x/1000), .SDcols = c(8, 10:12)]
data_panel[, (c(4, 5, 9, 13)) := lapply(.SD, function(x) x/100000), .SDcols = c(4, 5, 9, 13)]

data_panel <- pdata.frame(data_panel, index = c("GeoName", "Year"),
                          drop.index = TRUE)
poi_pnl_mod <- pglm(value ~ pop_i + pop_j+
                      euc_dist, data = data_panel, effect = "time", model = "random",
                    family = poisson)
poi_pnl_mod2 <- pglm(value ~ pop_i + pop_j + Tot_emp + Tot_emp_j+ per_cap_per_inc +
                       euc_dist, data = data_panel, effect = "time", model = "random",
                     family = poisson)
AIC(poi_pnl_mod)
AIC(poi_pnl_mod2)[1]
summary(poi_pnl_mod2)
rm(log_norm_mod, poi_pnl_mod, poi_pnl_mod2, log_data_mig, log_data_panel, data_panel)
####

detach(package:plm)
detach(package:pglm)

############
shp@data <- shp@data[, c(1, 3)]
shp@data$id <- as.numeric(rownames(shp@data))
data_mig[, (c(8, 11, 12)) := NULL]
data_mig[shp@data[, c(1, 3)], Dest_fact := i.id, on = c(GeoName = "GeoName")]
data_mig[shp@data[, c(1, 3)], Orig_fact := i.id, on = c(variable = "GeoName")]
data_mig[, (c(2, 12, 13)) := lapply(.SD, as.factor), .SDcols = c(2, 12, 13)]
library(mlr3)
library(iml)
library(mlr3learners)
library(mlr3spatiotempcv)
#data_mig <- fread("upload.csv")
data_use <- data_mig[, c(2, 4:13)]
task_mig <- TaskRegr$new(id = "migration", backend = data_use, target = "value")
learner <- lrn("regr.rpart")
learner$train(task_mig)
prediction <- learner$predict(task_mig)
prediction$score(msr("regr.rsq"))
prediction$score(msr("regr.mse"))
prediction$score(msr("regr.msle"))

future::plan(multisession, workers = 3)
model <- Predictor$new(learner, data = data_use[, -2], y = data_mig$value)
importance <- FeatureImp$new(model, loss = "mae")
features <- c("Eucliadean 
              Distance", "Population: 
              Destination", "Total Employment: 
              Origin", "Population: 
              Origin", "Origin 
              State", "Total Employment: 
              Destination", "Destination 
              State", "Per Capita 
              Personal Income: 
              Origin", "Per Capita
              Personal Income: 
              Destination", "Year")
importance$results$feature <- features
plot(importance)

effects <- FeatureEffects$new(model, method = "pdp")
plot(effects)

rm(task_mig, learner, prediction, model, effects)

task_mig <- TaskRegr$new(id = "migration", backend = data_use, target = "value")
learner <- lrn("regr.rpart")
learner$train(task_mig)

# Decision Spatio temporal CV
data_use <- data_mig[, c(1:2, 4:13)]
coord <- data.table(GeoName = shp@data[, 1], coordinates(shp))
data_use[coord, (c("x", "y")) := .(i.V1, i.V2), on = .(GeoName)]
rm(coord, shp_data)
data_use[, (c("GeoName", "Year")) := NULL]

task_mig <- TaskRegrST$new(id = "migration", backend = data_use, target = "value",
                           extra_args = list(coords_as_features = FALSE,
                                             crs = "+proj=longlat +datum=NAD83 +no_defs ",
                                             coordinate_names = c("x", "y")))
learner <- lrn("regr.ranger", num.threads = 3)
learner$train(task_mig)
resampling <- rsmp("repeated_sptcv_cstf", folds = 5, time_var = "Year",
                   space_var = "GeoName", repeats = 5)
rr <- resample(task_mig, learner, resampling)
rr$aggregate(msr("regr.rsq"))
rr$aggregate(msr("regr.mse"))
rr$aggregate(msr("regr.msle"))

#Random forest - normal - 1 - rsq = 0.9893138
data_use <- data_mig[, c(2, 4:13)]
task_mig <- TaskRegrST$new(id = "migration", backend = data_use, target = "value")
learner <- lrn("regr.ranger", num.threads = 4)
learner$train(task_mig)
prediction <- learner$predict(task_mig)
prediction$score(msr("regr.rsq"))
prediction$score(msr("regr.mse"))
prediction$score(msr("regr.msle"))
model <- Predictor$new(learner, data = data_use[, -2], y = data_mig$value)
effect <- FeatureEffects$new(model, method = "pdp")
plot(effect)
importance <- FeatureImp$new(model, loss = "mae")
features <- c("Eucliadean 
              Distance", "Population: 
              Destination", "Population: 
              Origin", "Total Employment: 
              Origin", "Total Employment: 
              Destination", "Destination 
              State", "Origin 
              State", "Per Capita 
              Personal Income: 
              Destination", "Per Capita
              Personal Income: 
              Origin", "Year")
importance$results$feature <- features
plot(importance)
rm(data_use, task_mig, learner, prediction, model, effect)

## Random forest - holdout 0.8 - 2 - rsq= 0.951554
data_use <- data_mig[, c(2, 4:13)]
task_mig <- TaskRegr$new(id = "migration", backend = data_use, target = "value")
learner <- lrn("regr.ranger", num.threads = 3)
resampling <- rsmp("holdout", ratio = 0.8)
rr <- resample(task_mig, learner, resampling)
rr$aggregate(msr("regr.rsq"))
rr$aggregate(msr("regr.mse"))
rr$aggregate(msr("regr.msle"))
rm(data_use, task_mig, learner)

# Random Forest - CV 5 - 3 - rsq = 0.9481204 
data_use <- data_mig[, c(2, 4:13)]
task_mig <- TaskRegr$new(id = "migration", backend = data_use, target = "value")
learner <- lrn("regr.ranger", num.threads = 3)
learner$train(task_mig)
resampling <- rsmp("cv", folds = 5)
rr <- resample(task_mig, learner, resampling)
rr$aggregate(msr("regr.rsq"))
rr$aggregate(msr("regr.mse"))
rr$aggregate(msr("regr.msle"))
model <- Predictor$new(rr, data = data_use[, -2], y = data_mig$value)
rm(data_use, task_mig, learner)

# Random Forest Spatio temporal CV
data_use <- data_mig[, c(1:2, 4:13)]
coord <- data.table(GeoName = shp@data[, 1], coordinates(shp))
data_use[coord, (c("x", "y")) := .(i.V1, i.V2), on = .(GeoName)]
rm(coord, shp_data)
data_use <- sf::st_as_sf(data_use, coords = c("x", "y"), crs)

task_mig <- TaskRegrST$new(id = "migration", backend = data_use, target = "value",
                         extra_args = list(coords_as_features = FALSE,
                                           crs = "+proj=longlat +datum=NAD83 +no_defs ",
                                           coordinate_names = c("x", "y")))
learner <- lrn("regr.ranger", num.threads = 3)
learner$train(task_mig)
resampling <- rsmp("repeated_sptcv_cstf", folds = 5, time_var = "Year",
                   space_var = "GeoName", repeats = 5)
rr <- resample(task_mig, learner, resampling)
rr$aggregate(msr("regr.rsq"))
rr$aggregate(msr("regr.mse"))
rr$aggregate(msr("regr.msle"))
model <- Predictor$new(rr, data = data_use[, -2], y = data_mig$value)
rm(data_use, task_mig, learner)

# o


data <- fread("data/full_data.csv", drop = 5:14)

data <- data_full[Year == 2011, c(1:2, 4:52)]
m <- as.matrix(data[, 3:ncol(data)])
dimnames(m) <- list(orig = data$GeoName, dest = colnames(data)[-1:-2])
max <- vector(mode = "integer")
for(i in 1:nrow(data)){
  max[i] <- sum(data[, i+2] + sum(data[i, -1:-2]))
}
data[, max := max]
data[, order := 1:49]
col <- c("#808080","#d3d3d3","#2f4f4f","#556b2f","#8b4513","#7f0000","#191970",
         "#808000","#5f9ea0","#008000","#3cb371","#bdb76b","#4682b4","#d2691e",
         "#9acd32","#cd5c5c","#00008b","#32cd32","#daa520","#8fbc8f","#800080",
         "#9932cc","#ff4500","#ff8c00","#ffd700","#ffff00","#0000cd","#00ff00",
         "#00ff7f","#dc143c","#00ffff","#00bfff","#f4a460","#a020f0","#adff2f",
         "#ff6347","#ff00ff","#db7093","#6495ed","#dda0dd","#90ee90","#87ceeb",
         "#ff1493","#7b68ee","#ee82ee","#7fffd4","#ffdab9","#ff69b4","#ffb6c1")
data[, col := col]

library("circlize")

circos.clear()

par(mar = rep(0, 4), cex=0.9)
circos.par(start.degree = 90, gap.degree = 4)

chordDiagram(x = m, directional = 1, order = data$GeoName,
             grid.col = data$col, annotationTrack = "grid",
             transparency = 0.25,  annotationTrackHeight = c(0.1, 0.1),
             diffHeight  = -0.04)

circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
  xlim = get.cell.meta.data("xlim")
  sector.index = get.cell.meta.data("sector.index")
  circos.text(mean(xlim), 2.5, sector.index, facing = "bending")
  circos.axis("top", major.at = seq(0, max(xlim)), minor.ticks=1, labels.niceFacing = FALSE )
}, bg.border = NA)


library(ggplot2)
data_mig[, (5:ncol(data_mig)) := NULL]
data_mig <- merge(data_mig, shp@data[, c(1, 4, 5)], by = "GeoName")
data_mig <- data_mig[shp@data[, c(1, 4, 5)], on = c(variable = "GeoName")]
colnames(data_mig)[-1:-4] <- c("ox", "oy", "dx", "dy")
data_mig <- data_mig[Year == 2011, ]
data_mig[, (3):=NULL]
library(ggmap)
lat <- c(27, 48)
long <- c(-70, -125)
bbox <- make_bbox(long, lat, f = 0.1)
map_us <- get_map(bbox, zoom = 5, maptype = "terrain")
ggmap(map_us)+
  geom_segment(data = data_mig, aes(x = ox, y = oy, xend = dx, yend = dy, alpha = value), col = "white")+
  scale_alpha_continuous(guide = FALSE)+
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        axis.text.y = element_blank(), axis.ticks.y = element_blank())

## Edge
library(igraph) 
library(ggraph)
d1 <- data.frame(from="origin", to=paste("group", seq(1,10), sep=""))
d2 <- data.frame(from=rep(d1$to, each=10), to=paste("subgroup", seq(1,100), sep="_"))
hierarchy <- rbind(d1, d2)
vertices <- data.frame(name = unique(c(as.character(hierarchy$from), as.character(hierarchy$to))) ) 
mygraph <- graph_from_data_frame( hierarchy, vertices=vertices)
all_leaves <- paste("subgroup", seq(1,100), sep="_")
connect <- rbind( 
  data.frame( from=sample(all_leaves, 100, replace=T) , to=sample(all_leaves, 100, replace=T)), 
  data.frame( from=sample(head(all_leaves), 30, replace=T) , to=sample( tail(all_leaves), 30, replace=T)), 
  data.frame( from=sample(all_leaves[25:30], 30, replace=T) , to=sample( all_leaves[55:60], 30, replace=T)), 
  data.frame( from=sample(all_leaves[75:80], 30, replace=T) , to=sample( all_leaves[55:60], 30, replace=T)) 
)
(from <- match( connect$from, vertices$name))
to <- match( connect$to, vertices$name)
ggraph(mygraph, layout = 'dendrogram', circular = TRUE) + 
  geom_conn_bundle(data = get_con(from = from, to = to), alpha=0.2, colour="skyblue", tension = 0.9) + 
  geom_node_point(aes(filter = leaf, x = x*1.05, y=y*1.05)) +
  theme_void()

d1 <- data.frame(from = "Year", to = as.character(2011:2019))
d2 <- data.frame(from = rep(d1$to, each = 49), to = rep(paste0("region ", 1:49), times = 9))
d2$to <- paste(d2$to, d2$from)
hierarchy <- rbind(d1, d2)
vertices <- data.frame(name = unique(c(as.character(hierarchy$from), as.character(hierarchy$to))) ) 
mygraph <- graph_from_data_frame(hierarchy, vertices=vertices)

ft <- tidyr::crossing(var1 = 11:59, var2 = 11:59)
ft <- ft[ft$var1 != ft$var2, ]
ggraph(mygraph, layout = 'dendrogram', circular = TRUE) + 
  geom_conn_bundle(data = get_con(from = ft$var1, to = ft$var2), alpha=0.2, colour="skyblue", tension = 0.9) + 
  geom_node_point(aes(filter = leaf, x = x*1.05, y=y*1.05)) +
  theme_void()

hierarchy <- data.frame(from = "regions", to = paste0("region ", 1:49))
vertices <- data.frame(name = unique(c(as.character(hierarchy$from), as.character(hierarchy$to))) ) 
mygraph <- graph_from_data_frame(hierarchy, vertices=vertices)
ft <- tidyr::crossing(var1 = 2:50, var2 = 2:50)
ft <- ft[ft$var1 != ft$var2, ]
ft$value <- rnorm(nrow(ft), 2, 3)

ggraph(mygraph, layout = 'dendrogram', circular = TRUE) + 
  geom_conn_bundle(data = get_con(from = ft$var1, to = ft$var2), alpha=0.2, colour=ft$var1, tension = 0.9) + 
  geom_node_point(aes(filter = leaf, size = c(0, hierarchy$value), x = x*1.05, y=y*1.05)) +
  theme_void()


# Alluvial
library(ggalluvial)
data <- data_mig[, 1:4]
is_alluvia_form(data, axes = 1:3)
ggplot(data, aes(y = value, axis1 = variable, axis2 = Year, axis3 = GeoName))+
  geom_alluvium(aes(fill = variable), width = 1/12)+
  geom_stratum(width = 1/12, fill = "black", color = "grey")
data <- data[Year == 2011, ]
ggplot(data, aes(y = value, axis1 = variable, axis2 = GeoName))+
  geom_alluvium(aes(fill = variable), width = 1/12)+
  geom_stratum(width = 1/12, fill = "black", color = "grey")
