library(sf) # sf class
library(sp) # sp class
library(spdep) # spatial statistics
library(RColorBrewer) # nice colours
library(spatialreg)
library(tmap)
library(ggplot2)
library(dplyr)



rm(list=ls())

#####loading data
{
  setwd("D:\\Studia\\4 rok\\Przestrzenna\\Projekt\\Dane\\")
  
  EU <- st_read('shape_data\\NUTS_RG_20M_2021_4326.shp',stringsAsFactors = FALSE)
  EU <- EU[EU$LEVL_CODE == 2, ]
  
  
  colnames(EU)[colnames(EU) == "NUTS_ID"] <- "geo"
  
  #EU.sp<-as_Spatial(EU, cast = TRUE, IDs ="NUTS_ID")
  
  dane<-read.csv("dataset2.csv", sep=",", dec=".", header=TRUE,
                 encoding="utf-8") # dane w csv UTF-8 / CSV data encoded in UTF-8
}

# check for maps
{
  cont.sf<- poly2nb(EU) # class nb
  cont.sf
  numery_indeksu <- c(150, 178, 240, 242, 243, 255, 303, 304, 305, 306, 307, 
                      308, 316, 345, 372, 382, 385, 386, 387, 392, 429, 430, 2009)
  
  wiersze_wybrane <- subset(EU, rownames(EU) %in% as.character(numery_indeksu))
  wartosci_geo <- paste0(wiersze_wybrane$geo,  collapse = '", "')
  wartosci_geo
  
  
  missing_names2 <- setdiff(EU$geo, dane$geo )
  #missing_names2
  # delete regions far from continent eg., gujana etc.
  EU1 <- subset(EU, !(EU$geo %in% c("AL01", "AL02", "AL03", "LI00", "NO0B", "CY00", 
                                    "EL41", "MT00", "EL42", "EL43",  "ITG2", "FRM0", 
                                    "FRY1", "FRY2", "FRY3", "FRY4", "FRY5", "ITG1", "EL62", 
                                    "IS00", "ES53", "ES63", "ES64", "ES70", "FI20", "PT20", "PT30" )))
  
  missing_names <- setdiff(dane$geo, EU1$geo)
  #missing_names
  # data from the eurostat contains mixed regions from nuts 2010, 2013, 2016, 2021, delete this regions 
  # without a mapping 
  dane1 <- subset(dane, !(dane$geo %in% c("CY00", "EA20", "EL41", "EL42", "EL43", "EL62", 
                                          "ES53", "ES63", "ES64", "ES70", "FI20", "FRM0", 
                                          "FRY1", "FRY2", "FRY3", "FRY4", "HR04", "IS00", 
                                          "ITG1", "ITG2", "MT00", "NO01", "NO03", "NO04", 
                                          "NO05", "PT20", "PT30")))
  
  length(unique(dane1$geo))
  length(unique(EU1$geo))
  # 307 uniques regions
}

# time variety of unemp, plots
{
  dane$country <- substr(dane$geo, 1, 2)
  
  # calculate weighted unemp 
  dane$weighted_unemp <- dane$unemp * dane$pop
  weighted_sum <- dane %>%
    group_by(country, time) %>%
    summarise(
      total_weighted_unemp = sum(weighted_unemp), # Sum
      total_pop = sum(pop) # tot pop
    ) %>%
    mutate(weighted_avg_unemp = total_weighted_unemp / total_pop) # avg unemp
  
  # transformation of data for plot
  weighted_avg_unemployment <- reshape2::dcast(weighted_sum, country ~ time, value.var = "weighted_avg_unemp")
  
  # prepare data for ggplot
  melted_data <- reshape2::melt(weighted_avg_unemployment, id.vars = 'country', variable.name = 'year', value.name = 'weighted_avg_unemp')
  
  # plot
  ggplot(melted_data, aes(x = year, y = weighted_avg_unemp, group = country, color = country)) +
    geom_line() + 
    theme_minimal() +
    labs(title = "Średnie ważone wskaźniki bezrobocia w latach według kraju",
         x = "Rok",
         y = "Średnie ważone wskaźniki bezrobocia (%)")
  
  
  # calculate per capita values for 'inv', 'tourists' i 'wages'
  dane$inv_per_capita <- dane$inv / dane$pop
  dane$tourists_per_capita <- dane$tourists / dane$pop
  dane$wages_per_capita <- dane$wages / dane$pop
  
  # columns to average
  columns_to_average <- c("education", "emp_high_tech", "gdp", 
                          "inv_per_capita", "migration", "pop", 
                          "pop_dens", "tourists_per_capita", "unemp", 
                          "wages_per_capita")
  
  # calculate average for regions
  average_values <- dane %>%
    group_by(geo) %>%
    summarise(across(all_of(columns_to_average), mean, na.rm = TRUE))
  
  avg_map <- merge(EU1, average_values, by =c('geo'))
  
  tmap_mode("plot")
  
  # list of variables to plot
  variables <- c("education", "emp_high_tech", "gdp", 
                 "inv_per_capita", "migration", "pop", 
                 "pop_dens", "tourists_per_capita", "unemp", 
                 "wages_per_capita")
  
  # path to save maps
  save_path <- "D:\\Studia\\4 rok\\Przestrzenna\\Projekt\\Dane\\png_maps\\"
  
  # loop for maps
  for (var in variables) {
    map <- tm_shape(avg_map) +
      tm_fill(var, palette = sf.colors(5), title = var) +
      tm_layout(legend.position = c("0.8", "top"))
    
      file_name <- paste0(save_path, var, ".png")
    
      tmap_save(map, file_name)
  }
  
  
  EU1_powtorzony <- EU1
  EU1_powtorzony$time <- 2014
  # add "time" from 2014 to 2019
  for (year in 2015:2019) {
    EU1_tmp <- EU1
    EU1_tmp$time <- year
    EU1_powtorzony <- rbind(EU1_powtorzony, EU1_tmp) 
  }
  
  rownames(EU1_powtorzony) <- NULL  # Zresetuj indeksy
  
  
  EU2 <- merge(EU1_powtorzony, dane1, by =c('time','geo'))
  
  EU11 <- EU2[EU2$time == 2019, ]
  tmap_mode("view")
  tm_shape(EU11) + tm_fill("tourists", palette = sf.colors(5))
}



# common border matrix 
cont.sf<- poly2nb(EU1) # class nb
cont.listw<-nb2listw(cont.sf, style="W") # class listw


# inverse distance matrix
{
  # spatial weights matrix – inverse distance
  crds<-st_centroid(EU1)
  pov.knn<-knearneigh(as.matrix(st_geometry(crds)), k=306) # 307 units onthe map
  pov.nb<-knn2nb(pov.knn)
  dist<-nbdists(pov.nb, crds)
  dist1<-lapply(dist, function(x) 1/x)
  dist.listw<-nb2listw(pov.nb, glist=dist1) # listw class
}



# panels

{
  library(splm) 
  library(plm)
  
  dane1$y <- dane1$unemp
  dane1$x1<-dane1$education
  dane1$x2<-dane1$emp_high_tech
  dane1$x3<-dane1$gdp
  dane1$x4<-dane1$inv
  dane1$x5<-dane1$migration
  dane1$x6<-dane1$pop_dens
  dane1$x7<-dane1$tourists / dane1$pop
  dane1$x8<- dane1$wages / dane1$pop

  
  
  # lags of independent variables 
  {
    library(data.table)
    setDT(dane1)
    
    # Sort
    setorder(dane1, geo, time)
    
    # Tworzenie opóźnionych zmiennych
    dane1[, x1_lag := shift(x1, 1, type = "lag"), by = geo]
    dane1[, x2_lag := shift(x2, 1, type = "lag"), by = geo]
    dane1[, x3_lag := shift(x3, 1, type = "lag"), by = geo]
    dane1[, x4_lag := shift(x4, 1, type = "lag"), by = geo]
    dane1[, x5_lag := shift(x5, 1, type = "lag"), by = geo]
    dane1[, x6_lag := shift(x6, 1, type = "lag"), by = geo]
    dane1[, x7_lag := shift(x7, 1, type = "lag"), by = geo]
    dane1[, x8_lag := shift(x8, 1, type = "lag"), by = geo]
    
    library(tidyr)
    dane1 <- dane1 %>%
      group_by(geo) %>%
      fill(x1_lag, x2_lag, x3_lag, x4_lag, x5_lag, x6_lag, x7_lag, x8_lag, .direction = "downup") %>%
      ungroup()
    
    # Sprawdzenie wyników
    head(dane1)
    
    eq1 <- y~x1+x2+x3+x4+x5+x6+x7+x8+
      x1_lag + x2_lag + x3_lag + x4_lag + x5_lag + x6_lag + x7_lag + x8_lag
    
  }
  
  
  eq1<-y~x1+x2+x3+x4+x5+x6+x8
  data.pdf<-pdata.frame(dane1)
  
  bsjktest(eq1, data=data.pdf, listw=cont.listw, test="C.1")
  bsjktest (eq1, data=data.pdf, listw=cont.listw, test="J")
  
  bsktest(eq1, data=data.pdf, listw=cont.listw, test="LMH", standardize=TRUE)
  bsktest(eq1, data=data.pdf, listw=cont.listw, test="LM1", standardize=TRUE)
  bsktest(eq1, data=data.pdf, listw=cont.listw, test="LM2", standardize=TRUE)
  
  test1<-sphtest(eq1, data=data.pdf, listw=cont.listw, spatial.model="error", method="GM")
  test1
  # since Hausman test cant tell us wheter to pick FE or RE we will use FE, 
  # because N is not too big 
  # coefficient lambda – spatial lag of y
  # coefficient rho – coefficient for error spatial autocorrelation, in kkp and b
  

  ### Stage 1: full model 
  model_full1 <- spml(eq1, data=data.pdf, listw=cont.listw, model="within", 
                      spatial.error="b", lag=TRUE, effect="twoways", rel.tol=2e-40)
  summary(model_full1)
  
  # reduce time effects
  model_red_11 <- spml(eq1, data=data.pdf, listw=cont.listw, model="within", 
                       spatial.error="b", lag=TRUE, effect="individual", rel.tol=2e-40)
  summary(model_red_11)
  # reduce individual effects
  model_red_12 <- spml(eq1, data=data.pdf, listw=cont.listw, model="within", 
                       spatial.error="b", lag=TRUE, effect="time", rel.tol=2e-40)
  summary(model_red_12)
  
  # time effects seems to be better for that model, rho >0 and significant
  # since I'm not not using Durbin - type model, and both, spatial lag of Y and 
  # spatial autocorrelation are significant I'm not removing any spatial compenents and
  # continue work to Stage 2:
  
  data.pdf$y <- data.pdf$y
  data.pdf$edu<-data.pdf$x1
  data.pdf$high_tech<-data.pdf$x2
  data.pdf$gdp<-data.pdf$x3
  data.pdf$inv<-data.pdf$x4 
  data.pdf$mig<-data.pdf$x5
  data.pdf$dens<-data.pdf$x6
  data.pdf$wages<- data.pdf$x8 
  
  eq2<-y~edu+high_tech+gdp+inv+mig+dens+wages
  
  model_f <- spml(eq2, data=data.pdf, listw=cont.listw, model="within", 
                       spatial.error="b", lag=TRUE, effect="time", rel.tol=2e-40)
  summary(model_f)
  
  # we cant interprate coefficients because we are using laged Y, 
  # we have to use direct and indirect impacts
  
  imp<-spatialreg::impacts(model_f, listw=cont.listw, time=6)
  summary(imp, zstats=TRUE, short=TRUE)
  
  
  

  
  
}

##### cross section models
{
  
  sub<-dane1[dane1$time==2019, ]
  sub$y<-sub$unemp
  sub$x1<-sub$education
  sub$x2<-sub$emp_high_tech
  sub$x3<-sub$gdp
  sub$x4<-sub$inv / sub$pop
  sub$x5<-sub$migration
  sub$x6<-sub$pop_dens
  sub$x7<-sub$tourists / sub$pop
  sub$x8<- sub$wages
  eq<-y~x1+x2+x3+x4+x5+x6+x7+x8
  
  
  
  eu1<-as_Spatial(EU1, cast = TRUE, IDs ="NUTS_ID")
  eu1$y<-sub$y
  range(eu1$y)
  rng<-seq(0, 20, 4) # from, to, by
  cls = brewer.pal(7, "PuBuGn")
  spplot(eu1, "y", col.regions = cls, at = rng)
  
  model.lm<-lm(eq, data=sub)
  summary(model.lm)
  
  res<-model.lm$residuals
  
  lm.morantest(model.lm, cont.listw) # czy reszty są losowe przestrzennie?
  moran.test(sub$x1, cont.listw)
  
  reszty<-factor(cut(res, breaks=c(-100, 0, 100),
                     labels=c("ujemne","dodatnie")))
  
  joincount.test(reszty, cont.listw)
  
  # Manski model (full specification) –  spatial lag of Y (rho),
  # option type="sacmixed" actives spatial lags of X
  GNS_1<-sacsarlm(eq, data=sub, listw=cont.listw, type="sacmixed",
                  method="LU") # method="LU"  / speed up
  summary(GNS_1)
  
  
  # SAC / SARAR model - includes spatial lag of Y, spatial error term
  SAC_1<-sacsarlm(eq, data=sub, listw=cont.listw)
  summary(SAC_1)
  
  # SEM - spatial error model
  # typically includes spatial error term only (with lambda coefficient)
  # option etype="emixed" activates spatial lags of X (with theta coeff.)
  # what makes spatial Durbin error model
  SDEM_1<-errorsarlm(eq, data=sub, listw=cont.listw, etype="emixed") # with spatial lags of X
  summary(SDEM_1)
  SEM_1<-errorsarlm(eq, data=sub, listw=cont.listw) # no spat-lags of X
  summary(SEM_1)
  
  # SAR - spatial lag model
  # normally includes spatial lag of Y only (with rho coefficient)
  # option type="mixed" activates spatial lags of X (with theta coeff.)
  SDM_1<-lagsarlm(eq, data=sub, listw=cont.listw, type="mixed") # with spatial lags of X
  
  summary(SDM_1)
  SAR_1<-lagsarlm(eq, data=sub, listw=cont.listw) # no spatial lags of X
  summary(SAR_1)
  
  
  # an ‘lm’ model augmented with the spatially lagged RHS variables
  # RHS variables – right-hand side variables
  SLX_1<-lmSLX(eq, data=sub, listw=cont.listw)
  summary(SLX_1)
  
  # OLS – no spatial components
  OLS_1<-lm(eq, data=sub)
  summary(OLS_1)
  
  
  library(texreg)
  
  screenreg(list(GNS_1, SAC_1, SDEM_1, SEM_1, SDM_1, SAR_1, SLX_1, OLS_1),
            custom.model.names=c("GNS_1", "SAC_1", "SDEM_1", "SEM_1", "SDM_1", "SAR_1",
                                 "SLX_1", "OLS_1"))
  
  
  
}



