
# ==============================================================================
# Make sure your device has internet connection before running the code!
# ==============================================================================


# Clean desk
rm(list=ls())
cat("\014") 


# Working directory
setwd("~/MMM Dropbox/MMM/Analysis")


under_path = getwd()


# Install packages

# Packages for Choropleth_timeseries.R Code
my_packages_choropleth = c("plotly","dplyr","readr","readxl","lubridate","htmlwidgets",
                           "openxlsx","colorRamps","languageserver","xts","matrixStats",
                           "scales","tidyr") 

# Packages for App.R Code
my_packages_shiny = c("shinythemes","DT","data.table","shiny")

install_if_missing <- function(p) {
  if (p %in% rownames(installed.packages()) == F) {
    install.packages(p, dependencies = T)
  } else {
    cat(paste("Skipping already installed package:", p, "\n"))
  }
}

invisible(sapply(my_packages_choropleth, install_if_missing)) # R.Code
invisible(sapply(my_packages_shiny, install_if_missing)) # App.R

# Library
library(plotly)
library(dplyr)
library(readr)
library(readxl)
library(lubridate)
library(htmlwidgets)
library(openxlsx)
library(colorRamps)
library(languageserver)
library(xts)
library(matrixStats)
library(scales)
library(tidyr)



# ISO Map function  ============================================================

ISO_map <- function(pre_path, country_list ="yes", warnings = F){
  if (warnings == F) {
    options(warn=-1)
  } else {
    options(warn=0)
  }
  # set working directory
  break_folder <- paste0(pre_path, "/Choropleth Map")
  setwd(paste0(pre_path,"/Choropleth Map/"))
  
  # Map 1 + Transformation
  Map_1 <- read.csv("map_files/Map.csv", sep=";")
  Map_1$latitude <- as.numeric(gsub(".", "",
                                    Map_1$latitude, fixed = TRUE))* 1e-6
  Map_1$longitude <- as.numeric(gsub(".", "",
                                     Map_1$longitude, fixed = TRUE))* 1e-6
  names(Map_1)[names(Map_1) == "name"] <- "Country"
  names(Map_1)[names(Map_1) == "country"] <- "Code"
  
  # Map 2 + Transformation (make sure that your code has internet connection)
  Map_2 <- read.csv(
    "https://raw.githubusercontent.com/plotly/datasets/master/2014_world_gdp_with_codes.csv")
  names(Map_2)[names(Map_2) == "COUNTRY"] <- "Country"
  
  # Merging both Map files
  Map_df <- merge(Map_1, Map_2, by = "Country") %>%
    select(Country, Code, latitude, longitude, CODE)
  
  # additional countries
  Dem_congo <- c("Democratic Republic of the Congo","CD",-4.322447,15.307045,"COD")
  Congo <- c("Congo (Congo-Brazzaville)","CG",-0.6605788,14.8965794,"COG")
  Ivory_Coast <- c("Cote d'Ivoire", "Ci",5.345317,-4.024429,"CIV")
  Cape_Verde <- c("Cabo Verde","CV",15.120142,-23.6051721,"CPV")
  Republic_of_North_Macedonia <- c("North Macedonia","MK",41.6086,41.6086,"MKD")
  Myanmar <- c("Myanmar (formerly Burma)","MM",16.871311,96.199379,"MMR")
  Sao_Tome_and_Principe <- c("Sao Tome and Principe","ST",0.255436,6.602781,"STP")
  Gambia <- c("Gambia", "GM", 13.4457859, 13.4457859 -15.3061209, "GMB")
  South_Sudan <- c("South Sudan", "SS", 7.8626845, 29.6949232, "SSD")
  South_Korea <- c("South Korea", "KR", 37.532600, 127.024612, "KOR")
  North_Korea <- c("North Korea", "KP", 39.019444, 125.738052,"PRK")
  
  # final map 
  Map_df <- rbind(Map_df,Dem_congo, Congo,Ivory_Coast,Cape_Verde,
                  Republic_of_North_Macedonia,Myanmar,Sao_Tome_and_Principe,
                  Gambia, South_Sudan, South_Korea, North_Korea)
  
  Map_df[195,1] <- "United States of America"
  # View(as.data.frame(Map_df))
  
  # Write csv file
  if (country_list == "yes") {
    
    # Either use provided xlsx file ...
    map_xlsx <- readxl::read_xlsx(paste0(gsub("/Analysis","",under_path),"/QualitativeDefinitions/country_list.xlsx"))
    for (i in 1:nrow(Map_df)) {
      if (Map_df[i,5] %in% map_xlsx$alternative10) {
        Map_df[i,1] <- map_xlsx[map_xlsx$alternative10 %in% Map_df[i,5],1]
      }
    }
    # View(as.data.frame(Map_df))
    try(dir.create(paste0(break_folder,"/map_files")))
    try(write.csv(Map_df, file = paste0(pre_path,"/Choropleth Map/map_files/Map_df.csv")))
    
  # ... or use the created file
  } else {
    try(dir.create(paste0(break_folder,"/map_files")))
    try(write.csv(Map_df, file = paste0(pre_path,"/Choropleth Map/map_files/Map_df.csv")))
  }
  # View(as.data.frame(Map_df))
}

ISO_map(under_path)
#ISO_map(under_path, warnings = T)




# Choropleth Map function  =====================================================
 
create_map <- function(pre_path,
                       criteria,
                       color_vector = c('red','#fc9803','#fcf003','#00CC66', 'dark green'),
                       ocean_color = '#C9E5EC', warnings = F) {
  # Warnings
  if (warnings == F) {
    options(warn=-1)
  } else {
    options(warn=0)
  }
  setwd(paste0(pre_path,"/Choropleth Map/"))
  
  if (file.exists("map_files/Map_df.csv") == F){
    ISO_map(pre_path)
  }
  Map_df <- read.csv("map_files/Map_df.csv")
  names(Map_df)[names(Map_df) == "CODE"] <- "ISO_Code"
  

  # Set working directory
  setwd(paste0(pre_path, "/Results/2 project - inequality"))
  
  # Read in all xlsx files
  path1 = paste0(pre_path, "/Results/2 project - inequality/")
  files <- list.files(path = path1,
                      pattern = "*.xlsx", full.names = F)

  # drop "~$files.xlsx"
  if (grepl('~', files[1]) == T) {
    files <- files[-1]
  }
  

  # Create time series data =============================
  
  # Read in necessary data
  sfm_indicators <- as.data.frame(readxl::read_xlsx(paste0(getwd(),
                                            "/",files)))
  countrynames <- as.data.frame(readxl::read_xlsx(
    paste0(gsub("/Analysis","",pre_path),"/QualitativeDefinitions/country_list.xlsx")))
  
  # Transform to data frame
  sfm_indicators <- as.data.frame(sfm_indicators)
  countrynames <- as.data.frame(countrynames)
  
  
  esg <- which(colnames(sfm_indicators)==criteria)
  
  pre_data_filtered <- sfm_indicators[c(1,ncol(sfm_indicators),esg)]
  names(pre_data_filtered)[names(pre_data_filtered) == "Country"] <- "ISO_Code"
  
  
  pre_data_filtered2 <- data.frame(pre_data_filtered) %>%
    mutate(year_ts = as.Date(ISOdate(Year,1,1)), class = "time_series") %>%
    group_by(ISO_Code, year_ts) 
  # potential problem: https://stackoverflow.com/questions/30255833/convert-four-digit-year-values-to-class-date
  
  data <- pre_data_filtered2 %>%
    select(year_ts, class, starts_with(!!criteria), ISO_Code, Year)
  
  data <- data[1:4420,]
  data_set <- na.omit(data)
  un <- unique(data[,4])
  data_set <- as.data.frame(data_set)
  
  
  precursor_data <- unstack(data_set, form = as.formula(paste(criteria, "~ ISO_Code")))
  data_transformed <- data.frame(sapply(
    precursor_data,'[', 1:max(unlist(lapply(precursor_data, length)))))[1:length(unique(data_set$year_ts)),]
  
  # Have to add new columns here if more data should be included (e.d. full country name)
  data_transformed <- data.frame(year_ts = unique(data_set$year_ts),
                                 class = unique(data_set$class),
                                 data_transformed)
    # cbind(year_ts = unique(data_set$year_ts),
    #                         class = unique(data_set$class),
    #                         data_transformed)
  


  # Calculate World/OECD/non-OECD mean and quantiles
  # 1) Indicators
  all_indicators <- names(sfm_indicators[-c(which(names(sfm_indicators) == "Country"),
                                            which(names(sfm_indicators) == "Year"))])
  # 2) Areas
  areas <- c("World","OECD","non-OECD")
  
  # 3) Years
  all_years <- unique(sfm_indicators$Year)
  
  
  area_vector <- c()
  for (area in areas) {
    area_vector <- c(area_vector,rep(paste0(area," Mean"), length(all_years)))
    area_vector <- c(area_vector,rep(paste0(area," 25% Quantile"), length(all_years)))
    area_vector <- c(area_vector,rep(paste0(area," 75% Quantile"), length(all_years)))
  }
  
  # Create second data frame for mean and quantile calculations
  sub_data_frame <- data.frame(area_vector)
  
  OECD_list <- c("AUS","AUT","BEL","CAN","CHL","COL","CRI","CZE","DNK",
                 "EST","FIN","FRA","DEU","GRC","HUN","ISL","IRL","ISR","ITA",
                 "JPN","KOR","LVA","LTU","LUX","MEX","NLD","NZL","NOR","POL",
                 "PRT","SVK","SVN","ESP","SWE","CHE","TUR","GBR","USA")
  
  
  # Calculation loop
  index_data_frame <- 2
  
  # 1) Indicator loop
  for (indicator in all_indicators) {
    # 2) Area loop
    for (area in areas) {
      if (area == "World") {
        area_subset <- sfm_indicators
      } else if (area == "OECD") {
        area_subset <- subset(sfm_indicators, Country %in% OECD_list)
      } else if (area == "non-OECD") {
        area_subset <- subset(sfm_indicators, !(Country %in% OECD_list))
      }
      indicator_subset <- area_subset[c(which(names(sfm_indicators) == "Country"),
                                        which(names(sfm_indicators) == indicator),
                                        which(names(sfm_indicators) == "Year"))]
      
      
      assign(paste0("mean_",area), c())
      assign(paste0("twentyfive_",area), c())
      assign(paste0("seventyfive_",area), c())
      
      # 3) Year loop
      for (year in all_years) {
        year_subset <- subset(na.omit(indicator_subset), Year %in% year)
        assign(paste0("mean_",area), c(get(paste0("mean_",area)),mean(year_subset[,2])))
        assign(paste0("twentyfive_",area), c(get(paste0("twentyfive_",area)),quantile(year_subset[,2], probs=0.25)))
        assign(paste0("seventyfive_",area), c(get(paste0("seventyfive_",area)),quantile(year_subset[,2], probs=0.75)))
      }
    }
    sub_data_frame[,index_data_frame] <- c(mean_World, twentyfive_World, seventyfive_World,
                                           mean_OECD, twentyfive_OECD, seventyfive_OECD,
                                           get("mean_non-OECD"), get("twentyfive_non-OECD"),
                                           get("seventyfive_non-OECD"))
    names(sub_data_frame)[names(sub_data_frame) == paste0("V",index_data_frame)] <- indicator
    names(sub_data_frame)[names(sub_data_frame) == "area_vector"] <- "Country"
    sub_data_frame[,index_data_frame][is.nan(sub_data_frame[,index_data_frame])] <- NA
    index_data_frame = index_data_frame + 1
  }
  sub_data_frame$Year <- rep(all_years, 9)
  
  
  precursor_data <- unstack(sub_data_frame, form = as.formula(paste(criteria, "~ Country")))
  data_transformed_world <- data.frame(sapply(
    precursor_data,'[', 1:max(unlist(lapply(precursor_data, length)))))[1:length(unique(sub_data_frame$Year)),]

  # Rename columns
  names(data_transformed_world) <- c("non_OECD_bottom","non_OECD_top","non_OECD_mean",
                                     "OECD_bottom","OECD_top","OECD_mean",
                                     "World_bottom","World_top","World_mean")
  
  # Reverse World/OECD/non-OECD data frame
  data_transformed_world <- rev(data_transformed_world)
  
  # Drop a row if it only contains NAs
  drop_row_NA <- apply(data_transformed_world, 1, function(x) all(is.na(x)))
  data_transformed_world <- data_transformed_world[ !drop_row_NA, ]
  
  
  # # Use full names instead of ISO-Code
  
  ISO_vector <- c(colnames(data_transformed))
  name_vector <- c()

  for (i in 1:(ncol(data_transformed))) {
    if (isTRUE(ISO_vector[i] %in% countrynames[,11])) {
      name_vector<- c(name_vector, countrynames[which(countrynames[,11] %in% ISO_vector[i]),1])
    } 
  }
  colnames(data_transformed) <- c("year_ts","class",name_vector)
  
  # Add row numbers
  number_vector <- as.data.frame(seq(nrow(data_transformed)))
  names(number_vector) <- c(" ")
  # Combine both data sets
  longpanel <- data.frame(number_vector,data_transformed,data_transformed_world)
    # cbind(number_vector,data_transformed,data_transformed_world)
  
  write_csv(longpanel, paste0(pre_path,"/Choropleth Map/Green_Finance_Indicators/data/longpanel_",criteria,".csv"))

  
  
  
  # Data Ranking ========================================
  
  data_ranking <- data.frame(sfm_indicators)
  
  ISO_vector <- c(unique(data_ranking[,1]))
  
  # Change Country Name
  for (i in 1:(length(ISO_vector))) {
    data_ranking$Country[data_ranking$Country == ISO_vector[i]] <- countrynames[which(countrynames[,11] %in% ISO_vector[i]),1]
  }
  
  # Calculate change for each indicator
  indicator_values <- colnames(data_ranking[2:(ncol(data_ranking)-1)])

  for(indic in indicator_values){
    data_ranking <- data_ranking %>%
      group_by(Country) %>%
      mutate(!!paste0("score_",indic) := # !!as.name(indic) - lead(!!as.name(indic)))
               ifelse(!!as.name(indic) - lead(!!as.name(indic)) == 0,
                                                0, ifelse(!!as.name(indic) - lead(!!as.name(indic)) > 0, 1, -1)))
      # mutate(!!paste0("percentage_",indic) := label_percent()(1 - (!!as.name(indic)/lead(!!as.name(indic)))))
  }
  
   
  # sub_data_ranking <- data_ranking %>% select(Country)
  # c_names <- c("Country")
  # for(indic in indicator_values){
  #   c_names <- c(c_names,indic, paste0("percentage_",indic))
  #   sub_data_ranking <- cbind(sub_data_ranking, data_ranking[[indic]],data_ranking[[paste0("percentage_",indic)]])
  # }
  # sub_data_ranking <- cbind(sub_data_ranking,data_ranking$Year)
  # c_names <- c(c_names,"Year")
  # colnames(sub_data_ranking) <- c_names
  # 
  # data_ranking <- sub_data_ranking

  
  # Write csv file for each year (all indicators)
  years <- c(unique(data_ranking[["Year"]]))
  
  for(year in years) {
    sub_data_ranking <- data_ranking[data_ranking[["Year"]] == year,]
    
    # Drop all indicators that only contain NAs for a given year
    sub_data_ranking_2 <- sub_data_ranking[, unlist(lapply(sub_data_ranking, function(x) !all(is.na(x))))]
    sub_colnames <- colnames(sub_data_ranking_2[,2:(ncol(sub_data_ranking_2)-1)])
    sub_colnames <- unique(gsub("^score_","",sub_colnames))
    sub_colnames <- sub_colnames[!sub_colnames %in% "Year"]

    sub_data_ranking <- data.frame(sub_data_ranking %>%
                                     select(Country,!!sub_colnames, Year, !!paste0("score_",sub_colnames)))
      
    
    
    # Drop all Rows that only contain NAs
    sub_data_ranking <- sub_data_ranking[rowSums(
      is.na(sub_data_ranking[,colnames(sub_data_ranking) %in% sub_colnames])) != 
      ncol(sub_data_ranking[,colnames(sub_data_ranking) %in% sub_colnames]),]
    
    
    
    # Write CSV
    write_csv(sub_data_ranking, paste0(pre_path,"/Choropleth Map/Green_Finance_Indicators/data/data_ranking_",year,".csv"))
  }
  


  # Create Choropleth maps ==============================
  
  # all files in ldf
  ldf <- lapply(paste0(path1, files), read.xlsx)
  # find position of indicator variable in ldf
  position <- which(files == files)
  
  
  # save data as data frame 
  if (any(ldf$Year < 2000) == T){
    data <- subset(as.data.frame(ldf[position]), Year>=2000)
  } else {
    data <- as.data.frame(ldf)
  }

  
  # Find column number of variable esg_criterion in data frame
  esg <- which(colnames(data)==criteria)
  
  
  data_filtered <- data %>%
    select("Country","Year",all_of(criteria))
  
  data_filtered <- na.omit(data_filtered)
  
  names(data_filtered)[names(data_filtered) == "Country"] <- "ISO_Code"
  
  # Choropleth Pre-settings
  l <- list(showcountry = T, color = toRGB("grey"), width = 0.5)
  
  # Specify map projection/options
  g <- list(
    showframe = F,
    showcoastlines = T,
    showocean = T,
    showland = T,
    oceancolor = toRGB(ocean_color),
    showlakes = T,
    lakecolor = toRGB('#9FD7E5'),
    projection = list(type = 'Mercator'),
    lataxis = list(range = c(-58.6,120)),
    landcolor = toRGB("grey93"),
    coastlinecolor = toRGB("grey93")
  )
  
  
  # Transform Data
  index <- data_filtered %>%
    inner_join(Map_df, by.x = Country, by.y = ISO_Code) %>%
    select(Year, ISO_Code, Country, longitude, latitude,
           starts_with(!!criteria)) %>%
    mutate(hover = paste0(Country, "<br><br>", paste0("Value:   ",
                                                      round(get(criteria),4))))


  z_index <- index[,which(colnames(index) == criteria)]
  
  # =========
  # max_value <- color_scaling(z_index, 0.25, decreasing = T)
  # min_value <- color_scaling(z_index, 0.25, decreasing = F)
  # min_value <- floor(min(color_scaling(z_index, 0.25, decreasing = F)))
  # max_value <- ceiling(max(color_scaling(z_index, 0.25, decreasing = T)))
  min_value <- floor(quantile(z_index, 0.01))
  max_value <- ceiling(quantile(z_index, 0.99))
  # =========
  
  fig <- plot_ly(index, frame = index$Year, type='choropleth',
                 locations=index$ISO_Code, z = z_index,
                 zmin = min_value,
                 # zmin = floor(min(z_index)),
                 zmax = max_value,
                 # zmax = ceiling(max(z_index)),
                 text = index$hover,
                 marker = list(line = l), hoverinfo = "text",
                 colors = color_vector,
                 colorbar=list(len=0.79,x = 5.8, y=0.812, thickness = 15))
  
  # title
  title_esg='<br>Choropleth Map'
  fig <- fig %>% 
    colorbar(title = paste0(criteria)) %>% 
    layout(title = title_esg, 
            geo = g,
            autosize = T) %>% 
    config(displayModeBar = F)
  fig
}


# criteria: E, BOND, GHG, PAT, CLMT

# color_vectors:
# currently used: c("#D2042D","#FFA500","#FFDB58","#4CBB17","#1E8477","#23335E")
# 1) c("#FF4F42","#FF6C42","#FF8942","#FFA742","#FFC442","#FFE242","#FFFF42","#D4D437","#AAAA2C","#7F7F21","#545416")
# 2) c("#FF4E4E","#FF5D3B","#FF7A27","#FF9C14","#FFC300","#59CB14","#239C5D","#2C5B74","#352F51")
# 3) c("#FEFCFF","#FED2F7","#FBA8CB","#F77E7F","#F28854","#ECBA2A","#C2E400","#12B113","#1E8477","#23335E","#32233D")
# 4) c("#FFFF7E","#FFED54","#FFAF2A","#F76700","#95DE01","#02C531","#0292AD","#0C0294")
# 5) c("#E6DA8E","#DADF77","#BBD760","#97CF4A","#B1B73F","#9F7D34","#86462A","#6C2025")


# Map color
# c("#E61B2F","#FF8268","#F8B056","#EBE149","#AADC3E","#63C937","#2FAF2B")
# c("#FF6564","#FF915A","#F6BB54","#E5DE50","#CCDB50","#858754","#464646")

# Ocean color
# c("#C9F0F2")
# c("#76b6c4")

create_map(under_path, 
           criteria = "PAT",
           color_vector = c("#D2042D","#FFA500","#FFDB58","#4CBB17","#1E8477","#23335E")
           )
           # ocean_color = c("white"))


# Pipette Hex Color: Abgeschwächteres blau



# Shiny function  ==============================================================

html_choropleth <- function(pre_path,  
                            esg_criteria,
                            color_vector = c('red','#fc9803','#fcf003','#00CC66', 'dark green'),
                            delete_files = "yes",
                            shiny_save = c("yes","no")){
  
  saving_folder <- paste0(pre_path, "/Choropleth Map")
  
  setwd(saving_folder)
  # try(dir.create(paste0(saving_folder,"/html_files")))
  
  color <- color_vector
  
  # Read in all xlsx files
  path1 = paste0(pre_path, "/Results/2 project - inequality/")
  files <- list.files(path = path1,
                      pattern = "*.xlsx", full.names = F)
  
  # drop "~$files.xlsx"
  if (grepl('~', files[1]) == T) {
    files <- files[-1]
  }
  
  proj <- read.xlsx(paste0(pre_path,
                            "/Results/2 project - inequality/",
                            files))
  

    
  # deletes folder with former choropleth html/data files
  if (delete_files == "yes") {
    shiny_folder_html <- paste0(saving_folder,"/Green_Finance_Indicators/html files")
    shiny_folder_data <- paste0(saving_folder,"/Green_Finance_Indicators/data")
    
    do.call(file.remove, list(list.files(shiny_folder_html, pattern = ".html", full.names = T)))
    do.call(file.remove, list(list.files(shiny_folder_data, pattern = ".csv", full.names = T)))
  }
  
  # push to shiny folder (Updates files in shiny folder)
  if (shiny_save == "yes"){ #|| shiny_save == "all") {
    shiny_folder <- paste0(saving_folder,"/Green_Finance_Indicators")
    setwd(shiny_folder)
    try(dir.create(paste0(shiny_folder,"/html files")))
    for (i in seq(esg_criteria)) {
      map <- create_map(pre_path, esg_criteria[i], color)
      
      choro <- ggplotly(p = map, width = 1000, height = 1000)
      try(saveWidget(choro, paste0(saving_folder, "/Green_Finance_Indicators/html files/choropleth - ",
                                   esg_criteria[i],".html")))
    }
    
    # Delete folders that are created with html files
    shiny_folder <- paste0(saving_folder,"/Green_Finance_Indicators/html files/")
    shiny_files <- list.files(shiny_folder, full.names = T)
    delete_files <- shiny_files[which(grepl("_files",shiny_files) == T)]
    
    for (i in seq(shiny_files)) {
      unlink(delete_files[i], recursive = T)
    }
  }

}

# indicators: E, BOND, GHG, PAT, CLMT

# Update specific maps
html_choropleth(under_path, 
                 c("E","BOND", "GHG", "PAT", "CLMT"), 
                 color_vector = c("#D2042D","#FFA500","#FFDB58","#4CBB17","#1E8477","#23335E"),
                 shiny_save = "yes", delete_files = "yes") 




