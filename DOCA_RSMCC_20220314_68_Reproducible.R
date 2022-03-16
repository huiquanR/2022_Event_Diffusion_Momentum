#### [Sys Setup] System Setup & Load Packages ####

# display sys environment #

Sys.info()
# sysname        release         version       nodename        machine 
# "Windows"      "10 x64"        "build 22000" "USERS"         "x86-64" 

sessionInfo()

# R version 4.1.1 (2021-08-10)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows 10 x64 (build 22000)

# Matrix products: default

# locale:
# [1] LC_COLLATE=Chinese (Simplified)_Singapore.936 
# [2] LC_CTYPE=Chinese (Simplified)_Singapore.936   
# [3] LC_MONETARY=Chinese (Simplified)_Singapore.936
# [4] LC_NUMERIC=C                                  
# [5] LC_TIME=Chinese (Simplified)_Singapore.936    

# attached base packages:
# [1] stats     graphics  grDevices utils     datasets  methods   base     

# other attached packages:
# [1] timeDate_3043.102      lubridate_1.8.0        data.table_1.14.2     
# [4] haven_2.4.3            questionr_0.7.7        MASS_7.3-55           
# [7] car_3.0-12             qdap_2.4.3             RColorBrewer_1.1-2    
#[10] qdapTools_1.3.5        qdapRegex_0.7.2        qdapDictionaries_1.0.7
#[13] pscl_1.5.5             sjlabelled_1.1.8       ggnewscale_0.4.6      
#[16] ggthemes_4.2.4         readstata13_0.10.0     ggeffects_1.1.1       
#[19] effects_4.2-1          carData_3.0-5          forcats_0.5.1         
#[22] purrr_0.3.4            readr_2.1.2            tidyr_1.2.0           
#[25] tibble_3.1.6           tidyverse_1.3.1        stringr_1.4.0         
#[28] gridExtra_2.3          sjPlot_2.8.10          sjmisc_2.8.9          
#[31] texreg_1.38.5          zipcode_1.0            fiftystater_1.0.1     
#[34] dplyr_1.0.8            maptools_1.1-3         sp_1.4-6              
#[37] ggmap_3.0.0            ggplot2_3.3.5          easypackages_0.1.0    

# loaded via a namespace (and not attached):
# [1] utf8_1.2.2          tidyselect_1.1.2    lme4_1.1-28         grid_4.1.1         
# [5] devtools_2.4.3      munsell_0.5.0       codetools_0.2-18    effectsize_0.6.0.1 
# [9] chron_2.3-56        miniUI_0.1.1.1      withr_2.5.0         colorspace_2.0-3   
#[13] NLP_0.2-1           highr_0.9           knitr_1.37          rstudioapi_0.13    
#[17] rJava_1.0-6         emmeans_1.7.2       slam_0.1-50         RgoogleMaps_1.4.5.3
#[21] openNLPdata_1.5.3-4 datawizard_0.3.0    rprojroot_2.0.2     coda_0.19-4        
#[25] vctrs_0.3.8         generics_0.1.2      TH.data_1.1-0       xfun_0.30          
#[29] R6_2.5.1            bitops_1.0-7        cachem_1.0.6        assertthat_0.2.1   
#[33] promises_1.2.0.1    scales_1.1.1        multcomp_1.4-18     nnet_7.3-17        
#[37] gtable_0.3.0        processx_3.5.2      sandwich_3.0-1      rlang_1.0.1        
#[41] splines_4.1.1       wordcloud_2.6       broom_0.7.12        reshape2_1.4.4     
#[45] abind_1.4-5         modelr_0.1.8        backports_1.4.1     httpuv_1.6.5       
#[49] tools_4.1.1         usethis_2.1.5       ellipsis_0.3.2      sessioninfo_1.2.2  
#[53] Rcpp_1.0.8          plyr_1.8.6          RCurl_1.98-1.6      ps_1.6.0           
#[57] prettyunits_1.1.1   cowplot_1.1.1       openNLP_0.2-7       zoo_1.8-9          
#[61] fs_1.5.2            survey_4.1-1        magrittr_2.0.2      openxlsx_4.2.5     
#[65] reprex_2.0.1        mvtnorm_1.1-3       pkgload_1.2.4       hms_1.1.1          
#[69] mime_0.12           xtable_1.8-4        XML_3.99-0.9        sjstats_0.18.1     
#[73] jpeg_0.1-9          readxl_1.3.1        testthat_3.1.2      compiler_4.1.1     
#[77] crayon_1.5.0        gender_0.6.0        minqa_1.2.4         htmltools_0.5.2    
#[81] later_1.3.0         venneuler_1.1-0     tzdb_0.2.0          DBI_1.1.2          
#[85] dbplyr_2.1.1        boot_1.3-28         Matrix_1.4-0        brio_1.1.3         
#[89] cli_3.2.0           mitools_2.4         parallel_4.1.1      insight_0.16.0     
#[93] igraph_1.2.11       pkgconfig_2.0.3     foreign_0.8-82      xml2_1.3.3         
#[97] estimability_1.3    rvest_1.0.2         callr_3.7.0         digest_0.6.29      
#[101] parameters_0.16.0   tm_0.7-8            cellranger_1.1.0    shiny_1.7.1        
#[105] rjson_0.2.21        nloptr_2.0.0        lifecycle_1.0.1     nlme_3.1-155       
#[109] jsonlite_1.8.0      desc_1.4.1          fansi_1.0.2         labelled_2.9.0     
#[113] pillar_1.7.0        lattice_0.20-45     fastmap_1.1.0       httr_1.4.2         
#[117] plotrix_3.8-2       pkgbuild_1.3.1      survival_3.2-13     glue_1.6.2         
#[121] remotes_2.4.2       bayestestR_0.11.5   zip_2.2.0           png_0.1-7          
#[125] stringi_1.7.6       performance_0.8.0   memoise_2.0.1    

##
R.version

#                _                           
#platform       x86_64-w64-mingw32          
#arch           x86_64                      
#os             mingw32                     
#system         x86_64, mingw32             
#status                                     
#major          4                           
#minor          1.1                         
#year           2021                        
#month          08                          
#day            10                          
#svn rev        80725                       
#language       R                           
#version.string R version 4.1.1 (2021-08-10)
#nickname       Kick Things 

Sys.setlocale('LC_ALL', locale = "English_United States.1252")

# [1] "LC_COLLATE=English_United States.1252;
# LC_CTYPE=English_United States.1252;
# LC_MONETARY=English_United States.1252;
# LC_NUMERIC=C;LC_TIME=English_United States.1252"

memory.size(max=TRUE)
memory.limit(100000)

## install packages requiring older versions/non-CRAN packages ##

# install.packages('devtools')
# devtools::install_version("fiftystater", version = "1.0.1", 
#                          repos = "http://cran.us.r-project.org")

# devtools::install_version("zipcode", version = "1.0", 
#                          repos = "http://cran.us.r-project.org")

# load libraries #
library(easypackages)
easypackages::libraries("ggmap", "maptools", "graphics", "utils", 
                        "fiftystater", "zipcode", # for mapping/GIS
                        "stats", "texreg", "sjmisc", "sjPlot", # outputting
                        "dplyr", "sp", "grDevices", "gridExtra", 
                        "stringr", # for str_count function
                        "tidyverse", "readr", # read rds data
                        "effects", "ggeffects", "readstata13", # read dta data
                        "ggthemes", "ggnewscale", # for multiple scales (ggplot2)
                        "sjlabelled",  "pscl", # for zeroinfl (Zero-Inflated Models)
                        "qdap", "car", "MASS", # for glm.nb (Negative Binomial Models)
                        "questionr", # for rename.variable function
                        "haven", "data.table", # read and convert dataframes
                        "lubridate", "timeDate") # for time/date processing
warnings()

# Caling Fonts from Windows Font Folder # 
windowsFonts(Arial    = windowsFont("TT Arial"), 
             Courier  = windowsFont("TT Courier New"),
             Times    = windowsFont("TT Times New Roman"), 
             Lucida   = windowsFont("TT Lucida Console"), 
             Helv     = windowsFont("Helvetica"), 
             Calibri  = windowsFont("TT Calibri"),
             Cambria  = windowsFont("TT Cambria"),
             Century  = windowsFont("TT Century"),
             Centaur  = windowsFont("TT Centaur"),
             Gill     = windowsFont("Gill Sans"))

#### [DefineFun] User-Defined Functions ####


##
## Functions of Outputting Effects ##
# This function converts "Effect" objects to data.frames #
allEff_toDF = function(i) {
  # function to get "AllEffects"
  Extract_allEffectsObj = function(i) {
    # count the order #
    # determine whether using 1st/2nd order term #
    ORDER_NUM = function(i) {
      a1 = sapply(i, names)
      a2 = colnames(a1)
      a3 = stringr::str_count(a2, pattern = ":")
      return(a3+1)
    }
    
    # first-order (no interaction) #
    EFF1 = function(i) {
      a1 = as.data.frame(i)
      a2 = as.data.frame(a1[1])
      a3 = a2[,1:3]
      names(a3) = c("TERM", "fit", "se")
      return(a3)
    }
    
    # second-order (interaction) & Collapse #
    
    EFF2 = function(i) {
      a1 = as.data.frame(i)
      a2 = as.data.frame(a1[1])
      a3 = a2[,3:4]
      a4 = a2[,1:2]
      
      COLLA = function(d) {paste0(d, collapse = "_")}
      a5 = apply(a4, 1, COLLA)
      a6 = cbind(a5, a3)
      
      names(a6) = c("TERM", "fit", "se")
      return(a6)
    }
    
    if (ORDER_NUM(i)>=2) {
      return(EFF2(i))
    } else {
      return(EFF1(i))
    }
  } # maybe could generalize this to higher-order interaction later...
  
  a = Extract_allEffectsObj(i)
  g = a[FALSE,]
  n = 1
  for (n in 1:length(i)) {
    g2 = Extract_allEffectsObj(i[n])
    g = rbind(g, g2)  
    n = n + 1
  }
  return(g)
} 

# retrieve Model Term Labels from model objects. #
get_MODEL_Labels = function(i) {attributes(i$terms$zero)$term.labels}

# get ZIP models' fitted values with ggemmeans. #
ZIP_fit = function(zipmodel) {
  termlist = get_MODEL_Labels(zipmodel)
  effect_each = function(i) {
    a1 = ggeffects::ggemmeans(model = zipmodel, 
                              terms = i)
    a1 = subset(a1, select = c(x, predicted))
    a1$x = as.character(a1$x) # this has to be done to avoid rbind errors (factor type)
    a1 = as.data.frame(a1) # this has to be done to avoid rbind errors (data attributes)
    return(a1)
  }
  a2 = do.call(rbind, lapply(termlist, effect_each))
  return(a2)
}

# functions for converting variable type. #
toFAC = function(i) {as.factor(as.character(i))}
toNUM = function(i) {as.numeric(as.character(i))}

# User-Defined GGplot2 theme #
theme_USER = theme_grey()+
  theme(plot.title = element_text(lineheight=2, hjust=.5, vjust=1.5),
        title=element_text(size=rel(1.5), family="Century"),
        axis.text.x = element_text(size=rel(1.5), family="Century"),
        axis.text.y = element_text(size=rel(1.5), family="Century"),
        legend.text=element_text(size=rel(1.3), family="Century"),
        legend.direction="vertical",
        legend.key = element_rect(size = 1.3, color = 'white'),
        legend.position=c(0.19,0.88), # first number;second number: 1 ->0 down
        legend.key.size = unit(0.6, "cm"))

# clean environment #
ls()
rm(list=setdiff(ls(), c("Eff_All", "event", "allEff_toDF",
                        "Paired_IndexZeroinfl", "theme_USER",
                        "ZIP_fit", "Round3", "toFAC", "toNUM", "map",
                        "get_MODEL_Labels", "Get_NB_Terms", 
                        "Paired_IndexZIP_Interact")))

#### [READ DOCA] --- for Data Cleaning --- ####


## Below is the reading data code. ##
## original DOCA data file from dta format ##
## remove the pound mark before reading in data ##

## here please enter your file path ##
## where you store the Original DoCA stata file ##

# event = foreign::read.dta("YOUR_FILE_PATH/DOCA_data_v10.dta")


##

## select vars ##
myvars = c("evmm", "evdd", "evyy", "eventid", "staten", "state1", 
           "cityn", "city1", "groups", "targd", "targnum", "targ1", 
           "claim1", "claim2","act1", "act2", "smonum", "smonamed",
           "form1", "form2", "violtype", "viold",  "deaths", 
           "police1", "police2", "police3", "police4", 
           "particex", "partices")

#
event = event[, myvars]
sapply(event, class)
rm(myvars)

#### [DCleaning] Generate CaseID and Time Vars ####

event$CASE = 1
event$CASEID = seq(1, length(event$evyy))
event$CASEID = str_pad(event$CASEID, 6, side=c("left"), pad="0")
event$CASEID = paste0("CASE_", event$CASEID)
head(event$CASEID)
event$CASEID = as.factor(event$CASEID)

# Date Variable #
event$evmm = as.character(event$evmm)
event$evdd = as.character(event$evdd)
event$evyy = as.character(event$evyy)

# use spr_pad to add leading zeros in front of dd and mm

event$year  = as.numeric(as.character(event$evyy))
event$month = str_pad(event$evmm, 2, side=c("left"), pad="0")
event$day   = str_pad(event$evdd, 2, side=c("left"), pad="0")
event$date = as.character(paste(event$year, event$month, event$day, sep="-"))
event$date = as.Date(event$date)
length((event$CASEID [is.na(event$date)]))

# generate year # 
dist_tab(event$date)
barplot(table(year(event$date)))
event$year = as.numeric(year(event$date))
hist(event$year)

# ordering Day Of Week # 

event$DayofWeek = dayOfWeek(as.timeDate(event$date))
event$DayofWeek = ordered(factor(event$DayofWeek), 
                          levels = c("Sun", 
                                     "Mon", 
                                     "Tue", 
                                     "Wed", 
                                     "Thu", 
                                     "Fri", 
                                     "Sat"))
event$DayofWeek = as.factor(event$DayofWeek)

# ordering MONTHS # 

event$month = as.character(months(as.timeDate(event$date)))
event$month = as.factor(ordered(factor(event$month), 
                                levels = c("1", "2", "3", "4",
                                           "5", "6", "7", "8",
                                           "9", "10","11","12")))
# remove entries that miss on DATE info #
event = subset(event, is.na(date)==F)

# decade #

dist_tab(event$decade)
event$decade = "60s"
event$decade = ifelse(event$year > 1969, "70s", event$decade)
event$decade = ifelse(event$year > 1979, "80s", event$decade)
event$decade = ifelse(event$year > 1989, "90s", event$decade)
dist_tab(event$decade)

#### [DCleaning] Location: State, City, Zipcodes, Coord ####

# state names #
event$STATE = as.character(event$state1)
event$STATE = ifelse(event$STATE=="Washington", "DC", event$STATE) # DC.
event$STATE = ifelse(event$STATE=="PR", NA, event$STATE) # excluding Puerto Rico
event$STATE = trimws(event$STATE) # trim whitespaces around the strings.
event$STATE = ifelse(event$STATE=="", NA, event$STATE) # excluding Empty cells

# city names#
event$CITY = as.character(event$city1)
event$CITY = trimws(event$CITY)

# paste city, state names together #
event$ADDRESS = paste(event$CITY, event$STATE, "USA", sep=", ")
tail(dist_tab(event$ADDRESS))

# merge coordinates with event data. #
# GEODATA = read.csv("YOUR_FILE_PATH/Dropbox/Data/DoCa/2350addresses.csv", header = T)
event   = merge(event, GEODATA, by=c("ADDRESS"))

#### [DCleaning] Size Variable: both numeric and categorical ####

dist_tab(event$particex)
summary(event$particex)
dist_tab(event$partices)

event$size_cat = NA
event = within(event, {
  # raw numbers #
  size_cat [particex >  0 & particex < 10] = "<10"
  size_cat [particex >= 10 & particex < 50] = "10~49"
  size_cat [particex >= 50 & particex < 100] = "50~99"
  size_cat [particex >= 100 & particex < 1000] = "100~999"
  size_cat [particex >= 1000 & particex < 10000] = "1000~9999"
  size_cat [particex >= 10000] = ">10000"
  # levels #
  size_cat [partices == 1] = "<10"
  size_cat [partices == 2] = "10~49"
  size_cat [partices == 3] = "50~99"
  size_cat [partices == 4] = "100~999"
  size_cat [partices == 5] = "1000~9999"
  size_cat [partices == 6] = ">10000"
})
dist_tab(event$size_cat)
event$size_cat =factor(event$size_cat,
                       levels = c("<10", 
                                  "10~49", 
                                  "50~99",
                                  "100~999", 
                                  "1000~9999", 
                                  ">10000"))
# size as numeric #
event$size_num = event$particex
event$size_num = ifelse(event$size_num == 0, 1, event$size_num)
event$size_num = ifelse(is.na(event$particex) & event$partices == 1, 1, event$size_num)
event$size_num = ifelse(is.na(event$particex) & event$partices == 2, 10, event$size_num)
event$size_num = ifelse(is.na(event$particex) & event$partices == 3, 50, event$size_num)
event$size_num = ifelse(is.na(event$particex) & event$partices == 4, 100, event$size_num)
event$size_num = ifelse(is.na(event$particex) & event$partices == 5, 1000, event$size_num)
event$size_num = ifelse(is.na(event$particex) & event$partices == 6, 10000, event$size_num)

#
summary(event$size_cat)

#### [DCleaning] Load USA MAP data and Clean State Labels ####

# this line is optional due to lazy data loading
data("fifty_states") 
map = fifty_states

# clean environment #
ls()
rm(list=setdiff(ls(), c("Eff_All", "event", "theme_USER",
                        "allEff_toDF", "ZIP_fit", "Round3", 
                        "Paired_IndexZeroinfl", "toNUM", 
                        "toFAC", "get_MODEL_Labels", 
                        "map","Get_NB_Terms")))

# naming the 50 states #
event$STATEid = NA
event = within(event, {
  STATEid [STATE == "AL"] = "alabama"#1
  STATEid [STATE == "AK"] = "alaska"#2
  STATEid [STATE == "AZ"] = "arizona"#3
  STATEid [STATE == "AR"] = "arkansas"#4
  STATEid [STATE == "CA"] = "california"#5
  STATEid [STATE == "CO"] = "colorado"#6
  STATEid [STATE == "CT"] = "connecticut"#7
  STATEid [STATE == "DE"] = "delaware"#8
  STATEid [STATE == "DC"] = "district of columbia"#9
  STATEid [STATE == "FL"] = "florida"#10
  STATEid [STATE == "GA"] = "georgia"#11
  STATEid [STATE == "HI"] = "hawaii"#12
  STATEid [STATE == "ID"] = "idaho"#13
  STATEid [STATE == "IL"] = "illinois"#14
  STATEid [STATE == "IN"] = "indiana"#15
  STATEid [STATE == "IA"] = "iowa"#16
  STATEid [STATE == "KS"] = "kansas"#17
  STATEid [STATE == "KY"] = "kentucky"#18
  STATEid [STATE == "LA"] = "louisiana"#19
  STATEid [STATE == "ME"] = "maine"#20
  STATEid [STATE == "MD"] = "maryland"#21
  STATEid [STATE == "MA"] = "massachusetts"#22
  STATEid [STATE == "MI"] = "michigan"#23
  STATEid [STATE == "MN"] = "minnesota"#24
  STATEid [STATE == "MS"] = "mississippi"#25
  STATEid [STATE == "MO"] = "missouri"#26
  STATEid [STATE == "MT"] = "montana"#27
  STATEid [STATE == "NE"] = "nebraska"#28
  STATEid [STATE == "NV"] = "nevada"#29
  STATEid [STATE == "NH"] = "new hampshire"#30
  STATEid [STATE == "NJ"] = "new jersey"#31
  STATEid [STATE == "NM"] = "new mexico"#32
  STATEid [STATE == "NY"] = "new york"#33
  STATEid [STATE == "NC"] = "north carolina"#34
  STATEid [STATE == "ND"] = "north dakota"#35
  STATEid [STATE == "OH"] = "ohio"#36
  STATEid [STATE == "OK"] = "oklahoma"#37
  STATEid [STATE == "OR"] = "oregon"#38
  STATEid [STATE == "PA"] = "pennsylvania"#39
  STATEid [STATE == "RI"] = "rhode island"#40
  STATEid [STATE == "SC"] = "south carolina"#41
  STATEid [STATE == "SD"] = "south dakota"#42
  STATEid [STATE == "TN"] = "tennessee"#43
  STATEid [STATE == "TX"] = "texas"#44
  STATEid [STATE == "UT"] = "utah"#45
  STATEid [STATE == "VT"] = "vermont"#46
  STATEid [STATE == "VA"] = "virginia"#47
  STATEid [STATE == "WA"] = "washington"#48
  STATEid [STATE == "WV"] = "west virginia"#49
  STATEid [STATE == "WI"] = "wisconsin"#50
  STATEid [STATE == "WY"] = "wyoming"#51
})
#
event$STATE  = as.factor(event$STATE)
event$STATEid = as.factor(event$STATEid)
dist_tab(event$STATEid)
map$id = as.factor(map$id)
dist_tab(map$id)

#### [DCleaning] United States courts of appeals #############


# 0 DC
# 1 Boston
# 2 New York
# 3 Philly
# 4 Richmond + DC
# 5 New Orleans
# 6 Cincinnati
# 7 Chicago
# 8 St Louis
# 9 San Franscisco
# 10 Denver
# 11 Atlanta

# 1 boston (4)	
USCA_List01 = c("maine",
                "massachusetts",
                "new hampshire",
                "rhode island")
# 2 new york (3)	",
USCA_List02 = c("connecticut",
                "new york",
                "vermont")
# 3 philly (4)	",
USCA_List03 = c("delaware",
                "new jersey",
                "pennsylvania")
# 4 richmond + 0 dc (7)	",
USCA_List04 = c("district of columbia",
                "maryland",
                "north carolina",
                "south carolina",
                "virginia",
                "west virginia")
# 5 new orleans (3)	",
USCA_List05 = c("louisiana",
                "mississippi",
                "texas")
# 6 cincinnati (4)	",
USCA_List06 = c("kentucky",
                "michigan",
                "ohio",
                "tennessee")
# 7 chicago (3)	",
USCA_List07 = c("illinois",
                "indiana",
                "wisconsin")
# 8 st louis (7)	",
USCA_List08 = c("arkansas",
                "iowa",
                "minnesota",
                "missouri",
                "nebraska",
                "north dakota",
                "south dakota")
# 9 san franscisco (9)	",
USCA_List09 = c("alaska",
                "arizona",
                "california",
                "hawaii",
                "idaho",
                "montana",
                "nevada",
                "oregon",
                "washington")
# 10 denver (6)	",
USCA_List10 = c("colorado",
                "kansas",
                "new mexico",
                "oklahoma",
                "utah",
                "wyoming")
# 11 atlanta (3)	",
USCA_List11 = c("alabama",
                "florida",
                "georgia")
USCA_all = c(USCA_List01, USCA_List02, USCA_List03, USCA_List04, USCA_List05, 
             USCA_List06, USCA_List07, USCA_List08, USCA_List09, USCA_List10, 
             USCA_List11)

## double checking the list are MECE ##
# list2 = unique(as.character(event$STATEid))
# intersect(list2, USCA_all)
# setdiff(list2, USCA_all)

# generate USA Courts of Appeals #
event$USCA = NA
event = within(event, {
  USCA [STATEid %in% USCA_List01] = "USCA_01"
  USCA [STATEid %in% USCA_List02] = "USCA_02"
  USCA [STATEid %in% USCA_List03] = "USCA_03"
  USCA [STATEid %in% USCA_List04] = "USCA_04"
  USCA [STATEid %in% USCA_List05] = "USCA_05"
  USCA [STATEid %in% USCA_List06] = "USCA_06"
  USCA [STATEid %in% USCA_List07] = "USCA_07"
  USCA [STATEid %in% USCA_List08] = "USCA_08"
  USCA [STATEid %in% USCA_List09] = "USCA_09"
  USCA [STATEid %in% USCA_List10] = "USCA_10"
  USCA [STATEid %in% USCA_List11] = "USCA_11"
})
dist_tab(event$STATEid)
dist_tab(event$USCA)

#### [DCleaning] Recoding: Claims & Forms ####

# claims #
event$claim_cat = as.numeric(as.character(event$claim1))
fivenum(event$claim_cat)
dist_tab(event$claim_cat)
event$claim_recat = NA
event = within(event, {
  claim_recat [claim_cat >= 200]  = "Anti-Nuclear"
  claim_recat [claim_cat >= 300]  = "Anti-Immigrant"
  claim_recat [claim_cat >= 400]  = "Anti-Globalization/Corporate Power"
  claim_recat [claim_cat >= 600]  = "Feminist"
  claim_recat [claim_cat >= 700]  = "Peace Movements"
  
  claim_recat [claim_cat >= 1000] = "Human Rights/Democratization"
  claim_recat [claim_cat >= 1100] = "Green/Environment"
  claim_recat [claim_cat >= 1300] = "Policy"
  claim_recat [claim_cat >= 1400] = "Labour"
  claim_recat [claim_cat >= 1500] = "African-American Rights"
  
  claim_recat [claim_cat >= 1600] = "LGBT"
  claim_recat [claim_cat >= 1700] = "Aboriginal"
  claim_recat [claim_cat >= 1800] = "Latino Rights"
  claim_recat [claim_cat >= 1900] = "Asian/Other Minorities"
  
  claim_recat [claim_cat >= 2000] = "Latino Rights"
  claim_recat [claim_cat >= 2100] = "Disabled"
  claim_recat [claim_cat >= 2200] = "Labour"
  
  claim_recat [claim_cat >= 2300] = "Asian/Other Minorities"
  claim_recat [claim_cat >= 2400] = "Feminist"
  claim_recat [claim_cat >= 2500] = "Anti-Minorities"
  claim_recat [claim_cat >= 2600] = "Animal Rights"
  claim_recat [claim_cat >= 2900] = "Asian/Other Minorities"
})
dist_tab(event$claim_recat)

#
event$claim_recat2 = NA
event = within(event, {
  claim_recat2 [claim_recat == "Policy"]   = "Policy"
  
  claim_recat2 [claim_recat == "Feminist"] = "Gender_Sex"
  claim_recat2 [claim_recat == "LGBT"]     = "Gender_Sex"
  claim_recat2 [claim_recat == "Disabled"] = "Gender_Sex"
  
  claim_recat2 [claim_recat == "African-American Rights"]= "Race_Ethnic"
  claim_recat2 [claim_recat == "Latino Rights"]          = "Race_Ethnic"
  claim_recat2 [claim_recat == "Asian/Other Minorities"] = "Race_Ethnic"
  claim_recat2 [claim_recat == "Aboriginal"]             = "Race_Ethnic"
  claim_recat2 [claim_recat == "Anti-Minorities"]        = "Race_Ethnic"
  claim_recat2 [claim_recat == "Anti-Immigrant"]         = "Race_Ethnic"
  
  claim_recat2 [claim_recat == "Labour"]                 = "Labour"
  
  claim_recat2 [claim_recat == "Anti-Globalization/Corporate Power"] = "Peace_Green"
  claim_recat2 [claim_recat == "Peace Movement"]    = "Peace_Green"
  claim_recat2 [claim_recat == "Human Rights/Democratization"] = "Peace_Green"
  claim_recat2 [claim_recat == "Green/Environment"] = "Peace_Green"
  claim_recat2 [claim_recat == "Anti-Nuclear"]      = "Peace_Green"
  claim_recat2 [claim_recat == "Animal Rights"]     = "Peace_Green"
})

# clean and relevel forms #

event$form_cat = as.numeric(as.character(event$form1))
fivenum(event$form_cat)
dist_tab(event$form_cat)
event$form_recat = NA
event = within(event, {
  form_recat [form_cat == 1]  = "Rally/March"
  form_recat [form_cat == 2]  = "Rally/March"
  form_recat [form_cat == 3]  = "Picket/Strike"
  form_recat [form_cat == 4]  = "Picket/Strike"
  form_recat [form_cat == 5]  = "Picket/Strike"
  form_recat [form_cat == 6]  = "Drama/Ceremony/Other"
  form_recat [form_cat == 7]  = "Drama/Ceremony/Other"
  form_recat [form_cat == 8]  = "Rally/March"
  form_recat [form_cat == 9]  = "Legal/Institution"
  form_recat [form_cat == 10] = "Drama/Ceremony/Other"
  form_recat [form_cat == 11] = "Violence/Conflict"
  form_recat [form_cat == 12] = "Violence/Conflict"
  form_recat [form_cat == 13] = "Picket/Strike"
  form_recat [form_cat == 14] = "Picket/Strike"
  form_recat [form_cat == 15] = "Legal/Institution"
  form_recat [form_cat == 16] = "Legal/Institution"
  form_recat [form_cat == 17] = "Violence/Conflict"
  form_recat [form_cat == 18] = "Legal/Institution"
  form_recat [form_cat == 20] = "Drama/Ceremony/Other"
  form_recat [form_cat == 98] = "Drama/Ceremony/Other"
  form_recat [form_cat == 99] = "Drama/Ceremony/Other"
  form_recat [form2 == 5]     = "Picket/Strike"
})
dist_tab(event$form_recat)

#### [DCleaning] Recoding: POP, SMOs, Police, Violence ####

# How many population groups involved? #
dist_tab(event$groups)
event$POPs = event$groups
event$POPs = ifelse(event$POPs == 0, 1, event$POPs)
event$POPs = ifelse(event$POPs >=10, 10, event$POPs)
fivenum(event$POPs)

# How many SMOs involved? #
dist_tab(event$smonum)
dist_tab(event$smonamed)


event$SMOs = event$smonum

event$SMOs = ifelse(is.na(event$smonum), 
                    0, event$SMOs)

event$SMOs = ifelse(is.na(event$smonum) & event$smonamed ==1,
                    1, event$SMOs)

event$SMOs = ifelse(is.na(event$smonum) & event$smonamed ==0,
                    0, event$SMOs)

event$SMOs = ifelse(is.na(event$SMOs), 0, event$SMOs)

event$SMOs = ifelse(event$SMOs >=10, 10, event$SMOs)

dist_tab(event$SMOs)

# [DCleaning] Policing and Violence #

dist_tab(event$police1)
dist_tab(event$police2)
dist_tab(event$police3)
dist_tab(event$police4)

event$POLs = NA
sapply(event, class)

event$police1 = as.numeric(as.character(event$police1))
event$police2 = as.numeric(as.character(event$police2))
event$police3 = as.numeric(as.character(event$police3))
event$police4 = as.numeric(as.character(event$police4))

event$POLs = rowSums(event[c("police1", "police2", 
                             "police3", "police4")], 
                     na.rm = TRUE)

dist_tab(event$POLs) # Level of policing.

#### [DCleaning] Reorder Factor Vars, Change Var Types ####

names(event)
event2 = subset(event, select = c(decade, year, month, 
                                  DayofWeek, ADDRESS,
                                  date, staten, STATEid, 
                                  CASEID, LON, LAT, CITY, STATE,
                                  size_cat, size_num, viold, 
                                  claim_recat2, 
                                  form_recat, POPs, SMOs, POLs, 
                                  USCA))

event2$RID = formatC(seq(1, length(event2$ADDRESS)), width = 6, flag = "0")
event2$RID = paste0("R_", event2$RID)
event2$RID = factor(event2$RID)

## Checking the frequencies of variables ##

# dist_tab(event2$year)
# dist_tab(event2$month)
# dist_tab(event2$DayofWeek)
# dist_tab(event2$decade)

## date and decade ##
event2$decade = factor(event2$decade, 
                       levels = c("60s", "70s",
                                  "80s", "90s"))

event2$date = as.Date(as.character(event2$date))

## Checking the frequencies of size, POPs, SMOs ##
# dist_tab(event2$size_cat)
# dist_tab(event2$size_num)
# dist_tab(event2$POPs)
# dist_tab(event2$SMOs)

## relevels claim and form ##
dist_tab(event2$claim_recat2)
event2$claim_recat2 = factor(event2$claim_recat2, levels = c("Labour",
                                                             "Gender_Sex",
                                                             "Race_Ethnic",
                                                             "Peace_Green",
                                                             "Policy"))
dist_tab(event2$form_recat)
event2$form_recat = factor(event2$form_recat,
                           levels = c("Picket/Strike", 
                                      "Rally/March", 
                                      "Drama/Ceremony/Other", 
                                      "Violence/Conflict", 
                                      "Legal/Institution"))

# Variable Type Conversion #

# functions for converting back and forth. #
# use string during the conversion to avoid NAs. #
toFac = function(x) {as.factor(as.character(x))}
toNum = function(x) {as.numeric(as.character(x))}

event2$DECADE = toFac(event2$decade)
event2$YEAR   = toFac(event2$year)
event2$VIO    = toFac(event2$viold)
event2$RID    = toFac(event2$RID)
event2$CLAIM  = toFac(event2$claim_recat2)
event2$FORM   = toFac(event2$form_recat)
event2$DATE   = toFac(as.character(event2$date)) # date has to be factor, not DATE in AMELIA
event2$POP    = toFac(event2$POPs)
event2$SMO    = toFac(event2$SMOs)
event2$POL    = toFac(event2$POLs)
event2$SIZE   = toFac(event2$size_num)

# select variables #
names(event2)
sapply(event2, class)
sapply(event2, summary)
event3 = subset(event2, select = c(CASEID, RID, DATE, 
                                   STATEid, LON, LAT,
                                   SIZE, FORM, POP, SMO, 
                                   POL, VIO, USCA))
event3$SIZE = toNum(event3$SIZE)
event3$SIZE = toFac(event3$SIZE)

# extract one cleaned data set #
event4 = na.exclude(event3)

# check variables and var types #
names(event4)
sapply(event4, class)

# size #
event4$SIZE = toNum(event4$SIZE)

# set levels of values. #

event4$FORM = factor(event4$FORM,
                     levels = c("Picket/Strike", 
                                "Rally/March", 
                                "Drama/Ceremony/Other", 
                                "Violence/Conflict", 
                                "Legal/Institution"))

# set var types.#
event4$POP = toNum(event4$POP)
event4$SMO = toNum(event4$SMO)
event4$POL = toNum(event4$POL)
event4$VIO = toNum(event4$VIO)

# checking outputs.#
tail(dist_tab(event4$POP))
tail(dist_tab(event4$SMO))
tail(dist_tab(event4$POL))
tail(dist_tab(event4$VIO))

#### [SetupData] Count: t = 1 to 90 days and Looping ####

# case have to be numeric to be counted;
# date has to be "as.Date" to be counted/calculate difference
event4$CASE = 1
head(event4$DATE)
event4$DATE = as.Date(as.character(event4$DATE))

# GENERATE EVENT2 from EVENT4 to be counted. # 
event5 = subset(event4, 
                select = c(RID, 
                           STATEid, 
                           DATE, 
                           CASE, 
                           VIO))

## from here. ##

# USCA: A sensitivity test for re-collapsing the groups by USCA #
#event5 = subset(event4, 
#                select = c(RID, 
#                           USCA, 
#                           DATE, 
#                           CASE, 
#                           VIO))

event5 = na.exclude(event5)

# DEFINE FUNCTION #
DIFFUSE_COUNT = function(i) {
  a = i[n,]
  b = a$DATE
  # c = a$LON
  # d = a$LAT
  e = a$STATEid
  f = a$RID
  
  PreAct01 = sum(i$CASE [i$DATE < b  & i$DATE >= b - 1 &
                           i$STATEid == e], na.rm = T)
  PreAct03 = sum(i$CASE [i$DATE < b  & i$DATE >= b - 3 &
                           i$STATEid == e], na.rm = T)
  PreAct07 = sum(i$CASE [i$DATE < b  & i$DATE >= b - 7 &
                           i$STATEid == e], na.rm = T)
  PreAct14 = sum(i$CASE [i$DATE < b & i$DATE >= b - 14 &
                           i$STATEid == e], na.rm = T)
  PreAct30 = sum(i$CASE [i$DATE < b & i$DATE >= b - 30 &
                           i$STATEid == e], na.rm = T)
  PreAct45 = sum(i$CASE [i$DATE < b & i$DATE >= b - 45 &
                           i$STATEid == e], na.rm = T)
  PreAct60 = sum(i$CASE [i$DATE < b & i$DATE >= b - 60 &
                           i$STATEid == e], na.rm = T)
  PreAct90 = sum(i$CASE [i$DATE < b & i$DATE >= b - 90 &
                           i$STATEid == e], na.rm = T)
  
  PostAct01 = sum(i$CASE [i$DATE <= b + 1  & i$DATE > b &
                            i$STATEid == e], na.rm = T)
  PostAct03 = sum(i$CASE [i$DATE <= b + 3  & i$DATE > b &
                            i$STATEid == e], na.rm = T)
  PostAct07 = sum(i$CASE [i$DATE <= b + 7  & i$DATE > b &
                            i$STATEid == e], na.rm = T)
  PostAct14 = sum(i$CASE [i$DATE <= b + 14 & i$DATE > b &
                            i$STATEid == e], na.rm = T)
  PostAct30 = sum(i$CASE [i$DATE <= b + 30 & i$DATE > b &
                            i$STATEid == e], na.rm = T)
  PostAct45 = sum(i$CASE [i$DATE <= b + 45 & i$DATE > b &
                            i$STATEid == e], na.rm = T)
  PostAct60 = sum(i$CASE [i$DATE <= b + 60 & i$DATE > b &
                            i$STATEid == e], na.rm = T)
  PostAct90 = sum(i$CASE [i$DATE <= b + 90 & i$DATE > b &
                            i$STATEid == e], na.rm = T)
  
  mydata = data.frame(f,
                      PreAct01, PreAct03, PreAct07, PreAct14,
                      PreAct30, PreAct45, PreAct60, PreAct90,
                      PostAct01, PostAct03, PostAct07, PostAct14, 
                      PostAct30, PostAct45, PostAct60, PostAct90)
  return(mydata)
}

# set empty data set #
n = 1
g1 = DIFFUSE_COUNT(event5)
g = g1[FALSE,] 


## Counting and Looping - Please remove pound signs when running ##

# for (n in 1:length(event5$RID)){
# g1 = DIFFUSE_COUNT(event5)
# g = rbind(g, g1)
# }

# RENAME.VAR in QUESTIONR - merge: counts and original data file#

g = questionr::rename.variable(g, "f", "RID")
event5 = merge(event4, g, by = c("RID"), all.x = T)
event6 = event5
fivenum(event6$PostAct60)

#### [SetupData] Governors Data: Match Events/Gov By States/Date ####

## read in Governor Data ##
## please enter your own path where you ##
## store the gov/president info excel file ##


# GOVdata = readxl::read_xlsx("YOUR_FILE_PATH/Dropbox/Data/USAgov/1960-1995GOV.xlsx")



# clean the environment #
rm(list=setdiff(ls(), 
                c("GOVdata", "event6")))

# state match #
t1 = unique(GOVdata$STATE)
t1 = tolower(t1)
t2 = unique(as.character(event6$STATEid))

# state correction #
GOVdata$STATE = tolower(as.character(GOVdata$STATE))
event6$STATE = tolower(as.character(event6$STATEid))
event6$STATE = ifelse(event6$STATE == "district of columbia",
                      "d.c.", event6$STATE)

## Match Time Spread Out Governor Data to Match ##

GOVdata$var1 = substr(GOVdata$inFullDate, 6,
                      nchar(GOVdata$inFullDate))
GOVdata$var2 = substr(GOVdata$outFullDate, 6,
                      nchar(GOVdata$outFullDate))

#
GOVdata$NAME [GOVdata$PARTY == "West"]

# independent/local parties converted to mainstream #
# parties according to their preference #
GOVdata = within(GOVdata, {
  PARTY [PARTY == "A Connecticut Party"] = "Republican"
  PARTY [PARTY == "AlaskanIndependence"] = "Other"
  PARTY [PARTY == "Dem"] = "Democratic"
  PARTY [PARTY == "Democratic_NPL"] = "Democratic"
  PARTY [PARTY == "Democratic_Farmer_Labor"] = "Democratic"
  PARTY [PARTY == "Independent"] = "Other"
  PARTY [PARTY == "Independent_Republican"] = "Republican"
  PARTY [PARTY == "Independent_Republican/Republican"] = "Republican"
  PARTY [PARTY == "Rep"] = "Republican"
  PARTY [NAME  == "Richard A. Snelling"] = "Republican"
  PARTY [NAME  == "Philip H. Hoff"] = "Democratic"
  PARTY [NAME  == "Madeleine Kunin"] = "Democratic"
}) 

# spread out the date range. #
GOVdata2 = data.table::setDT(GOVdata)[,
                                      .(CaseID = CaseID, 
                                        DATE = seq(as.Date(var1), 
                                                   as.Date(var2)-1, 
                                                   by = "day")),
                                      by = 1:nrow(GOVdata)]
names(GOVdata2) = c("nrow", "CaseID", "DATE")
GOVdata3 = merge(GOVdata2, GOVdata, 
                 by = "CaseID")
GOVdata3 = subset(GOVdata3, 
                  select = c(CaseID, DATE, STATE, PARTY))
names(GOVdata3) = c("GOVid", "DATE", 
                    "STATE", "PARTY")

# merging #
names(event6)
event7 = subset(event6, 
                select = c(CASEID, DATE, STATE))

# this is generating GOV-PROTEST DATA #
event8 = merge(event7,  GOVdata3, 
               by = c("DATE", "STATE"))
event8 = subset(event8, 
                select = c(CASEID, PARTY))
names(event8) = c("CASEID", 
                  "GPA")
event9 = merge(event6, event8, 
               by = c("CASEID"))

# specify variable type #
event9$GPA = factor(event9$GPA)
dist_tab(event9$GPA)
sapply(event9, class)

#### [SetupData] Generating POTUS-PROTEST DATA #####

# spread out the date range. #
GOVdata4 = GOVdata[GOVdata$STCODE == "POTUS"]
GOVdata5 = data.table::setDT(GOVdata4)[,
                                       .(CaseID = CaseID, 
                                         DATE = seq(as.Date(var1), 
                                                    as.Date(var2)-1, 
                                                    by = "day")),
                                       by = 1:nrow(GOVdata4)]

# GOVdata5 = 17532 days, about 48 years of presi term #

names(GOVdata5) = c("nrow", "CaseID", "DATE")
GOVdata6 = merge(GOVdata5, GOVdata, by = "CaseID")
GOVdata6 = subset(GOVdata6, select = c(CaseID, DATE, STATE, PARTY))
names(GOVdata6) = c("POTUSid", "DATE", "STATE", "PARTY")
event10 = subset(event6, select = c(CASEID, DATE, STATE))

# this is generating POTUS-PROTEST DATA #
event11 = merge(event10,  GOVdata6, by = c("DATE"))
event11 = subset(event11, select = c(CASEID, PARTY))
names(event11) = c("CASEID", "PPA")
event = merge(event9, event11, by = c("CASEID"))
event = na.exclude(event)

# PPA / STATE convert to factor var #
event$PPA = factor(event$PPA)
event$STATE = factor(event$STATE)

# [After Above Cleaning] FINALIZED MERGED DATA RDS WRITTEN #
# write_rds(event, compress = "gz",
#           "d:/event_20220309_Allmerged_90days.rds")

#### [Analysis:] Read Cleaned and Merged Data ####

## read data ##
# Select Date Range: 19600401-19951001 inclusive. #
# this is generated from original DOCA data (19600101 - 19951231) #
# by counting pre-event and post-event occurrences #
# and re-write into a new data file #

## Please enter your file path where you store the data file ##
# event = read_rds("YOUR_FILE_PATH/Dropbox/Data/DoCa/event_20220309_Allmerged_90days.rds")

# limit Date range to only (19600401 to 19951001) # 
# according to the temporal range: max = 90 days #
# this step has to be after the DV calculation # 
# (Pre- Post- Looping) on original data file #

list1 = seq(as.Date("1960-04-01"), as.Date("1995-10-01"), 1)
event = event[event$DATE %in% list1,] # 18951 cases remain.


## relabel some variables / re-level some variables ##

# GPA abbreviated #

sjmisc::frq(event$GPA)
event$GPA = ifelse(event$GPA=="Republican", "R", "D")
event$GPA = factor(event$GPA, levels = c("D", "R"))
sjmisc::frq(event$GPA)

# PPA abbreviated #

event$PPA = ifelse(event$PPA=="Republican", "R", "D")
event$PPA = factor(event$PPA, levels = c("D", "R"))
event$PPA = ifelse(event$PPA == "D", "Dem_Potus", "Rep_Potus")
sjmisc::frq(event$PPA)

# functions for converting back and forth. #
toFAC = function(x) {as.factor(as.character(x))}
toNUM = function(x) {as.numeric(as.character(x))}

# Find which variables are numeric #
List_toFac = c("CASEID", "RID", "STATEid", "FORM",
               "STATE", "GPA", "PPA")
# Convert non-factor ones to factor #
event[List_toFac] = lapply(event[List_toFac], toFAC)

#
event$VIO_F = toFAC(event$VIO)
event$POL_F = ifelse(event$POL > 0, "Policing", "No Policing")
event$POL_F = toFAC(event$POL_F)
sjmisc::frq(event$POL_F)

event$POP_F = ifelse(event$POP > 1, "Multi-Groups",
                     "Single-Group")
event$POP_F = factor(event$POP_F, 
                     levels = c("Single-Group",
                                "Multi-Groups"))
qdap::dist_tab(event$POP_F)

# size-f #

event$SIZE_F = NA
hist(event$SIZE)
fivenum(event$SIZE)
event = within(event, {
  SIZE_F [SIZE >  0   ] = "<10"
  SIZE_F [SIZE >= 10  ] = "10~49"
  SIZE_F [SIZE >= 50  ] = "50~99"
  SIZE_F [SIZE >= 100 ] = "100~999"
  SIZE_F [SIZE >= 1000] = "1000+"
})
dist_tab(event$SIZE_F)
barplot(table(event$SIZE_F))
event$SIZE_F = factor(event$SIZE_F,
                      levels = c("<10", "10~49", "50~99", 
                                 "100~999", "1000+"))
sjmisc::frq(event$SIZE_F)

# SMO-f #
event$SMO_F = NA
event = within(event, {
  SMO_F [SMO == 0 ] = "NO SMO"
  SMO_F [SMO == 1 ] = "ONE SMO"
  SMO_F [SMO >= 2 ] = "SMOs"
})
dist_tab(event$SMO_F)
event$SMO_F =factor(event$SMO_F)

# generate year/DECADE from DATE #

event$YEAR = year(event$DATE)
event$DECADE = NA
event = within(event, {
  DECADE [event$YEAR >= 1960 & event$YEAR<1970] = "60s"
  DECADE [event$YEAR >= 1970 & event$YEAR<1980] = "70s"
  DECADE [event$YEAR >= 1980 & event$YEAR<1990] = "80s"
  DECADE [event$YEAR >= 1990 & event$YEAR<2000] = "90s"
})
event$DECADE = factor(event$DECADE)
event$YEAR = factor(event$YEAR)

# generate presidential terms #
event$potus = NA
event = within(event, {
  potus [event$DATE < as.Date("2001-01-20")] = "09.Clinton"
  potus [event$DATE < as.Date("1993-01-20")] = "08.Bush"
  potus [event$DATE < as.Date("1989-01-20")] = "07.Reagan"
  potus [event$DATE < as.Date("1981-01-20")] = "06.Carter"
  potus [event$DATE < as.Date("1977-01-20")] = "05.Ford"
  potus [event$DATE < as.Date("1974-08-09")] = "04.Nixon"
  potus [event$DATE < as.Date("1969-01-20")] = "03.Johnson"
  potus [event$DATE < as.Date("1963-11-22")] = "02.JFK"
  potus [event$DATE < as.Date("1961-01-20")] = "01.DDE"
})
frq(event$potus)
event$potus = factor(event$potus)

event$potus_merge = NA
event = within(event, {
  potus_merge [potus == "01.DDE"]     = "1.DDE_JFK"
  potus_merge [potus == "02.JFK"]     = "1.DDE_JFK"
  potus_merge [potus == "03.Johnson"] = "2.Johnson"
  potus_merge [potus == "04.Nixon"]   = "3.Nixon_Ford"
  potus_merge [potus == "05.Ford"]    = "3.Nixon_Ford"
  potus_merge [potus == "06.Carter"]  = "4.Carter"
  potus_merge [potus == "07.Reagan"]  = "5.Reagan"
  potus_merge [potus == "08.Bush"]    = "6.Bush_Clinton"
  potus_merge [potus == "09.Clinton"] = "6.Bush_Clinton"
})
event$potus_merge = factor(event$potus_merge)

# generate log size #
fivenum(event$SIZE) # size is rounded down to nearest 10.
event$SIZE = ifelse(event$SIZE==0, 1, event$SIZE)
event$LOGSIZE = log10(event$SIZE)

# MONTH #
event$MONTH = month(event$DATE)
event$MONTH = toFAC(event$MONTH)

#### [Analysis:] [Prepare] Table One: Descriptive Information for Vars ####

#
NUMs = c('PreAct01', 'PostAct01', 
         'PreAct03', 'PostAct03', 
         'PreAct07', 'PostAct07', 
         'PreAct14', 'PostAct14',
         'PreAct30', 'PostAct30', 
         'PreAct45', 'PostAct45', 
         'PreAct60', 'PostAct60', 
         'PreAct90', 'PostAct90')

# MEAN/SD values of COUNTS #
a = sapply(event[NUMs], mean)
a = (round(a,2))
b = sapply(event[NUMs], sd)
b = (round(b,2))
c = cbind(NUMs, a, b)
c = data.table(c)
c
c$c = paste0(c$a, "(", c$b, ")")
c$c

# FREQ/PCT values of CATEGORICAL VARS #
names(event)
CATs = c("DECADE", "SIZE_F", "FORM",
         "SMO_F", "VIO_F", "POL_F",
         "PPA", "GPA")

d = sapply(event[CATs], dist_tab)
d = do.call(rbind.data.frame, d)
d = cbind(rownames(d), d)
names(d) = c("Var", "Term", "Freq", "Cumu", "Pct", "CumuPct")
d = subset(d, select = c(Var, Term, Freq, Pct))
d$Pct = paste0(d$Pct, "%")

# output the descriptives into CSV file #
write_csv(c, "D:/DOCA_Table1A.csv")
write_csv(d, "D:/DOCA_Table1B.csv")

#### [Analysis:] [Prepare] Load Customized Functions ####

allEff_toDF = function(i) {
  # function to get "AllEffects"
  Extract_allEffectsObj = function(i) {
    # count the order #
    # determine whether using 1st/2nd order term #
    ORDER_NUM = function(i) {
      a1 = sapply(i, names)
      a2 = colnames(a1)
      a3 = stringr::str_count(a2, pattern = ":")
      return(a3+1)
    }
    
    # first-order (no interaction) #
    EFF1 = function(i) {
      a1 = as.data.frame(i)
      a2 = as.data.frame(a1[1])
      a3 = a2[,1:3]
      names(a3) = c("TERM", "fit", "se")
      return(a3)
    }
    
    # second-order (interaction) & Collapse #
    
    EFF2 = function(i) {
      a1 = as.data.frame(i)
      a2 = as.data.frame(a1[1])
      a3 = a2[,3:4]
      a4 = a2[,1:2]
      
      COLLA = function(d) {paste0(d, collapse = "_")}
      a5 = apply(a4, 1, COLLA)
      a6 = cbind(a5, a3)
      
      names(a6) = c("TERM", "fit", "se")
      return(a6)
    }
    
    if (ORDER_NUM(i)>=2) {
      return(EFF2(i))
    } else {
      return(EFF1(i))
    }
  } # maybe we could generalize this to higher-order interaction
  
  a = Extract_allEffectsObj(i)
  g = a[FALSE,]
  n = 1
  for (n in 1:length(i)) {
    g2 = Extract_allEffectsObj(i[n])
    g = rbind(g, g2)  
    n = n + 1
  }
  return(g)
} 

#
get_MODEL_Labels = function(i) {attributes(i$terms$zero)$term.labels}

ZIP_fit = function(zipmodel) {
  termlist = get_MODEL_Labels(zipmodel)
  effect_each = function(i) {
    a1 = ggeffects::ggemmeans(model = zipmodel, 
                              terms = i)
    a1 = subset(a1, select = c(x, predicted))
    a1$x = as.character(a1$x) # this has to be done to avoid rbind errors (factor type)
    a1 = as.data.frame(a1) # this has to be done to avoid rbind errors (data attributes)
    return(a1)
  }
  a2 = do.call(rbind, lapply(termlist, effect_each))
  return(a2)
}

Get_NB_Terms = function(i) {
  a1 = allEff_toDF(allEffects(i))
  a2 = as.character(a1$TERM)
  return(a2)
}

Paired_IndexZeroinfl = function(c, d, a, b) {
  # c & d: glm models (pre, post)
  # a & b: zeroinfl models (pre, post)
  # get predicted values from Zero-infl models #
  
  termlist = Get_NB_Terms(c)
  n1 = ZIP_fit(a)
  n2 = ZIP_fit(b)
  PRE = n1$predicted
  POST= n2$predicted
  fit = log(POST/PRE)
  ratio = exp(fit)
  
  # get matrix from glm.nb models #
  e1 = allEffects(c)
  e2 = allEffects(d)
  getMat = function(i) {
    d = i$model.matrix
    return(d)
  }
  mat1 = do.call(rbind, lapply(e1, getMat))
  mat2 = do.call(rbind, lapply(e2, getMat))
  
  # get vcov #
  a1 = vcov(a)
  a2 = vcov(b)
  cov1 = a1[1:(0.5*sqrt(length(a1))),1:(0.5*sqrt(length(a1)))]
  cov2 = a2[1:(0.5*sqrt(length(a2))),1:(0.5*sqrt(length(a2)))]
  
  # 4. Calculate s.e from model matrix and vcov 
  v.d = mat1%*%cov1%*%t(mat1)+mat2%*%cov2%*%t(mat2)
  se = sqrt(diag(v.d))
  upper = exp(fit + 1.96*se)
  lower = exp(fit - 1.96*se)
  model = deparse(substitute(a))
  
  ci.d = data.frame(cbind(model, termlist, 
                          PRE, POST, 
                          ratio, se, 
                          lower, upper))
  return(ci.d)
}


Paired_IndexZIP_Interact = function(c, d, a, b, term_formula) {
  # c & d: glm models (pre, post)
  # a & b: zeroinfl models (pre, post)
  # get predicted values from Zero-infl models #
  n1 = emmeans::emmeans(a, specs = eval(parse(text = term_formula)))
  n2 = emmeans::emmeans(b, specs = eval(parse(text = term_formula)))
  k1 = as.data.frame(n1$emmeans)
  k2 = as.data.frame(n2$emmeans)
  
  k1$term = paste0(k1[,1], "_", k1[,2])
  k2$term = paste0(k2[,1], "_", k2[,2])
  b1 = subset(k1, select = c(term, emmean))
  b2 = subset(k2, select = c(term, emmean))
  names(b1) = c("TERM", "predicted")
  names(b2) = c("TERM", "predicted")
  
  termlist = b1$TERM
  PRE = b1$predicted
  POST= b2$predicted
  fit = log(POST/PRE)
  ratio = exp(fit)
  
  # get matrix from glm.nb models #
  mat1 = do.call(rbind, lapply(allEffects(c), function(i) {return(i$model.matrix)}))
  mat2 = do.call(rbind, lapply(allEffects(d), function(i) {return(i$model.matrix)}))
  a1 = vcov(a)
  a2 = vcov(b)
  cov1 = a1[1:(0.5*sqrt(length(a1))),1:(0.5*sqrt(length(a1)))]
  cov2 = a2[1:(0.5*sqrt(length(a2))),1:(0.5*sqrt(length(a2)))]
  
  # 4. Calculate s.e from model matrix and vcov 
  v.d = mat1%*%cov1%*%t(mat1)+mat2%*%cov2%*%t(mat2)
  se = sqrt(diag(v.d))
  upper = exp(fit + 1.96*se)
  lower = exp(fit - 1.96*se)
  model = deparse(substitute(a))
  ci.d = data.frame(cbind(model, termlist, PRE, POST, ratio, se, lower, upper))
  return(ci.d)
}


toFAC = function(i) {as.factor(as.character(i))}
toNUM = function(i) {as.numeric(as.character(i))}

#### [Analysis:] [Study01] Modelling: Features of Protest ####

indep = c("SIZE_F + SMO_F + VIO_F + FORM + DECADE")
dep = c('PreAct01', 'PostAct01',
        'PreAct03', 'PostAct03',
        'PreAct07', 'PostAct07',
        'PreAct14', 'PostAct14',
        'PreAct30', 'PostAct30',
        'PreAct45', 'PostAct45',
        'PreAct60', 'PostAct60',
        'PreAct90', 'PostAct90')

# 2. Get model.matrix from glm #
z_Pre01 = pscl::zeroinfl(as.formula(paste(dep[1 ], "~", indep)), data = event, dist = "poi")
z_Post01= pscl::zeroinfl(as.formula(paste(dep[2 ], "~", indep)), data = event, dist = "poi")
z_Pre03 = pscl::zeroinfl(as.formula(paste(dep[3 ], "~", indep)), data = event, dist = "poi")
z_Post03= pscl::zeroinfl(as.formula(paste(dep[4 ], "~", indep)), data = event, dist = "poi")
z_Pre07 = pscl::zeroinfl(as.formula(paste(dep[5 ], "~", indep)), data = event, dist = "poi")
z_Post07= pscl::zeroinfl(as.formula(paste(dep[6 ], "~", indep)), data = event, dist = "poi")
z_Pre14 = pscl::zeroinfl(as.formula(paste(dep[7 ], "~", indep)), data = event, dist = "poi")
z_Post14= pscl::zeroinfl(as.formula(paste(dep[8 ], "~", indep)), data = event, dist = "poi")
z_Pre30 = pscl::zeroinfl(as.formula(paste(dep[9 ], "~", indep)), data = event, dist = "poi")
z_Post30= pscl::zeroinfl(as.formula(paste(dep[10], "~", indep)), data = event, dist = "poi")
z_Pre45 = pscl::zeroinfl(as.formula(paste(dep[11], "~", indep)), data = event, dist = "poi")
z_Post45= pscl::zeroinfl(as.formula(paste(dep[12], "~", indep)), data = event, dist = "poi")
z_Pre60 = pscl::zeroinfl(as.formula(paste(dep[13], "~", indep)), data = event, dist = "poi")
z_Post60= pscl::zeroinfl(as.formula(paste(dep[14], "~", indep)), data = event, dist = "poi")
z_Pre90 = pscl::zeroinfl(as.formula(paste(dep[15], "~", indep)), data = event, dist = "poi")
z_Post90= pscl::zeroinfl(as.formula(paste(dep[16], "~", indep)), data = event, dist = "poi")

# outputting the models #
htmlreg(list(z_Pre01, z_Pre03, z_Pre07, z_Pre14,
             z_Pre30, z_Pre45, z_Pre60, z_Pre90), 
        "D:/Study1_ZIP_pre.html")

# outputting the models #
htmlreg(list(z_Post01, z_Post03, z_Post07, z_Post14,
             z_Post30, z_Post45, z_Post60, z_Post90), 
        "D:/Study1_ZIP_Post.html")

#
m_Pre01 = glm.nb(as.formula(paste(dep[1 ], "~", indep)), data = event)
m_Post01= glm.nb(as.formula(paste(dep[2 ], "~", indep)), data = event)
m_Pre03 = glm.nb(as.formula(paste(dep[3 ], "~", indep)), data = event)
m_Post03= glm.nb(as.formula(paste(dep[4 ], "~", indep)), data = event) 
m_Pre07 = glm.nb(as.formula(paste(dep[5 ], "~", indep)), data = event)
m_Post07= glm.nb(as.formula(paste(dep[6 ], "~", indep)), data = event)
m_Pre14 = glm.nb(as.formula(paste(dep[7 ], "~", indep)), data = event)
m_Post14= glm.nb(as.formula(paste(dep[8 ], "~", indep)), data = event)
m_Pre30 = glm.nb(as.formula(paste(dep[9 ], "~", indep)), data = event)
m_Post30= glm.nb(as.formula(paste(dep[10], "~", indep)), data = event)
m_Pre45 = glm.nb(as.formula(paste(dep[11], "~", indep)), data = event)
m_Post45= glm.nb(as.formula(paste(dep[12], "~", indep)), data = event)
m_Pre60 = glm.nb(as.formula(paste(dep[13], "~", indep)), data = event)
m_Post60= glm.nb(as.formula(paste(dep[14], "~", indep)), data = event)
m_Pre90 = glm.nb(as.formula(paste(dep[15], "~", indep)), data = event)
m_Post90= glm.nb(as.formula(paste(dep[16], "~", indep)), data = event)

#

Study1_Pair_Eff_01 = Paired_IndexZeroinfl(m_Pre01, m_Post01, z_Pre01, z_Post01)
Study1_Pair_Eff_03 = Paired_IndexZeroinfl(m_Pre03, m_Post03, z_Pre03, z_Post03)
Study1_Pair_Eff_07 = Paired_IndexZeroinfl(m_Pre07, m_Post07, z_Pre07, z_Post07)
Study1_Pair_Eff_14 = Paired_IndexZeroinfl(m_Pre14, m_Post14, z_Pre14, z_Post14)
Study1_Pair_Eff_30 = Paired_IndexZeroinfl(m_Pre30, m_Post30, z_Pre30, z_Post30)
Study1_Pair_Eff_45 = Paired_IndexZeroinfl(m_Pre45, m_Post45, z_Pre45, z_Post45)
Study1_Pair_Eff_60 = Paired_IndexZeroinfl(m_Pre60, m_Post60, z_Pre60, z_Post60)
Study1_Pair_Eff_90 = Paired_IndexZeroinfl(m_Pre90, m_Post90, z_Pre90, z_Post90)

# combine the list into a data.frame #
temp  = lapply(ls(, pattern = "Study1_Pair_Eff_"), 
               get) 
temp2 = do.call(rbind, temp)

# name the terms properly #
(Get_NB_Terms(m_Pre01))
a4 = c("SIZE.<10", "SIZE.10-49", "SIZE.50-99", "SIZE.100-999", "SIZE.1000+",
       "SMO.0", "SMO.1", "SMO.2+", 
       "VIO.0", "VIO.1",
       "FORM.Drama/Ceremony/Other", "FORM.Legal/Institution",
       "FORM.PicketStrike", "FORM.Rally.March",
       "FORM.Violence/Conflict", 
       "DECADE.1960s", "DECADE.1970s", 
       "DECADE.1980s", "DECADE.1990s")
temp2
Eff_All = cbind(a4, temp2)
length(Eff_All)
names(Eff_All)[1] = c("TERMS")

# Round to 3 digits #
List_toRound = c("PRE", "POST", "ratio",
                 "se", "lower", "upper")
toFAC = function(i) {as.factor(as.character(i))}
toNUM = function(i) {as.numeric(as.character(i))}
Eff_All[List_toRound] = lapply(Eff_All[List_toRound], toNUM)
Round3 = function(i) {round(i, 3)}
Eff_All[List_toRound] = lapply(Eff_All[List_toRound], Round3)
head(Eff_All)

write_csv(Eff_All, "D:/Study1_Eff.csv")
head(Eff_All)

#### [Outputs :] [Study01] Eff plots: Features of Protest ####

# You can generate the EFF object from previous steps. ##
# Eff_All = read_csv("YOUR_FILE_PATH/Dropbox/Project/1.Pub/0DONE/202205_RSMCC_DOCA/Output/Study1_Eff.csv")
dist_tab(Eff_All$TERMS)


# labelling the terms #
a5 = c("SIZE.<10", "SIZE.10-49", "SIZE.50-99", "SIZE.100-999", "SIZE.1000+",
       "SMO.0", "SMO.1", "SMO.2+", 
       "VIO.0", "VIO.1",
       "FORM.PicketStrike", "FORM.Rally.March",
       "FORM.Drama/Ceremony/Other", "FORM.Violence/Conflict", 
       "FORM.Legal/Institution")
Eff_All = Eff_All[Eff_All$TERMS %in% a5,]
Eff_All

# functions for converting back and forth. #
toFAC = function(x) {as.factor(as.character(x))}
toNUM = function(x) {as.numeric(as.character(x))}

Eff_All$model = toFAC(Eff_All$model)
Eff_All$TERMS = toFAC(Eff_All$TERMS)

# Generate number of days #
Eff_All$day = substr(Eff_All$model, 6, 7)
Eff_All$day = as.numeric(Eff_All$day)
fivenum(Eff_All$day)
dist_tab(Eff_All$day)

# Distinguish the types of diffusion (or self-limiting) #
Eff_All$TYPE = "Non-Sig"
Eff_All$TYPE = ifelse(Eff_All$lower>1, "Diffusive", Eff_All$TYPE)
Eff_All$TYPE = ifelse(Eff_All$upper<1, "Self-Limiting", Eff_All$TYPE)
dist_tab(Eff_All$TYPE)

#
Eff_All$GROUP = "SIZE"
Eff_All = within(Eff_All, {
  GROUP [TERMS %in% c("SMO.0", "SMO.1", "SMO.2+")] = "SMO" 
  GROUP [TERMS %in% c("VIO.0", "VIO.1")] = "VIO" 
  GROUP [TERMS %in% c("FORM.PicketStrike", 
                      "FORM.Rally.March",
                      "FORM.Drama/Ceremony/Other", 
                      "FORM.Violence/Conflict", 
                      "FORM.Legal/Institution")] = "FORM" 
})
Eff_All$GROUP = factor(Eff_All$GROUP)
table(Eff_All$GROUP)

# Study_01: Effplot #

head(Eff_All)



# set the theme for plots #
theme_USER =  theme_bw() + theme(text = element_text(size = 8), 
                                 legend.background = element_rect(colour = "black", fill = "white"), 
                                 legend.margin = margin(4, 4, 4, 4), 
                                 plot.title = element_text(lineheight = 2, 
                                                           face = "bold", 
                                                           hjust = .5, 
                                                           vjust = 1.5,
                                                           size = 12) , 
                                 legend.title = element_text(size = 12) , 
                                 legend.text = element_text(size = 12), 
                                 legend.key.size = unit(1.2, "lines"), 
                                 legend.title.align = 0.5, 
                                 legend.position = c(0.86, 0.22), 
                                 legend.box.margin = margin(0, 0, 0, 0, "cm"), 
                                 axis.title=element_text(size=11,face="bold"),
                                 axis.text.y = element_text(size = 10), 
                                 axis.text.x = element_text(size = 9, angle = 58, hjust = 1 ))

# function for y-axis/x-axis alignment #
# digits to display for axis #
# label y#
scaleFUN1 = function(x) sprintf("%.1f", x)
scaleFUN2 = function(x) sprintf("%.2f", x)
scaleFUN3 = function(x) sprintf("%.2f", x)

# label y#
SCALE_y1 = scale_y_continuous(labels = scaleFUN1)
SCALE_y2 = scale_y_continuous(labels = scaleFUN2, limits = c(0.7, 1.3))
SCALE_y3 = scale_y_continuous(labels = scaleFUN3)

# label x #
SCALE_x  = scale_x_continuous(limits = c(1, 90),
                              breaks = c(1, 3, 7, 14,
                                         30,45,60,90))

# color/linetype: manual #
SCALE_COLOR3 = scale_color_manual(values = c("green", "grey23", "red3"), 
                                  labels = c("Diffusive", "Non-Sig", "Self-Limiting"),
                                  name = "Type of Diffusion")

SCALE_VIO_linetype = scale_linetype_manual(values = c("solid", "dotdash"),
                                           labels = c("Non-Violent", 
                                                      "Violent"),
                                           name = "Violence")
SCALE_VIO_color = scale_color_manual(values = c("grey77", "grey56"),
                                     labels = c("Non-Violent", 
                                                "Violent"),
                                     name = "Violence")

INVISIBLE_SCALE_COLOR3 = scale_color_manual(values = c("green", "grey23", "red3"), 
                                            labels = c("Diffusive", "Non-Sig", "Self-Limiting"),
                                            name = "Type of Diffusion",
                                            guide = "none")

INVISIBLE_SCALE_VIO_linetype = scale_linetype_manual(values = c("solid", "dotdash"),
                                                     labels = c("Non-Violent", 
                                                                "Violent"),
                                                     name = "Violence",
                                                     guide = "none")
INVISIBLE_SCALE_VIO_color = scale_color_manual(values = c("grey77", "grey56"),
                                               labels = c("Non-Violent", 
                                                          "Violent"),
                                               name = "Violence",
                                               guide = "none")


# SIZE #
b = Eff_All[Eff_All$GROUP == "SIZE",]

SCALE_SIZE_linetype = scale_linetype_manual(values = c("solid", 
                                                       "dotdash", 
                                                       "dotted",
                                                       "longdash",
                                                       "dotdash"),
                                            labels = c("less than 10", 
                                                       "10-49",
                                                       "50-99",
                                                       "100-999",
                                                       "more than 1000"),
                                            name = "Num of Participants")

#
SCALE_SIZE_color = scale_color_manual(values = c("grey88", 
                                                 "grey66", 
                                                 "grey44",
                                                 "grey22", 
                                                 "black"),
                                      labels = c("less than 10", 
                                                 "10-49",
                                                 "50-99",
                                                 "100-999",
                                                 "more than 1000"),
                                      name = "Num of Participants")

#
plot_SIZE = ggplot() + 
  geom_pointrange(b, size = rel(1.2), 
                  mapping = aes(x = day, y = ratio, 
                                color = TYPE, 
                                ymin=lower, ymax=upper,
                                width = 0.18, alpha = 0.01)) + 
  INVISIBLE_SCALE_COLOR3 + SCALE_y2 + SCALE_x + new_scale_color() +
  geom_hline(yintercept=1, linetype="dashed", color = "grey32") +
  geom_line(data = b, size = rel(0.95), 
            aes(x = day, y = ratio, group = TERMS, 
                linetype = TERMS, color = TERMS)) + 
  SCALE_SIZE_linetype + SCALE_SIZE_color +  
  theme_USER + theme(legend.position = c(0.81, 0.84)) + 
  guides(alpha = "none") +
  labs(y = "Event Diffusion Momentum (EDM Index)", 
       x = "",
       title = "Fig.1(A) Number of Participants") + 
  theme(text = element_text(size = 30))
plot_SIZE

# FORM #
b = Eff_All[Eff_All$GROUP == "FORM",]

SCALE_FORM_linetype = scale_linetype_manual(values = c("solid", 
                                                       "dotdash", 
                                                       "dotted",
                                                       "longdash",
                                                       "dotdash"),
                                            labels = c("Drama/Ceremony", 
                                                       "Legal/Institutional",
                                                       "Picket/Strike",
                                                       "Rally/March",
                                                       "Conflict"),
                                            name = "Form of Action")

SCALE_FORM_color = scale_color_manual(values = c("grey88", 
                                                 "grey66", 
                                                 "grey44",
                                                 "grey22", 
                                                 "black"),
                                      labels = c("Drama/Ceremony", 
                                                 "Legal/Institutional",
                                                 "Picket/Strike",
                                                 "Rally/March",
                                                 "Conflict"),
                                      name = "Form of Action")
plot_FORM = ggplot() + 
  geom_pointrange(b, size = rel(1.2), 
                  mapping = aes(x = day, y = ratio, 
                                color = TYPE, 
                                ymin=lower, ymax=upper,
                                width = 0.18, alpha = 0.01)) + 
  INVISIBLE_SCALE_COLOR3 + SCALE_y2 + SCALE_x + new_scale_color() +
  geom_hline(yintercept=1, linetype="dashed", color = "grey32") +
  geom_line(data = b, size = rel(0.95), 
            aes(x = day, y = ratio, group = TERMS, 
                linetype = TERMS, color = TERMS)) + 
  SCALE_FORM_linetype + SCALE_FORM_color +  
  theme_USER + theme(legend.position = c(0.81, 0.84)) + 
  guides(alpha = "none") +
  labs(y = "", 
       x = "",
       title = "Fig.1(B) Form of Action") + 
  theme(text = element_text(size = 30))
plot_FORM

# SMO #
dist_tab(Eff_All$GROUP)
b = Eff_All[Eff_All$GROUP == "SMO",]
b$TYPE
dist_tab(Eff_All$TERMS)


SCALE_SMO_linetype = scale_linetype_manual(values = c("solid", "dotdash", 
                                                      "dotted"),
                                           labels = c("No SMO", 
                                                      "One SMO",
                                                      "Multi SMOs"),
                                           name = "Num of SMO")

SCALE_SMO_color = scale_color_manual(values = c("grey71", "grey54", "grey81"),
                                     labels = c("No SMO", 
                                                "One SMO",
                                                "Multi SMOs"),
                                     name = "Num of SMO")
SCALE_COLOR2 = scale_color_manual(values=c("grey23", "red3"), 
                                  labels = c("Neutral", "Self-Limiting"),
                                  name = "Type of Diffusion")
#
plot_SMO = ggplot() + 
  geom_pointrange(b, size = rel(1.2), 
                  mapping = aes(x = day, y = ratio, 
                                color = TYPE, 
                                ymin=lower, ymax=upper,
                                width = 0.18, alpha = 0.01)) + 
  INVISIBLE_SCALE_COLOR3 + SCALE_y2 + SCALE_x + new_scale_color() +
  geom_hline(yintercept=1, linetype="dashed", color = "grey32") +
  geom_line(data = b, size = rel(0.95), 
            aes(x = day, y = ratio, group = TERMS, 
                linetype = TERMS, color = TERMS)) + 
  SCALE_SMO_linetype + SCALE_SMO_color +  
  theme_USER + theme(legend.position = c(0.84, 0.87)) + 
  guides(alpha = "none") +
  labs(y = "Event Diffusion Momentum (EDM Index)", 
       x = "Range of Diffusion: 1-90 Days",
       title = "Fig.1(C) Number of SMOs involved") + 
  theme(text = element_text(size = 30))
plot_SMO

# Violence #
b = Eff_All[Eff_All$GROUP == "VIO",]
b

plot_VIO = ggplot() + 
  geom_pointrange(b, size = rel(1.2), 
                  mapping = aes(x = day, y = ratio, 
                                color = TYPE, 
                                ymin=lower, ymax=upper,
                                width = 0.18, alpha = 0.01)) + 
  SCALE_COLOR3 + SCALE_y2 + SCALE_x + new_scale_color() +
  geom_hline(yintercept=1, linetype="dashed", color = "grey32") +
  geom_line(data = b, size = rel(0.95), 
            aes(x = day, y = ratio, group = TERMS, 
                linetype = TERMS, color = TERMS)) + 
  SCALE_VIO_linetype + SCALE_VIO_color +  
  theme_USER + theme(legend.position = c(0.83, 0.79)) + 
  guides(alpha = "none") +
  labs(y = "", 
       x = "Range of Diffusion: 1-90 Days",
       title = "Fig.1(D) Violent vs. Non-Violent Protests") + 
  theme(text = element_text(size = 30))
plot_VIO



# 8 ratio plots #
EFF_PLOTS = cowplot::plot_grid(plotlist = list(plot_SIZE, plot_FORM,
                                               plot_SMO, plot_VIO), ncol = 2)
EFF_PLOTS

#
ggsave(plot = EFF_PLOTS, 
       "D:/Study1_effPlot.png",
       dpi = 400, units = c("cm"), 
       width = 28.5, height = 32.5)

#### [Analysis:] [Study02] Modelling: GPA * PPA ####

# 
indep= c("DECADE + PPA * GPA")
dep  = c('PreAct01', 'PostAct01', 
         'PreAct03', 'PostAct03',
         'PreAct07', 'PostAct07', 
         'PreAct14', 'PostAct14',
         'PreAct30', 'PostAct30', 
         'PreAct45', 'PostAct45',
         'PreAct60', 'PostAct60', 
         'PreAct90', 'PostAct90')

# ZIP #
library(pscl)
z_Pre01 = pscl::zeroinfl(as.formula(paste(dep[1 ], "~", indep)), data = event, dist = "poi")
z_Post01= pscl::zeroinfl(as.formula(paste(dep[2 ], "~", indep)), data = event, dist = "poi")
z_Pre03 = pscl::zeroinfl(as.formula(paste(dep[3 ], "~", indep)), data = event, dist = "poi")
z_Post03= pscl::zeroinfl(as.formula(paste(dep[4 ], "~", indep)), data = event, dist = "poi")
z_Pre07 = pscl::zeroinfl(as.formula(paste(dep[5 ], "~", indep)), data = event, dist = "poi")
z_Post07= pscl::zeroinfl(as.formula(paste(dep[6 ], "~", indep)), data = event, dist = "poi")
z_Pre14 = pscl::zeroinfl(as.formula(paste(dep[7 ], "~", indep)), data = event, dist = "poi")
z_Post14= pscl::zeroinfl(as.formula(paste(dep[8 ], "~", indep)), data = event, dist = "poi")
z_Pre30 = pscl::zeroinfl(as.formula(paste(dep[9 ], "~", indep)), data = event, dist = "poi")
z_Post30= pscl::zeroinfl(as.formula(paste(dep[10], "~", indep)), data = event, dist = "poi")
z_Pre45 = pscl::zeroinfl(as.formula(paste(dep[11], "~", indep)), data = event, dist = "poi")
z_Post45= pscl::zeroinfl(as.formula(paste(dep[12], "~", indep)), data = event, dist = "poi")
z_Pre60 = pscl::zeroinfl(as.formula(paste(dep[13], "~", indep)), data = event, dist = "poi")
z_Post60= pscl::zeroinfl(as.formula(paste(dep[14], "~", indep)), data = event, dist = "poi")
z_Pre90 = pscl::zeroinfl(as.formula(paste(dep[15], "~", indep)), data = event, dist = "poi")
z_Post90= pscl::zeroinfl(as.formula(paste(dep[16], "~", indep)), data = event, dist = "poi")

# Negbin #
m_Pre01 = glm.nb(as.formula(paste(dep[1 ], "~", indep)), data = event)
m_Post01= glm.nb(as.formula(paste(dep[2 ], "~", indep)), data = event)
m_Pre03 = glm.nb(as.formula(paste(dep[3 ], "~", indep)), data = event)
m_Post03= glm.nb(as.formula(paste(dep[4 ], "~", indep)), data = event) 
m_Pre07 = glm.nb(as.formula(paste(dep[5 ], "~", indep)), data = event)
m_Post07= glm.nb(as.formula(paste(dep[6 ], "~", indep)), data = event)
m_Pre14 = glm.nb(as.formula(paste(dep[7 ], "~", indep)), data = event)
m_Post14= glm.nb(as.formula(paste(dep[8 ], "~", indep)), data = event)
m_Pre30 = glm.nb(as.formula(paste(dep[9 ], "~", indep)), data = event)
m_Post30= glm.nb(as.formula(paste(dep[10], "~", indep)), data = event)
m_Pre45 = glm.nb(as.formula(paste(dep[11], "~", indep)), data = event)
m_Post45= glm.nb(as.formula(paste(dep[12], "~", indep)), data = event)
m_Pre60 = glm.nb(as.formula(paste(dep[13], "~", indep)), data = event)
m_Post60= glm.nb(as.formula(paste(dep[14], "~", indep)), data = event)
m_Pre90 = glm.nb(as.formula(paste(dep[15], "~", indep)), data = event)
m_Post90= glm.nb(as.formula(paste(dep[16], "~", indep)), data = event)

summary(m_Post90)

#
htmlreg(list(z_Pre01, z_Pre03, 
             z_Pre07, z_Pre14,
             z_Pre30, z_Pre45, 
             z_Pre60, z_Pre90), 
        "D:/Study2_ZIP_pre.html")

#
htmlreg(list(z_Post01, z_Post03, 
             z_Post07, z_Post14,
             z_Post30, z_Post45, 
             z_Post60, z_Post90), 
        "D:/Study2_ZIP_Post.html")

#
Get_NB_Terms(m_Pre01)
ZIP_fit(z_Pre01)
k1 = emmeans::emmeans(z_Pre60, pairwise ~ PPA|GPA, type = "response")
k1

Paired_IndexZIP_Interact = function(c, d, a, b, term_formula) {
  # c & d: glm models (pre, post)
  # a & b: zeroinfl models (pre, post)
  # get predicted values from Zero-infl models #
  n1 = emmeans::emmeans(a, specs = eval(parse(text = term_formula)))
  n2 = emmeans::emmeans(b, specs = eval(parse(text = term_formula)))
  k1 = as.data.frame(n1$emmeans)
  k2 = as.data.frame(n2$emmeans)
  
  k1$term = paste0(k1[,1], "_", k1[,2])
  k2$term = paste0(k2[,1], "_", k2[,2])
  b1 = subset(k1, select = c(term, emmean))
  b2 = subset(k2, select = c(term, emmean))
  names(b1) = c("TERM", "predicted")
  names(b2) = c("TERM", "predicted")
  
  termlist = b1$TERM
  PRE = b1$predicted
  POST= b2$predicted
  fit = log(POST/PRE)
  ratio = exp(fit)
  
  # get matrix from glm.nb models #
  mat1 = do.call(rbind, lapply(allEffects(c), function(i) {return(i$model.matrix)}))
  mat2 = do.call(rbind, lapply(allEffects(d), function(i) {return(i$model.matrix)}))
  a1 = vcov(a)
  a2 = vcov(b)
  cov1 = a1[1:(0.5*sqrt(length(a1))),1:(0.5*sqrt(length(a1)))]
  cov2 = a2[1:(0.5*sqrt(length(a2))),1:(0.5*sqrt(length(a2)))]
  
  # 4. Calculate s.e from model matrix and vcov 
  v.d = mat1%*%cov1%*%t(mat1)+mat2%*%cov2%*%t(mat2)
  se = sqrt(diag(v.d))
  upper = exp(fit + 1.96*se)
  lower = exp(fit - 1.96*se)
  model = deparse(substitute(a))
  ci.d = data.frame(cbind(model, termlist, PRE, POST, ratio, se, lower, upper))
  return(ci.d)
}

k1 = c("pairwise~PPA|GPA")

GPA_Pair_Eff_01 = Paired_IndexZIP_Interact(m_Pre01, m_Post01, z_Pre01, z_Post01, k1)
GPA_Pair_Eff_03 = Paired_IndexZIP_Interact(m_Pre03, m_Post03, z_Pre03, z_Post03, k1)
GPA_Pair_Eff_07 = Paired_IndexZIP_Interact(m_Pre07, m_Post07, z_Pre07, z_Post07, k1)
GPA_Pair_Eff_14 = Paired_IndexZIP_Interact(m_Pre14, m_Post14, z_Pre14, z_Post14, k1)
GPA_Pair_Eff_30 = Paired_IndexZIP_Interact(m_Pre30, m_Post30, z_Pre30, z_Post30, k1)
GPA_Pair_Eff_45 = Paired_IndexZIP_Interact(m_Pre45, m_Post45, z_Pre45, z_Post45, k1)
GPA_Pair_Eff_60 = Paired_IndexZIP_Interact(m_Pre60, m_Post60, z_Pre60, z_Post60, k1)
GPA_Pair_Eff_90 = Paired_IndexZIP_Interact(m_Pre90, m_Post90, z_Pre90, z_Post90, k1)

# combine the list into a data.frame #
temp  = lapply(ls(, pattern = "GPA_Pair_Eff_"), get) 
temp
temp2 = do.call(rbind, temp)
temp2

# name the terms properly #
Eff_All = temp2

# Round to 3 digits #
List_toRound = c("PRE", "POST", "ratio",
                 "se", "lower", "upper")
toFAC = function(i) {as.factor(as.character(i))}
toNUM = function(i) {as.numeric(as.character(i))}
Eff_All[List_toRound] = lapply(Eff_All[List_toRound], toNUM)
Round3 = function(i) {round(i, 3)}
Eff_All[List_toRound] = lapply(Eff_All[List_toRound], Round3)
head(Eff_All)

#
write_csv(Eff_All, "D:/Study2_Eff.csv")

#### [Outputs :] [Study02] Eff plots: GPA * PPA ####

## PLEASE enter your own file path or generate from previous steps ##
# Eff_All = read_csv("YOUR_FILE_PATH/Dropbox/Project/1.Pub/0DONE/202205_RSMCC_DOCA/Output/Study2_Eff.csv")
dist_tab(Eff_All$termlist)
Eff_All$TERMS = Eff_All$termlist

a5 = c("Dem_Potus_D", "Rep_Potus_D", 
       "Dem_Potus_R", "Rep_Potus_R")
df = Eff_All[Eff_All$TERMS %in% a5,]

head(df)
df$GOV   = substr(df$TERMS,11,11)
df$POTUS = substr(df$TERMS,1,  9)
df$DAY   = substr(df$model, 6, 7)


toFAC = function(i) {as.factor(as.character(i))}
toNUM = function(i) {as.numeric(as.character(i))}
df$DAY   = toNUM(df$DAY)
fivenum(df$DAY)

# Study2 - ggplot2 #

# set the theme for plots #
theme_USER =  theme_bw() + 
  theme(text = element_text(size = 8), 
        legend.background = element_rect(colour = "black", fill = "white"), 
        legend.margin = margin(4, 4, 4, 4), 
        plot.title = element_text(lineheight = 2, 
                                  face = "bold", 
                                  hjust = .5, 
                                  vjust = 1.5,
                                  size = 12) , 
        legend.title = element_text(size = 12) , 
        legend.text = element_text(size = 12), 
        legend.key.size = unit(1.2, "lines"), 
        legend.title.align = 0.5, 
        legend.position = c(0.86, 0.22), 
        legend.box.margin = margin(0, 0, 0, 0, "cm"), 
        axis.title=element_text(size=11,face="bold"),
        axis.text.y = element_text(size = 10), 
        axis.text.x = element_text(size = 9, angle = 58, hjust = 1 ))

# function for y-axis/x-axis alignment #
# digits to display for axis #
# label y#
scaleFUN1 = function(x) sprintf("%.1f", x)
scaleFUN2 = function(x) sprintf("%.2f", x)
scaleFUN3 = function(x) sprintf("%.2f", x)

#
df$AA = paste(scaleFUN3(df$ratio), " (",
              scaleFUN3(df$lower), " ~ ",
              scaleFUN3(df$upper), ")", 
              sep = "")
df$AA

df = as.data.frame(df)

# Distinguish the types of diffusion (or self-limiting) #
df$TYPE = "Non-Sig"
df$TYPE = ifelse(df$lower>1, "Diffusive", df$TYPE)
df$TYPE = ifelse(df$upper<1, "Self-Limiting", df$TYPE)
dist_tab(df$TYPE)

# label y#
SCALE_y1 = scale_y_continuous(labels = scaleFUN1)
SCALE_y2 = scale_y_continuous(labels = scaleFUN2, limits = c(0.7, 1.3))
SCALE_y3 = scale_y_continuous(labels = scaleFUN3)

# label x #
SCALE_x  = scale_x_continuous(limits = c(1, 90),
                              breaks = c(1, 3, 7, 14,
                                         30,45,60,90))

# color/linetype: manual #
SCALE_COLOR3 = scale_color_manual(values = c("green", "grey23", "red3"), 
                                  labels = c("Diffusive", "Non-Sig", "Self-Limiting"),
                                  name   = "Type of Diffusion")
SCALE_COLOR2 = scale_color_manual(values = c("grey23", "red3"), 
                                  labels = c("Non-Sig", "Self-Limiting"),
                                  name   = "Type of Diffusion")

SCALE_GOV_linetype = scale_linetype_manual(values = c("solid", "dotdash"),
                                           labels = c("Dem", "Rep"),
                                           name   = "Governor's Party")

SCALE_GOV_color = scale_color_manual(values = c("Steel Blue", "Red"),
                                     labels = c("Dem", "Rep"),
                                     name   = "Governor's Party")

# Violence #
b = df[df$POTUS == "Dem_Potus",]
dist_tab(b$TYPE)
b

plot_PotusD = ggplot() + 
  geom_pointrange(b, size = rel(1.2), 
                  mapping = aes(x = DAY, y = ratio, color = TYPE, 
                                ymin=lower, ymax=upper,
                                width = 0.18, alpha = 0.01)) + 
  SCALE_COLOR3 + 
  SCALE_y2 + SCALE_x + new_scale_color() +
  geom_hline(yintercept=1, linetype="dashed", color = "grey32") +
  geom_line(data = b, size = rel(0.95), 
            aes(x = DAY, y = ratio, group = GOV, 
                linetype = GOV, color = GOV)) + 
  SCALE_GOV_linetype + SCALE_GOV_color +  
  theme_USER + theme(legend.position = c(0.80, 0.79)) + 
  guides(alpha = "none") +
  # ylim(0.89, 1.26)+
  labs(y = "Event Diffusion Momentum (EDM Index)", 
       x = "Range of Diffusion: 1-90 Days",
       title = "Fig.2(A) EDM under Democratic Presidents") 
plot_PotusD

SCALE_COLOR2 = scale_color_manual(values = c("grey23", "red3"), 
                                  labels = c("Non-Sig", "Self-Limiting"),
                                  name   = "Type of Diffusion")
c = df[df$POTUS == "Rep_Potus",]
plot_PotusR = ggplot() + 
  geom_pointrange(c, size = rel(1.2), 
                  mapping = aes(x = DAY, y = ratio, color = TYPE, 
                                ymin=lower, ymax=upper,
                                width = 0.18, alpha = 0.01)) + 
  SCALE_COLOR2 + 
  SCALE_y2 + SCALE_x + new_scale_color() +
  geom_hline(yintercept=1, linetype="dashed", color = "grey32") +
  geom_line(data = c, size = rel(0.95), 
            aes(x = DAY, y = ratio, group = GOV, 
                linetype = GOV, color = GOV)) + 
  SCALE_GOV_linetype + SCALE_GOV_color +  
  theme_USER + theme(legend.position = "none") + 
  guides(alpha = "none") +
  labs(y = "",  x = "Range of Diffusion: 1-90 Days",
       title = "Fig.2(B) EDM under Republican Presidents") 
plot_PotusR

EFF_PLOT2 = cowplot::plot_grid(plotlist = list(plot_PotusD,
                                               plot_PotusR), ncol = 2)
EFF_PLOT2

#
ggsave("D:/Study2_effPlot.png", 
       plot = EFF_PLOT2, 
       dpi = 400, units = c("cm"), 
       width = 28.5, height = 16.25)

#### [Analysis:] [Study03] Modelling: GPA * POLICE ####

#write_rds(event, 
#          compress = "gz",
#          "D:/20220311_event_cleaned.rds")

## Please enter your own file path here. ## 
event = read_rds("YOUR_FILE_PATH/Dropbox/Data/DoCa/20220311_event_cleaned.rds")

# 
indep=c("GPA * POL_F + DECADE")
dep = c('PreAct01', 'PostAct01', 
        'PreAct03', 'PostAct03',
        'PreAct07', 'PostAct07', 
        'PreAct14', 'PostAct14',
        'PreAct30', 'PostAct30', 
        'PreAct45', 'PostAct45',
        'PreAct60', 'PostAct60', 
        'PreAct90', 'PostAct90')

#
Police_z_Pre01 = zeroinfl(as.formula(paste(dep[1 ], "~", indep)), data = event, dist = "poi")
Police_z_Post01= zeroinfl(as.formula(paste(dep[2 ], "~", indep)), data = event, dist = "poi")
Police_z_Pre03 = zeroinfl(as.formula(paste(dep[3 ], "~", indep)), data = event, dist = "poi")
Police_z_Post03= zeroinfl(as.formula(paste(dep[4 ], "~", indep)), data = event, dist = "poi")
Police_z_Pre07 = zeroinfl(as.formula(paste(dep[5 ], "~", indep)), data = event, dist = "poi")
Police_z_Post07= zeroinfl(as.formula(paste(dep[6 ], "~", indep)), data = event, dist = "poi")
Police_z_Pre14 = zeroinfl(as.formula(paste(dep[7 ], "~", indep)), data = event, dist = "poi")
Police_z_Post14= zeroinfl(as.formula(paste(dep[8 ], "~", indep)), data = event, dist = "poi")
Police_z_Pre30 = zeroinfl(as.formula(paste(dep[9 ], "~", indep)), data = event, dist = "poi")
Police_z_Post30= zeroinfl(as.formula(paste(dep[10], "~", indep)), data = event, dist = "poi")
Police_z_Pre45 = zeroinfl(as.formula(paste(dep[11], "~", indep)), data = event, dist = "poi")
Police_z_Post45= zeroinfl(as.formula(paste(dep[12], "~", indep)), data = event, dist = "poi")
Police_z_Pre60 = zeroinfl(as.formula(paste(dep[13], "~", indep)), data = event, dist = "poi")
Police_z_Post60= zeroinfl(as.formula(paste(dep[14], "~", indep)), data = event, dist = "poi")
Police_z_Pre90 = zeroinfl(as.formula(paste(dep[15], "~", indep)), data = event, dist = "poi")
Police_z_Post90= zeroinfl(as.formula(paste(dep[16], "~", indep)), data = event, dist = "poi")

#
Police_m_Pre01 = glm.nb(as.formula(paste(dep[1 ], "~", indep)), data = event)
Police_m_Post01= glm.nb(as.formula(paste(dep[2 ], "~", indep)), data = event)
Police_m_Pre03 = glm.nb(as.formula(paste(dep[3 ], "~", indep)), data = event)
Police_m_Post03= glm.nb(as.formula(paste(dep[4 ], "~", indep)), data = event) 
Police_m_Pre07 = glm.nb(as.formula(paste(dep[5 ], "~", indep)), data = event)
Police_m_Post07= glm.nb(as.formula(paste(dep[6 ], "~", indep)), data = event)
Police_m_Pre14 = glm.nb(as.formula(paste(dep[7 ], "~", indep)), data = event)
Police_m_Post14= glm.nb(as.formula(paste(dep[8 ], "~", indep)), data = event)
Police_m_Pre30 = glm.nb(as.formula(paste(dep[9 ], "~", indep)), data = event)
Police_m_Post30= glm.nb(as.formula(paste(dep[10], "~", indep)), data = event)
Police_m_Pre45 = glm.nb(as.formula(paste(dep[11], "~", indep)), data = event)
Police_m_Post45= glm.nb(as.formula(paste(dep[12], "~", indep)), data = event)
Police_m_Pre60 = glm.nb(as.formula(paste(dep[13], "~", indep)), data = event)
Police_m_Post60= glm.nb(as.formula(paste(dep[14], "~", indep)), data = event)
Police_m_Pre90 = glm.nb(as.formula(paste(dep[15], "~", indep)), data = event)
Police_m_Post90= glm.nb(as.formula(paste(dep[16], "~", indep)), data = event)

#
htmlreg(list(Police_z_Pre01, Police_z_Pre03, Police_z_Pre07, Police_z_Pre14,
             Police_z_Pre30, Police_z_Pre45, Police_z_Pre60, Police_z_Pre90), 
        "D:/20220311_Study3_ZIP_pre.html")

#
htmlreg(list(Police_z_Post01, Police_z_Post03, Police_z_Post07, Police_z_Post14,
             Police_z_Post30, Police_z_Post45, Police_z_Post60, Police_z_Post90), 
        "D:/20220311_Study3_ZIP_Post.html")

#
k2 = c("pairwise ~ GPA | POL_F")
Police_Pair_Eff_01 = Paired_IndexZIP_Interact(Police_m_Pre01, Police_m_Post01, Police_z_Pre01, Police_z_Post01, k2)
Police_Pair_Eff_03 = Paired_IndexZIP_Interact(Police_m_Pre03, Police_m_Post03, Police_z_Pre03, Police_z_Post03, k2)
Police_Pair_Eff_07 = Paired_IndexZIP_Interact(Police_m_Pre07, Police_m_Post07, Police_z_Pre07, Police_z_Post07, k2)
Police_Pair_Eff_14 = Paired_IndexZIP_Interact(Police_m_Pre14, Police_m_Post14, Police_z_Pre14, Police_z_Post14, k2)
Police_Pair_Eff_30 = Paired_IndexZIP_Interact(Police_m_Pre30, Police_m_Post30, Police_z_Pre30, Police_z_Post30, k2)
Police_Pair_Eff_45 = Paired_IndexZIP_Interact(Police_m_Pre45, Police_m_Post45, Police_z_Pre45, Police_z_Post45, k2)
Police_Pair_Eff_60 = Paired_IndexZIP_Interact(Police_m_Pre60, Police_m_Post60, Police_z_Pre60, Police_z_Post60, k2)
Police_Pair_Eff_90 = Paired_IndexZIP_Interact(Police_m_Pre90, Police_m_Post90, Police_z_Pre90, Police_z_Post90, k2)

#

# combine the list into a data.frame #
temp  = lapply(ls(, pattern = "Police_Pair_Eff_"), get) 
temp
temp2 = do.call(rbind, temp)
temp2

# name the terms properly #

Eff_All = temp2
head(Eff_All)

# Round to 3 digits #
List_toRound = c("PRE", "POST", "ratio",
                 "se", "lower", "upper")
toFAC = function(i) {as.factor(as.character(i))}
toNUM = function(i) {as.numeric(as.character(i))}
sapply(Eff_All, class)

Eff_All[List_toRound] = lapply(Eff_All[List_toRound], toNUM)
Round3 = function(i) {round(i, 3)}
Eff_All[List_toRound] = lapply(Eff_All[List_toRound], Round3)
head(Eff_All)

write_csv(Eff_All, "D:/Study3_PoliceGov.csv")

#### [Outputs :] [Study03] Eff plots: GPA * POLICE ####

# Please enter your own file path here.
# Eff_All = read_csv("YOUR_FILE_PATH/Dropbox/Project/1.Pub/0DONE/202205_RSMCC_DOCA/Output/Study3_PoliceGov.csv")
df = Eff_All
head(df)
sapply(df, class)
#
df$GOV = substr(df$termlist, 1,  1)
table(df$GOV)

df$POL = ifelse(grepl(as.character(df$termlist), pattern = "No "), 0, 1)

table(df$model)
df$DAY = substr(df$model, 13, 15)
df$model

df$DAY = toNUM(df$DAY)

# Study3 - ggplot2 #

# set the theme for plots #
theme_USER =  theme_bw() + theme(text = element_text(size = 8), 
                                 legend.background = element_rect(colour = "black", fill = "white"), 
                                 legend.margin = margin(4, 4, 4, 4), 
                                 plot.title = element_text(lineheight = 2, 
                                                           face = "bold", 
                                                           hjust = .5, 
                                                           vjust = 1.5,
                                                           size = 12) , 
                                 legend.title = element_text(size = 12) , 
                                 legend.text = element_text(size = 12), 
                                 legend.key.size = unit(1.2, "lines"), 
                                 legend.title.align = 0.5, 
                                 legend.position = c(0.86, 0.22), 
                                 legend.box.margin = margin(0, 0, 0, 0, "cm"), 
                                 axis.title=element_text(size=11,face="bold"),
                                 axis.text.y = element_text(size = 10), 
                                 axis.text.x = element_text(size = 9, angle = 58, hjust = 1 ))

# function for y-axis/x-axis alignment #
# digits to display for axis #
# label y #
scaleFUN1 = function(x) sprintf("%.1f", x)
scaleFUN2 = function(x) sprintf("%.2f", x)
scaleFUN3 = function(x) sprintf("%.2f", x)

#
df$AA = paste(scaleFUN3(df$ratio), " (",
              scaleFUN3(df$lower), " ~ ",
              scaleFUN3(df$upper), ")", 
              sep = "")
df$AA

# Distinguish the types of diffusion (or self-limiting) #
df$TYPE = "Non-Sig"
df$TYPE = ifelse(df$lower>1, "Diffusive", df$TYPE)
df$TYPE = ifelse(df$upper<1, "Self-Limiting", df$TYPE)
dist_tab(df$TYPE)

# label y#
SCALE_y1 = scale_y_continuous(labels = scaleFUN1)
SCALE_y2 = scale_y_continuous(labels = scaleFUN2, limits = c(0.68, 1.32))
SCALE_y3 = scale_y_continuous(labels = scaleFUN3)

# label x #
SCALE_x  = scale_x_continuous(limits = c(1, 90),
                              breaks = c(1, 3, 7, 14,
                                         30,45,60,90))

# color/linetype: manual #

SCALE_COLOR3 = scale_color_manual(values = c("green", "grey23", "red3"), 
                                  labels = c("Diffusive", "Non-Sig", "Self-Limiting"),
                                  name = "Type of Diffusion")

SCALE_COLOR2 = scale_color_manual(values = c("grey23", "red3"), 
                                  labels = c("Non-Sig", "Self-Limiting"),
                                  name = "Type of Diffusion")

SCALE_POL_linetype = scale_linetype_manual(values = c("solid", "dotdash"),
                                           labels = c("No Police", "Police"),
                                           name = "Policing")

SCALE_POL_color = scale_color_manual(values = c("Steel Blue", "Red"),
                                     labels = c("No Police", "Police"),
                                     name = "Policing")

# Violence #
b = df[df$GOV == "D",]
names(b)
sapply(b, class)


# Round to 3 digits #
List_toRound = c("PRE", "POST", "ratio",
                 "se", "lower", "upper")
b[List_toRound] = lapply(b[List_toRound], toNUM)

plot_POL_D = ggplot() + 
  geom_pointrange(b, size = rel(1.2), 
                  mapping = aes(x = DAY, y = ratio, color = TYPE, 
                                ymin=lower, ymax=upper,
                                width = 0.18, alpha = 0.01)) + 
  SCALE_COLOR2 + 
  SCALE_y2 + SCALE_x + new_scale_color() +
  geom_hline(yintercept=1, linetype="dashed", color = "grey32") +
  geom_line(data = b, size = rel(0.95), 
            aes(x = DAY, y = ratio, group = factor(POL), 
                linetype = factor(POL), color = factor(POL))) + 
  SCALE_POL_linetype + SCALE_POL_color +  
  theme_USER + theme(legend.position = "None") + 
  guides(alpha = "none") +
  labs(y = "Event Diffusion Momentum (EDM Index)", 
       x = "Range of Diffusion: 1-90 Days",
       title = "Fig.3(A) EDM under Democratic Governors") + 
  theme(text = element_text(size = 30))

plot_POL_D

df$GOV

#
c = df[df$GOV == "R",]
sapply(c, class)
c[List_toRound] = lapply(c[List_toRound], toNUM)

plot_POL_R = ggplot() + 
  geom_pointrange(c, size = rel(1.2), 
                  mapping = aes(x = DAY, y = ratio, color = TYPE, 
                                ymin=lower, ymax=upper,
                                width = 0.18, alpha = 0.01)) + 
  SCALE_COLOR3 + 
  SCALE_y2 + SCALE_x + new_scale_color() +
  geom_hline(yintercept=1, linetype="dashed", color = "grey32") +
  geom_line(data = c, size = rel(0.95), 
            aes(x = DAY, y = ratio, group = factor(POL), 
                linetype = factor(POL), color = factor(POL))) + 
  SCALE_POL_linetype + SCALE_POL_color +  
  theme_USER + theme(legend.position = c(0.80, 0.79)) + 
  guides(alpha = "none") +
  labs(y = "", 
       x = "Range of Diffusion: 1-90 Days",
       title = "Fig.3(B) EDM under Republican Governors") + 
  theme(text = element_text(size = 30))

plot_POL_R

EFF_PLOT3 = cowplot::plot_grid(plotlist = list(plot_POL_D,
                                               plot_POL_R), ncol = 2)
EFF_PLOT3

#
ggsave("D:/Study3_effPlot.png", plot = EFF_PLOT3, 
       dpi = 400, units = c("cm"), 
       width = 28.5, height = 16.25)

#### ---- Reproducible Codes End Here ---- ####