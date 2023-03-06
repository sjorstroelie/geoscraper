#Scraping Geoguesser challenge results using Rselenium

#Note for first time use: If not already installed, the script will try to install 4 packages from CRAN (starting on line 35)

#Step 1; pass geoguessr account info to "mail" and "password" variables.

#WARNING: it NOT recommended to write down plain text login info in saved documents. 
#Pass the following to the console instead 
# mail <-'your@email.com'
# password <- 'somethingreallycomplex'

#Working directory is the same place where the script is saved
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#Step 2: fill out the name of a .csv with challenge data. It should have two columns:
#column 1 should be named "challenge" and can contain any character string
#columm 2 should be named "url" and should contain challenge url's

challenge_csv <- "rondelijst.csv"

#Step 4: fill out the name of the export file; this is up to you as long as its a csv.
export_filename <- "challenge_data.csv"



############## If you just want to run the script and cannot read R, you should not edit anything below this line ###############

#Checking if required packages are already installed, then installing packages if needed 
required_packages <- c("RSelenium", "tidyverse","rvest","stringr")

to_install <- required_packages[!required_packages %in% installed.packages()]

install.packages(to_install)

#Loading packages
library(RSelenium)
library(tidyverse)
library(rvest)
library(stringr)

#Read challenge csv
geoguessr_challenges <- read.csv2(challenge_csv)

#Set URL for geoguessr login page
geoguessr_signin <- "https://www.geoguessr.com/signin"

#Starting Rselenium driver

driver <- rsDriver(browser="firefox")

remote_driver <- driver[["client"]]
remote_driver$open()

#Set implicit time-out (Preventing Rselenium from  trying to read data before the page is loaded)
remote_driver$setTimeout(type = "implicit", milliseconds = 5000)


#Navigate to login
remote_driver$navigate(geoguessr_signin)

#click on cookie msg
cookie_popup <- remote_driver$findElement(using = "id",value = "accept-choices")
cookie_popup$clickElement()

#Login procedure; wrapped in tryCatch in case user is already logged in
tryCatch(
  {
  #Find username/email field
  username_field <- remote_driver$findElement(using = "name", value = "email")
  
  
  #Sent email to username field
  username_field$sendKeysToElement(list(mail))
  
  #Find password field
  password_field <- remote_driver$findElement(using = "name", value = "password")
  
  #Sent password to password field
  password_field$sendKeysToElement(list(password))
  
  #Find login button
  knop <- remote_driver$findElement(using = "class" ,value = "button_wrapper__NkcHZ")
  #Click login button
  knop$clickElement()
  
},
#Sent error msg to console if login fields/buttons cannot be found
error = function(cond){
  message("Already logged in?")
}
)


#Helperfunctions for reading scores / distances and time

#Read scores
read_score  <- function(string){
  str_remove_all(string,"pts|,")%>%
    as.numeric() %>%
    replace_na(0)
} 

#Read distance and convert to meters
read_distance <- function(string){
  
  #Set distance to NA if "Timed out"
  if(str_detect(string,"Timed out")){
    
    distance <- NA
    
    #If distance is measured in "km"; multiply by 1000
  }else if(str_detect(string, "km")){
    
    
    distance <- str_extract(string, pattern = ".*(?= km)")%>%
      str_replace(",","")%>%
      as.numeric()
    
    distance <- distance *1000
    
    #Distance is measured in meters already  
  }else{
    distance <- str_extract(string, pattern = ".*(?= m )")%>%
      as.numeric()
    
  }
  
  #Return distance
  distance
}

#Read time and convert to seconds
read_time <- function(string){
  
  #Get time from the string that includes both distance and time
  time <- str_extract(string, pattern = "(?<= - ).*")
  
  #Get minutes
  minutes <- str_extract(time, pattern = ".*(?= min)")%>%
    as.numeric()%>%
    #If there are no minutes in the string; set minutes to 0
    replace_na(0)
  
  #convert minutes to seconds
  seconds_1 <- minutes*60
  
  #Get seconds from the string
  seconds_2 <- str_extract(time, pattern = "..(?= sec)")%>%
    as.numeric()%>%
    #If there are no seconds in the string; set seconds to 0
    replace_na(0)
  
  #Add up seconds from "min" and "sec"  elements
  total_seconds <- seconds_1 + seconds_2
  
  #Return sum of seconds
  total_seconds
  
  
}


#Loop over all rounds in geoguessr challenge df
all_challenges <- lapply(1:nrow(geoguessr_challenges),function(i){
  
  
  challenge <- geoguessr_challenges$challenge[i]
  url <- geoguessr_challenges$url[i]
  #Print current round & url to console
  print(paste("reading challenge", challenge,
              "URL:", url))
  
  #Navigate driver to challenge URL
  remote_driver$navigate(url)
  
  #Read challenge data
  
  #Find highscore table
  table <- remote_driver$findElement(using = "class",value = "results_table__FHKQm")    
  #Read table as character string
  table_char_str <- table$getElementText()
  #Make a vector by splitting on "\n"
  table_vector <- str_split(table_char_str, pattern = "\n") %>%
    unlist()
  
  

  #table_vector
  #-trim the first 6 indices (round names) & trim last index "CHALLENGE A FRIEND" 
  table_vector <- table_vector[7:(length(table_vector)-1)]

  # table_vector[1:14]  
  #Calculate number of players; each row of challenge data (1 player) is 14 elements long
  
  n_players <- length(table_vector)/14
  
  #Convert vector to matrix with n_players columns 
  table_df <- matrix(table_vector,ncol = n_players) %>% 
    #transpose so each row contains 1 player and each column contains a variable
    t()%>%
    #to dataframe
    as.data.frame()

    
  #name variables
  names(table_df) <- c("placement","player","score_1","time_1","score_2","time_2","score_3","time_3","score_4","time_4","score_5","time_5","score_total","time_total")
  
    #Cleanup; 
    #- distance in meters
    #- time in seconds
    #- everything to numeric
    #- long df
    table_df %>%
      dplyr::select(-c(time_total,score_total))%>%
      mutate(
      #placement to numeric
      placement = as.numeric(str_remove(placement,"\\.")),
      #score columns to numeric
      across(contains("score"), read_score),
      #Make distance columns
      across(.cols = contains("time"),
             .fns = Vectorize(read_distance),
             .names = "distance_{str_extract(.col,'[:digit:]')}"),
      
      #time_colums
      across(contains("time"),read_time))%>%
      #pivot to longest format, then back to wide with columms for time,score,distance
      pivot_longer(score_1:distance_5, names_to = c("type","round"), names_sep = "_") %>%
      pivot_wider(names_from = "type") %>%
      #Add the challenge name
      mutate(challenge = challenge)

})

all_challenges <- do.call(rbind,all_challenges)

#Write csv to working directory
write.csv(all_challenges,export_filename, row.names = F)
