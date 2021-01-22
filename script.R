library(tidyverse)

StormData <- read.csv2("ProjectData/StormData.bz2", sep = ",")

FatalityData <- StormData %>%
        select(EVTYPE, FATALITIES) %>%
        mutate(FATALITIES = as.numeric(FATALITIES))   

TotalFatalities <- sum(FatalityEvent$FATALITIES)

FatalityEvent <- FatalityData %>%
        group_by(EVTYPE) %>%
        summarise(total = sum(FATALITIES)) %>%
        slice_max(total, n = 10)  

FatalityEvent <-  FatalityEvent %>%
        mutate(EVTYPE = fct_reorder(EVTYPE, total))
        
ggplot(FatalityEvent, aes(x = EVTYPE, y = total)) + 
        geom_bar(stat = "identity") + 
        coord_flip() +
        labs(title = "Population Health: top 10 most harmful event types -",
             subtitle = "Number of Fatalities, 1950 - Nov 2011",
             x ="", 
             y = "",
             caption = "Source: NOOA Storm Data") +
        theme_light()


InjuryData <- StormData %>%
        select(EVTYPE, INJURIES) %>%
        mutate(INJURIES = as.numeric(INJURIES))   

InjuryEvent <- InjuryData %>%
        group_by(EVTYPE) %>%
        summarise(total = sum(INJURIES)) %>%
        slice_max(total, n = 10)  

InjuryEvent <-  InjuryEvent %>%
        mutate(EVTYPE = fct_reorder(EVTYPE, total))

ggplot(InjuryEvent, aes(x = EVTYPE, y = total)) + 
        geom_bar(stat = "identity") + 
        coord_flip() +
        labs(title = "Population Health: top 10 most harmful event types -",
             subtitle = "Number of Injuries, 1950 - Nov 2011",
             x ="", 
             y = "",
             caption = "Source: NOOA Storm Data") + 
        theme_light()

PropertyData<- StormData %>%
        select(EVTYPE, PROPDMG : CROPDMGEXP) %>%
        mutate(PROPDMG = as.numeric(PROPDMG),
               CROPDMG = as.numeric(CROPDMG))  

PropertyData <- PropertyData %>%
        mutate(PROPDMG = PROPDMG * case_when(PROPDMGEXP == "K" ~ 1000,
                                             PROPDMGEXP == "M" ~ 1000000,
                                             PROPDMGEXP == "B" ~ 1000000000,
                                             TRUE ~ 1)) %>%
        mutate(CROPDMG = CROPDMG * case_when(CROPDMGEXP == "K" ~ 1000,
                                             CROPDMGEXP == "M" ~ 1000000,
                                             CROPDMGEXP == "B" ~ 1000000000,
                                             TRUE ~ 1)) %>%
        mutate(USD = PROPDMG + CROPDMG)

PropertyEvent <- PropertyData %>%
        group_by(EVTYPE) %>%
        summarise(total = sum(USD)) %>%
        slice_max(total, n = 10) 

PropertyEvent <- PropertyEvent %>%
        mutate(EVTYPE = fct_reorder(EVTYPE, total))

ggplot(PropertyEvent, aes(x = EVTYPE, y = total / 1000000000)) + 
        geom_bar(stat = "identity") + 
        coord_flip() +
        labs(title = "Economic damage: top 10 most expensive event types -",
             subtitle = "in bn USD, 1950 - Nov 2011",
             x ="", 
             y = "",
             caption = "Source: NOOA Storm Data") + 
        theme_light()