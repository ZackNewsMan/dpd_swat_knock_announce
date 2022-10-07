# ACS data per neighborhod comes from here and was downloaded on 10/05: https://www.denvergov.org/opendata/dataset/american-community-survey-nbrhd-2015-2019 

library(tidyverse)

library(readr)
dpd_swat_knock_20_22 <- read_csv("dpd_swat_knock_20_22.csv")
View(dpd_swat_knock_20_22)

  # Brought back in as dpd_swat_knock_20_22_2, see line 138 for more info



                ############## reimport totals ############################
                
                # 184 total knock and announce warrants in Denver in this data 
                # Excel has 187, where did I lose the 3??
                
                dpd_swat_knock_20_22 %>% 
                  summarize(swat_total = sum(swat_count)) 
                
                # This has 187 in R too, where did I lose the 3?
                
                den_swat_nbhd_race_poverty %>% 
                  filter(!is.na(swat_count)) %>%
                  summarize(swat_total = sum(swat_count))
                
                # Goes down to 184 when I filter out for NA, not sure what's happening. Maybe anti-join will show
                
                anti_join(den_swat_nbhd_race_poverty, dpd_swat_knock_20_22, by = "nbhd_name") %>% 
                  View()
                
                # Didn't work, let's switch it:
                
                anti_join(dpd_swat_knock_20_22, den_swat_nbhd_race_poverty, by = "nbhd_name") %>% 
                  View()
                
                # Here are the three that didn't make it into the join:
                
                #  nbhd_name          swat_count
                # Green Valley Ranch          1
                # Norh Capitol Hill           1
                # Outside of Denver           1

                # Need to add one to North Capitol Hill (DPD misspelled it) and Gateway - Green Valley Ranch to account for DPD messing up. Will then run numbers again


 library(readr)
  dpd_swat_knock_20_22_2 <- read_csv("dpd_swat_knock_20_22_2.csv")
  View(dpd_swat_knock_20_22_2)


dpd_swat_knock_20_22 <- dpd_swat_knock_20_22_2


library(readr)
den_acs_nbrhd_2015_2019 <- read_csv("den_acs_nbrhd_2015_2019.csv")
View(den_acs_nbrhd_2015_2019)

# Want to do a left join to bring SWAT data into same table as neighborhood ACS data
  # Outline:   tree_neighborhood_2020_2021 <- left_join(trimmed_total_basa_op_2021, trimmed_total_basa_op_2020, by = "neighborhood") 

  left_join(den_acs_nbrhd_2015_2019, dpd_swat_knock_20_22, by = "nbhd_name") %>% 
    View()
  
  # Worked! 
  
 den_swat_nbhd_race_poverty <- left_join(den_acs_nbrhd_2015_2019, dpd_swat_knock_20_22, by = "nbhd_name")
 
    #  SQL version: 
      # CREATE TABLE den_swat_nbhd_race_poverty AS
      # SELECT *
      # FROM den_acs_nbrhd_2015_2019 LEFT JOIN dpd_swat_knock_20_22 ON den_acs_nbrhd_2015_2019.nbhd_name = dpd_swat_knock_20_22.nbhd_name
 

 # Let's see which neighborhoods didn't have a SWAT knock and announce warrant and if they merged correctly
 
  den_swat_nbhd_race_poverty %>% 
    select(nbhd_name, swat_count, pct_white, pct_poverty) %>% 
    View()
  
    # Poverty doesn't seem to have a big affect on number of SWAT knock and announce warrants. But race does. 

    # Worked well, let's add some columns and make a new table
        
       den_swat_nbhd_race_poverty %>% 
          select(nbhd_name, total_population_all, swat_count, pct_white, pct_hispanic, pct_black, pct_nativeam, pct_hawaiianpi, pct_otherrace,pct_twoormore_races) %>% 
          View()
       
       den_swat_race <- den_swat_nbhd_race_poverty %>% 
         select(nbhd_name, total_population_all, swat_count, pct_white, pct_hispanic, pct_black, pct_nativeam, pct_hawaiianpi, pct_otherrace,pct_twoormore_races)
  
       # SQL:
         # CREATE TABLE den_swat_race AS
         # SELECT nbhd_name, total_population_all, swat_count, pct_white, pct_hispanic, pct_black, pct_nativeam, pct_hawaiianpi, pct_otherrace,pct_twoormore_races
         # FROM den_swat_nbhd_race_poverty
         
  # How many SWAT raids happened in neighborhoods with 51% or higher white (aka a majority White nbhd)?  
  
       den_swat_race %>% 
        filter(pct_white >= 51) %>% 
        View()
       
        # 50.1?
       
       den_swat_race %>% 
         filter(pct_white >= 50.1) %>% 
         View()
       
        # No difference
      
      # 51 neighborhoods majority white
       
       den_swat_race %>% 
         filter(pct_white >= 51) %>% 
         group_by(nbhd_name) %>% 
         summarize(swat_count = n()) 
       
        # oh count not actually summing how I need it, it's counting
       
             den_swat_race %>% 
               filter(pct_white >= 51) %>% 
               summarize(swat_total = sum(swat_count))
             
             # NA's giving trouble, won't sum with it there. So after filter for being 51% or more white, then take out rows with a NA
             
             den_swat_race %>% 
               filter(pct_white >= 51) %>% 
               filter(!is.na(swat_count)) %>%
               summarize(swat_total = sum(swat_count)) 
             
             # 66 knock and announce warrants in neighborhoods that are majority white
               
                # SQL verified: 66 knock and announce warrants  
                     # SELECT SUM(swat_count)
                     # FROM den_swat_race
                     # WHERE pct_white >= 51
                     # AND swat_count NOT NULL
            
                # Check for 50.1 
             
             den_swat_race %>% 
               filter(pct_white >= 50.1) %>% 
               filter(!is.na(swat_count)) %>%
               summarize(swat_total = sum(swat_count)) 
             
                # No difference, still 66 
             
            # How many white neighborhoods have never had a knock and announce SWAT warrant? Looking for NA's
             
              den_swat_race %>% 
               filter(pct_white >= 51) %>% 
               filter(is.na(swat_count))
             
              # 22 majority white neighborhoods have never had a knock and announce SWAT warrant
  
                # Verified by SQL:
                 # SELECT *
                 # FROM den_swat_race
                 # WHERE pct_white >= 51
                 # AND swat_count IS NULL
              
              # How many majority white neighborhoods exist
              
              den_swat_race %>% 
                filter(pct_white >= 50.1)
              
                # 51 majority white neighborhoods
              
              51-22
              
              ((29/51)*100)
              
              # SWAT warrants have occurred in 57% of Denver's 51 majority White neighborhoods.
              
              
  # How many SWAT raids happened in neighborhoods with 49% or less white (aka a majority minority nbhd)?  
    # <= is symbol for "less than or equal to": https://www.datamentor.io/r-programming/operator/     
                  
        # How many minority neighborhoods are there?   
              
              den_swat_race %>% 
                filter(pct_white <= 49.9)
             
              # Only 27 neighborhoods in Denver are majority minority 
              
                # SQL verified: 
                  # SELECT *
                  # FROM den_swat_race
                  # WHERE pct_white <= 49.9
        
        # And how many SWAT warrants happened there?
              
              den_swat_race %>% 
                filter(pct_white <= 49.9) %>% 
                filter(!is.na(swat_count)) %>%
                summarize(swat_total = sum(swat_count))       
              
              # 120 knock and announce SWAT warrants happened in minority neighborhoods
              
              # Verified by SQL:
                # SELECT SUM(swat_count)
                # FROM den_swat_race
                # WHERE pct_white <= 49.9
                # AND swat_count NOT NULL

              # Windsor is 49.00864124 White, which would mean that it is minority majority but technically not 49 or below. Need to round up 
              
              den_swat_race %>% 
                filter(pct_white <= 49.9) %>% 
                filter(!is.na(swat_count)) %>%
                summarize(swat_total = sum(swat_count)
              
              
      # How many minority neighborhoods have never had a knock and announce SWAT warrant? Looking for NA's
              
              den_swat_race %>% 
                filter(pct_white <= 49.9) %>% 
                filter(is.na(swat_count))
              
              # 2 majority minority neighborhoods have never had a knock and announce SWAT warrant. DIA and Sun Valley
              
              # Verified by SQL:
                # SELECT *
                # FROM den_swat_race
                # WHERE pct_white <= 49.9
                # AND swat_count IS NULL
              
              # How many majority white neighborhoods exist
              
              den_swat_race %>% 
                filter(pct_white <= 49.9)
              
              # 27 majority minority neighborhoods      
              
              ((24/27)*100)
              
                # SWAT warrants have occurred in 89% of Denver's 27 majority minority neighborhoods.
              
              ################### perc of totals ###############
                            
# Total SWAT warrants from 2020 to 2022:
      
      den_swat_race %>% 
      filter(!is.na(swat_count)) %>%
      summarize(swat_total = sum(swat_count)) 
   
      
        # 187 total knock and announce warrants in Denver in this data with a neighborhood made available
          # One happened outside of Denver, but will count it to be able to talk easier about all SWAT knock and announce activity 
          
  # 120 warrants served in minority neighborhoods
      
      ((120/187)*100)
      
      # 64% of knock and announce SWAT warrants happened in minority majority neighborhoods. 
  
  #  66 knock and announce warrants in neighborhoods that are majority white            
              
    ((66/187)*100)
              
     # 35% of knock and announce SWAT warrants happened in minority majority neighborhoods.         
              
  