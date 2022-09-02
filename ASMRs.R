
## function to calculate ASMRs

asmr <- function(df, ESP_data=ESP, poisson_data=poisson, total_esp, death, age_grp, group_vars){
  
  death <- enquo(death)
  age_grp <- enquo(age_grp)

  
   grp_vars = c(quo_name(age_grp), group_vars)
     print(grp_vars)
     rates_by_age <- df %>%
        group_by(across(all_of(!!grp_vars))) %>%
        summarise(deaths = sum(!!death, na.rm = TRUE), 
                  pops = n(),
                  person_years = sum(time_at_risk_years)) %>%
        mutate(crude_rate_100000_person_years = 100000*(deaths / person_years)) %>%
               rename(agegroup = !!age_grp) %>%
        collect() %>%
        left_join(ESP_data, by =  "agegroup")%>%
        mutate(variance = (crude_rate_100000_person_years^2 / deaths) * ESP^2,
              stand_rate = crude_rate_100000_person_years * ESP)
  print(rates_by_age)
  if (sum(is.na(rates_by_age$ESP)) != 0){
    stop("Missing population weights. Check the labels of the age groups")
  }


  asmrs <- rates_by_age %>%
        group_by(across(all_of(!!group_vars))) %>%
        summarise(across(c("deaths","pops", "stand_rate", "variance", "person_years"), sum, na.rm=TRUE)) %>%
        mutate(variance = variance/total_esp^2,
               SE = sqrt(variance),
              rate = stand_rate/total_esp) %>%
        left_join( poisson_data, by = c("deaths")) %>%
        mutate( lower = ifelse(deaths < 100, ((L * deaths - deaths)*((variance/deaths)^0.5)+rate),
                              rate -(1.96 * SE)),
                upper = ifelse(deaths < 100, ((U * deaths - deaths)* ((variance/deaths)^0.5)+rate),
                                                                                   rate + (1.96 * SE)))%>%
    mutate(asmr_per_100000_person_years = rate, asmr_lower=lower, asmr_upper=upper, rate=100000*(deaths / person_years))%>%
    select_at(vars(c(all_of(!!group_vars), "deaths", "pops", "person_years", "rate", "asmr_per_100000_person_years", "asmr_lower", "asmr_upper")))%>%
  ungroup()

  asmrs
}


## ASMRs over whole period

set_period_dfs <- function(df_tt, cenmort_df, min_age, start, end, asmr, date_type, case_type, category, strat, max_age=NULL, save=TRUE){
  listofdfs <- list()
  is_missing <- missing(max_age)
  
       df_all <- df_tt %>%
          filter(.data[[date_type]] >= start & .data[[date_type]] <= end) %>%
	  group_by(census_person_id) %>%
          arrange(desc(.data[[case_type]]),.data[[date_type]]) %>%
          mutate(test_number = row_number()) %>% filter(test_number==1) %>%
          mutate(case_in_period = .data[[case_type]]) %>% ungroup()
    
     
       df <- cenmort_df %>% filter(dod >= start | (is.na(dod))) %>%
                  left_join(df_all, by = "census_person_id") %>%
                  mutate(case_in_period = ifelse(is.na(organism_species_name), 0, case_in_period)) %>%
                  mutate(age_new = (datediff(start,dob))/365.25) %>%
                  mutate(age_range_new = case_when(age_new >= 10 & age_new < 15 ~ "10-14",
                                         age_new >= 15 & age_new < 20 ~ "15-19",
                                         age_new >= 20 & age_new < 25 ~ "20-24",
                                         age_new >= 25 & age_new < 30 ~ "25-29",
                                         age_new >= 30 & age_new < 35 ~ "30-34",
                                         age_new >= 35 & age_new < 40 ~ "35-39",
                                         age_new >= 40 & age_new < 45 ~ "40-44",
                                         age_new >= 45 & age_new < 50 ~ "45-49",
                                         age_new >= 50 & age_new < 55 ~ "50-54",
                                         age_new >= 55 & age_new < 60 ~ "55-59",
                                         age_new >= 60 & age_new < 65 ~ "60-64",
                                         age_new >= 65 & age_new < 70 ~ "65-69",
                                         age_new >= 70 & age_new < 75 ~ "70-74",
                                         age_new >= 75 & age_new < 80 ~ "75-79",
                                         age_new >= 80 & age_new < 85 ~ "80-84",
                                         age_new >= 85 & age_new < 90 ~ "85-89",
                                         age_new >= 90 ~ "90-100")) %>%
                  filter(age_new >= min_age) %>%
                  mutate(t = ifelse(is.na(.data[[date_type]]), datediff(end, start)+1, datediff(.data[[date_type]], start)+1)) %>%
                  mutate(t = case_when(is.na(dod) ~ t,
                                       dod >= start & dod <= end ~ datediff(dod, start)+1,
                                       TRUE ~ t),
                         time_at_risk_years = t/7,
                         overall = "Total")


       asmr_all <- df %>%
                   asmr(death = case_in_period,
                        total_esp = total_esp,
                        age_grp = age_range_new,
                        group_vars = c(category, strat)) %>%
                   mutate(Outcome = case_type) %>%
                   rename(
                   'total' = deaths,
                   'person_weeks' = person_years,
                   'asmr_per_100000_person_weeks' = asmr_per_100000_person_years)

       set_period_dfs <- as.data.frame(asmr_all)

 return(set_period_dfs)
}








                  
          



    asmr_all <- df_all %>%
        asmr(death = death_name,
        total_esp = total_esp,
        age_grp = age_range_2020,
        group_vars=c(category, strat)) %>% mutate(Outcome = death_type[i])


    if (save==TRUE)
      write.csv(asmr_all, paste(out_dir, "/",death_type[i], "_", category,"_asmr_all_",start,"_to_",end,".csv", sep=""), row.names=FALSE)

    listofdfs[[i]] <- asmr_all

  }
  set_period_dfs <- dplyr::bind_rows(listofdfs)
  write.csv(set_period_dfs, paste(out_dir, "/","deaths", "_", category,"_asmr_all_",start,"_to_",end,".csv", sep=""), row.names=FALSE)
      
    return(set_period_dfs)
}
    


    
    


