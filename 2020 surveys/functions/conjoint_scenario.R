conjoint_scenario <- function(data, condition, defect_cause, by = "NULL"){
  
  results <- data %>% 
    # analyze at the task level
    group_by(task, id, survey)
  
  
  # only tasks with one D+ and one D-
  if(condition == "legislature"){
    results <- filter(results, length(unique(`Legislative checks`)) == 2)
  }else if(condition == "courts"){
    results <- filter(results, length(unique(`Judicial checks`)) == 2)
  }else{
    stop("The 'condition' input is not valid.")
  }
  
  # indicator for same lockdown preference/party between candidate and respondent
  if(defect_cause == "lockdown"){
    results <- results %>% 
      mutate(congruence = ifelse(covid_lockdowns == `Lockdown policy`,1,0),
             congruence_length = length(unique(congruence)))
  }else if(defect_cause == "party"){
    results <- results %>% 
      mutate(congruence = ifelse(partyid == Party,1,0),
             congruence_length = length(unique(congruence)))
  }else{
    stop("The 'defect_cause' input is not valid.")
  }
  
  # analyzing just the D- candidate
  if(condition == "legislature"){
    results <- filter(results, `Legislative checks` == "Shut down legislature")
  }else{
    results <- filter(results, `Judicial checks` == "Ignore courts")
  }
  
  # Results by scenario
  results <- results %>% 
    ungroup() %>% 
    group_by(congruence, congruence_length, Country, eval(parse(text=by))) %>% 
    dplyr::summarise(mean = mean(selected, na.rm = T),
                     lwr = lwr_conf(selected),
                     upr = upr_conf(selected),
                     n = n())
  
  if(by != "NULL"){
    results <- dplyr::rename(results, by = `eval(parse(text = by))`)
  }
  
  results <- results %>% 
    mutate(
      scenario = case_when(
        congruence == FALSE & congruence_length == 1 ~ "Neither candidate",
        congruence == TRUE & congruence_length == 2 ~ "Only D- candidate",
        congruence == FALSE & congruence_length == 2 ~ "Only D+ candidate",
        congruence == TRUE & congruence_length == 1 ~ "Both candidates"
      ),
      defection_cause = defect_cause
    ) %>% 
    mutate(scenario = factor(scenario, levels = c("Only D+ candidate",
                                            "Both candidates", 
                                            "Neither candidate",
                                            "Only D- candidate")),
           label = paste0(round(mean,3), " [", round(lwr,3), ",", round(upr,3),"]", "n=", n))
  
  return(results)
}