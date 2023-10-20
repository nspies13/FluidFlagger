# Helper functions
simulateContaminationRow <- function(input, mix_ratio, fluid){
  
  cols <- names(fluid)[which(names(fluid) %in% names(input))]
  
  output <- input %>%
    dplyr::mutate(across(all_of(cols), ~(1 - mix_ratio) * . + fluid[cur_column()] * mix_ratio)) %>%
    select(all_of(cols))
  
  output %>% 
    mutate(across(c("sodium", "chloride", "co2_totl", "bun", "glucose"), ~round(.))) %>%
    mutate(across(c("potassium_plas", "calcium"), ~round(., 1))) %>%
    mutate(creatinine = round(creatinine, 2)) %>% 
    mutate(anion_gap = sodium - chloride - co2_totl) %>%
    mutate(mix_ratio = mix_ratio)
  
}

getDistanceFromFluid <- function(input, fluid_name, analytes_to_compare = c("sodium", "chloride", "potassium_plas", "co2_totl", "calcium", "glucose")){
  
  proxy::dist(input %>% select(all_of(analytes_to_compare)), matrix(get(fluid_name)[str_replace_all(analytes_to_compare, "_prior|_post", "")], nrow = 1))
  
}

simulateContaminationRow <- function(input, mix_ratio, fluid){
  
  cols <- names(fluid)[which(names(fluid) %in% names(input))]
  
  output <- input %>%
    dplyr::mutate(across(all_of(cols), ~(1 - mix_ratio) * . + fluid[cur_column()] * mix_ratio)) %>%
    select(all_of(cols))
  
  output %>% 
    mutate(across(c("sodium", "chloride", "co2_totl", "bun", "glucose"), ~round(.))) %>%
    mutate(across(c("potassium_plas", "calcium"), ~round(., 1))) %>%
    mutate(creatinine = round(creatinine, 2)) %>% 
    mutate(anion_gap = sodium - chloride - co2_totl) %>%
    mutate(mix_ratio = mix_ratio)
  
}

makeMixRatios <- function(contam_rate = 0.1, contam_train_input, fluid_names_tar){
  
  minimum_significant_contamination = ifelse(grepl("D5", fluid_names_tar), 0.01, 0.1)
  
  out = round(rbeta(contam_rate * nrow(contam_train_input), 1, 6), 2) + minimum_significant_contamination
  out[out >= 1] <- 0.99
  
  out
  
}

makeContaminationInput <- function(input){
  
  long <- input %>% pivot_longer(any_of(lab_strings), names_to = "task_assay", values_to = "result_val", values_drop_na = T)
  
  priors <- long %>% arrange(drawn_dt_tm, performed_dt_tm, verified_dt_tm) %>% group_by(patient_id, task_assay) %>% mutate(prior = lag(result_val)) %>% pivot_wider(id_cols = matches("_id"), names_from = "task_assay", values_from = "prior", names_glue = "{task_assay}_prior", values_fn = last)
  posts <- long %>% arrange(drawn_dt_tm, performed_dt_tm, verified_dt_tm) %>% group_by(patient_id, task_assay) %>% mutate(post = lead(result_val)) %>% pivot_wider(id_cols = matches("_id"), names_from = "task_assay", values_from = "post", names_glue = "{task_assay}_post", values_fn = last)
  
  full <- full_join(input, priors) %>% full_join(posts) %>% drop_na(any_of(lab_strings), matches("_prior"))
  
  full
  
}

makeContaminationTibble <- function(input, mix_ratios, fluid, fluid_name){
  
  input = input %>% mutate(across(any_of(lab_strings), ~.x, .names = "{col}_real"))
  split = initial_split(input, prop = length(mix_ratios)/nrow(input))
  
  unsim_rows <- testing(split)
  sim_rows <- training(split)
  
  sim_rows = sim_rows %>% mutate(mix_ratio = mix_ratios, label = fluid_name)
  unsim_rows = unsim_rows %>% mutate(mix_ratio = 0, label = "Patient")
  
  tmp = simulateContaminationRow(sim_rows, sim_rows$mix_ratio, fluid)
  
  sim_rows[,names(tmp)] <- tmp
  
  output = 
    bind_rows(unsim_rows, sim_rows) %>% 
    mutate(across(any_of(lab_strings), ~ . - get(paste0(cur_column(), "_prior")), .names = "{col}_delta"))
  
  output
  
}

makeFullContaminationTibble <- function(input, mix_ratios, fluid, fluid_name){
  
  input_prep = 
    input %>% drop_na(any_of(lab_strings)) %>%
    slice_sample(n = length(mix_ratios), replace = T) %>% 
    mutate(across(any_of(lab_strings), ~.x, .names = "{col}_real"), mix_ratio = mix_ratios, label = fluid_name)
  
  tmp = simulateContaminationRow(input_prep, input_prep$mix_ratio, fluid)
  
  input_prep[,names(tmp)] <- tmp
  
  output = 
    input_prep %>%
    mutate(across(any_of(lab_strings), ~ . - get(paste0(cur_column(), "_prior")), .names = "{col}_delta"),
           across(any_of(lab_strings), ~ get(paste0(cur_column(), "_delta"))/get(paste0(cur_column(), "_prior")), .names = "{col}_prop"))
  
  output
  
  
}

plotContamSimDistributions <- function(contam_sim_full, label = "NS"){
  
  gg_input_wide <- contam_sim_full %>% select(mix_ratio, label, any_of(lab_strings), matches("prop"), -matches("anion_gap"))
  gg_input_long <- gg_input_wide %>% pivot_longer(cols = c(any_of(lab_strings), matches("prop")), names_to = "analyte", values_to = "result_val")
  
  gg_input_results <- gg_input_long %>% filter(!grepl("prop", analyte)) %>% group_by(mix_ratio, label, analyte) %>% reframe(low = quantile(result_val, 0.05), high = quantile(result_val, 0.95))
  
  gg <- ggplot(gg_input_results, aes(x = mix_ratio, xend = mix_ratio, y = low, yend = high, color = label)) + 
    geom_segment(show.legend = F) + 
    scale_color_manual(values = color_map_global) + 
    scale_x_continuous(breaks = c(0, 0.5, 1), labels = c("0", "0.5", "1")) + 
    ylab("Simulated Results") + xlab("Mixture Ratio") +
    facet_wrap(~factor(analyte, levels = lab_strings_bmp), scales = "free", labeller = as_labeller(analyte_labels), nrow = 2)
  ggsave(paste0("../Figures/Contamination/Simulation/", label, "_contamination_sim.pdf"), gg, width = 10, height = 6)
  
  gg
  
}

restrictFluids <- function(contam_sim, fluid_name){
  
  contam_sim %>% 
    filter(label %in% c("Patient", fluid_name)) %>% 
    mutate(target = factor(ifelse(label == "Patient", 0, 1), levels = c(0, 1)))
  
}

getMixRatios <- function(fluid_name, contam_train_input, contam_rate = 0.1, minimum_significant_ratio = 0.1, minimum_significant_ratio_dextrose = 0.01){
  
  mix_ratio = ifelse(grepl("D5",fluid_name), round(rbeta(contam_rate * nrow(contam_train_input), 1, 6), 2) + minimum_significant_ratio_dextrose, round(rbeta(contam_rate * nrow(contam_train_input), 1, 6), 2) + minimum_significant_ratio)
  
  mix_ratio
  
}

