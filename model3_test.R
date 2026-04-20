# ==============================================================================
# CMML3 Model Evaluation: Stability and Failure Rate Analysis
# Purpose: Comparing task failure rates across baseline variants (V1-V3)
# ==============================================================================

rm(list = ls())
library(dplyr)
library(tidyr)
library(ggplot2)
library(foreach)
library(doParallel)

# Setup output directory
output_dir <- "Model_Stability_Report"
if (!dir.exists(output_dir)) dir.create(output_dir)

# Simulation parameters
patience_levels <- c(0.2, 0.6, 1.0)
cond_list <- c("H", "L", "HL", "LH")
models_to_test <- c(1, 2, 3) 
subjectnum_test <- 10  

# Create parameter grid
grid_params <- expand.grid(
  Patience = patience_levels, 
  Condition = cond_list, 
  Version = models_to_test, 
  stringsAsFactors = FALSE
)

# Initialize parallel processing
num_cores <- detectCores() - 1  
cl <- makeCluster(num_cores)
registerDoParallel(cl)

message("Executing stability diagnostic tests...")

# Main simulation loop
survival_data <- foreach(i = 1:nrow(grid_params), .combine = dplyr::bind_rows) %dopar% {
  
  # Load necessary libraries and source functions within each worker node
  suppressMessages(library(dplyr))
  suppressMessages(library(tidyr))
  source("model_functions(1).R", local = TRUE) 
  
  curr_p <- grid_params$Patience[i]
  curr_c <- grid_params$Condition[i]
  curr_v <- grid_params$Version[i]
  
  # Set model parameters based on current patience scalar [cite: 185, 252]
  Param.df <- data.frame(
    Subject = 1:subjectnum_test, 
    a_schema = 0.2, h_schema = 1000, Beta_N = 0.2, Beta_Var = 0.3,
    a_generic = 0.1, h_generic = 1500, Beta_gN = 0.1, Beta_gVar = 0.2,
    w = 0.3, Phi = 20, decay_speed = 0.999, decay_speed_thres = 0.999,
    theta_shift = 3, timevar = 0.0001, modeltimestep = 0.061,
    patience = curr_p,
    thres_schema = 50 * curr_p,  
    thres_item_inter = 6 * curr_p, 
    thres_item_final = 13.75 * curr_p
  )
  
  # Error handling to capture simulation timeouts or crashes
  true_error_msg <- "No Error"
  
  res <- tryCatch({
    sim_out <- simulation(Param.df, type = curr_c, exp_type = "painting", 
                          save = FALSE, sim.mode = "whole", scale.confi.init = FALSE, 
                          model.version = curr_v)
    sim_out
  }, error = function(e) { 
    true_error_msg <<- e$message
    return(list(allresult_processed = NULL)) 
  })
  
  df <- res$allresult_processed
  
  # Calculate failure rates
  if (is.null(df) || nrow(df) == 0) {
    crash_rate <- 100.0
  } else {
    actual_subjects <- length(unique(df$Subject))
    crash_rate <- ((subjectnum_test - actual_subjects) / subjectnum_test) * 100
  }
  
  return(data.frame(
    Model_Version = paste0("V", curr_v),
    Condition = curr_c,
    Patience = curr_p,
    Crash_Rate = crash_rate,
    Survival_Rate = 100 - crash_rate,
    Error_Message = true_error_msg
  ))
}

stopCluster(cl)
message("Diagnostic tests completed.")

# Data processing for visualization
survival_data$Model_Version <- factor(survival_data$Model_Version, levels = c("V1", "V2", "V3"))
survival_data$Condition <- factor(survival_data$Condition, levels = c("H", "L", "HL", "LH"))
survival_data$Patience_Label <- factor(paste0("Patience = ", survival_data$Patience), 
                                       levels = c("Patience = 1", "Patience = 0.6", "Patience = 0.2"))

# Formal plotting
p_crash <- ggplot(survival_data, aes(x = Model_Version, y = Crash_Rate, fill = Model_Version)) +
  geom_bar(stat = "identity", color = "black", width = 0.7) +
  facet_grid(Condition ~ Patience_Label) + 
  theme_bw(base_size = 14) +
  scale_fill_manual(values = c("V1" = "#3182bd", "V2" = "#31a354", "V3" = "#de2d26")) +
  scale_y_continuous(limits = c(0, 115), breaks = seq(0, 100, 25)) +
  labs(title = "Task Failure Rates Across Model Versions",
       x = "Model Variant",
       y = "Failure / Timeout Rate (%)",
       fill = "Model Version") +
  theme(strip.background = element_rect(fill = "#f0f0f0"), 
        strip.text = element_text(face = "bold", size = 11),
        legend.position = "bottom") +
  geom_text(aes(label = ifelse(Crash_Rate > 0, paste0(Crash_Rate, "%"), "")), 
            vjust = -0.5, size = 3.5, fontface = "bold")

# Save final plot
ggsave(file.path(output_dir, "Supplementary_Figure_1.png"), plot = p_crash, width = 12, height = 10, dpi = 300)
message("Visualization generated.")