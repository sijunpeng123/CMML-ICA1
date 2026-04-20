# ==============================================================================
# Supplementary Figure 2: Sim Model vs Baseline (V2) 
# ==============================================================================
library(dplyr)
library(tidyr)
library(ggplot2)
library(foreach)
library(doParallel)
library(patchwork)

dir.create("figures", showWarnings = FALSE)

theme_report <- function() {
  theme_bw(base_size = 13) +
    theme(
      text = element_text(family = "sans"),
      plot.title = element_text(face = "bold", size = 13),
      strip.background = element_rect(fill = "#f0f0f0"),
      strip.text = element_text(face = "bold"),
      legend.position = "bottom",
      panel.grid.major.x = element_blank()
    )
}

patience_levels <- c(1.0, 0.2)  
cond_list <- c("H", "L", "HL", "LH")
model_types <- c("V2", "Sim") 
subjectnum_test <- 10  

grid_params <- expand.grid(Patience = patience_levels, 
                           Condition = cond_list, 
                           Model_Type = model_types, 
                           stringsAsFactors = FALSE)

cl <- makeCluster(detectCores() - 1)
registerDoParallel(cl)

cat("Running data collection for Supplementary Fig 2...\n")

supp_sim_data <- foreach(i = 1:nrow(grid_params), .combine = dplyr::bind_rows) %dopar% {
  suppressMessages(library(dplyr))
  suppressMessages(library(tidyr))
  
  curr_p <- grid_params$Patience[i]
  curr_c <- grid_params$Condition[i]
  curr_m <- grid_params$Model_Type[i]
  
  if (curr_m == "Sim") {
    source("model_functions_sim.R", local = TRUE) 
    run_version <- 1 
  } else {
    source("model_functions(1).R", local = TRUE) 
    run_version <- as.numeric(gsub("V", "", curr_m)) 
  }
  
  computeAC_performance <- function(risk, Schema_res, Item_EI, Outputs_cho, ThisPhase, schemainfo) {
    if (risk == 'high'){ 
      if (length(unique(Schema_res)) == 1){ AC = 1; performance = sum(Item_EI$payoff[Item_EI$ID %in% Outputs_cho[ThisPhase,c("Cho_1","Cho_2","Cho_3","Cho_4")]]) * 3
      } else if(length(unique(Schema_res)) == 2 & sum(Schema_res == unique(Schema_res)[1]) != 2){ AC = 0.5; performance = sum(Item_EI$payoff[Item_EI$ID %in% Outputs_cho[ThisPhase,c("Cho_1","Cho_2","Cho_3","Cho_4")]])
      } else { AC = 0; performance = sum(Item_EI$payoff[Item_EI$ID %in% Outputs_cho[ThisPhase,c("Cho_1","Cho_2","Cho_3","Cho_4")]]) }
    } else { 
      if (length(unique(Schema_res)) == 1){ AC = 1; performance = sum(Item_EI$payoff[Item_EI$ID %in% Outputs_cho[ThisPhase,c("Cho_1","Cho_2","Cho_3","Cho_4")]]) * 3
      } else if(length(unique(Schema_res)) == 2 & sum(Schema_res == unique(Schema_res)[1]) != 2){ AC = 0.5; right_schema = as.data.frame(sort(table(Schema_res), decreasing=TRUE))[1,1]; performance = sum(Item_EI$payoff[Item_EI$ID %in% Outputs_cho[ThisPhase,c("Cho_1","Cho_2","Cho_3","Cho_4")]]) + schemainfo$payoff[schemainfo$schemaID==right_schema]*3
      } else { AC = 0; performance = sum(Item_EI$payoff[Item_EI$ID %in% Outputs_cho[ThisPhase,c("Cho_1","Cho_2","Cho_3","Cho_4")]]) }
    }
    return(list(AC, performance, 0))
  }
  assign("computeAC_performance", computeAC_performance, envir = environment())
  
  Param.df <- data.frame(Subject=1:subjectnum_test, a_schema=0.2, h_schema=1000, Beta_N=0.2, Beta_Var=0.3, a_generic=0.1, h_generic=1500, Beta_gN=0.1, Beta_gVar=0.2, w=0.3, Phi=20, decay_speed=0.999, decay_speed_thres=0.999, theta_shift=3, timevar=0.0001, modeltimestep=0.061, patience=curr_p, thres_schema=50*curr_p, thres_item_inter=6*curr_p, thres_item_final=13.75*curr_p)
  
  res <- tryCatch({ simulation(Param.df, type = curr_c, exp_type = "painting", save = FALSE, sim.mode = "whole", scale.confi.init = FALSE, model.version = run_version) }, error = function(e) { list(allresult_processed = NULL) })
  
  df <- res$allresult_processed
  if (!is.null(df) && nrow(df) > 0) {
    df %>% group_by(Subject) %>% 
      summarise(Total_Score = sum(performance, na.rm = TRUE), 
                Mean_Accuracy = mean(AC, na.rm = TRUE), 
                Completed_Rounds = n(),
                .groups = "drop") %>% 
      mutate(Patience = curr_p, Condition = curr_c, Model_Type = curr_m, Status="Success")
  } else {
    data.frame(Subject=NA, Total_Score=0, Mean_Accuracy=NA, Completed_Rounds=NA, Patience=curr_p, Condition=curr_c, Model_Type=curr_m, Status="Failed")
  }
}
stopCluster(cl)

valid_data <- supp_sim_data %>% filter(Status == "Success" & !is.na(Subject))

summary_stats <- valid_data %>%
  group_by(Model_Type, Condition, Patience) %>%
  summarise(
    Avg_Score = mean(Total_Score), SE_Score = sd(Total_Score) / sqrt(n()),
    Avg_AC = mean(Mean_Accuracy), SE_AC = sd(Mean_Accuracy) / sqrt(n()),
    Avg_Rounds = mean(Completed_Rounds), SE_Rounds = sd(Completed_Rounds) / sqrt(n()),
    .groups = "drop"
  ) %>%
  mutate(Strategy = ifelse(Patience == 1.0, "Normal (P=1.0)", "Give-up (P=0.2)"))

summary_stats$Strategy <- factor(summary_stats$Strategy, levels = c("Normal (P=1.0)", "Give-up (P=0.2)"))
summary_stats$Condition <- factor(summary_stats$Condition, levels = c("H", "L", "HL", "LH"))
summary_stats$Model_Type <- factor(summary_stats$Model_Type, levels = c("V2", "Sim"))

fill_colors <- c("V2" = "#bdbdbd", "Sim" = "#756bb1")

p_ac <- ggplot(summary_stats, aes(x = Condition, y = Avg_AC, fill = Model_Type)) +
  geom_bar(stat="identity", position=position_dodge(width=0.8), width=0.7, color="black") +
  geom_errorbar(aes(ymin = Avg_AC - SE_AC, ymax = Avg_AC + SE_AC), position=position_dodge(width=0.8), width=0.2) +
  facet_wrap(~ Strategy) + scale_fill_manual(values = fill_colors) + theme_report() +
  labs(title = "A: Mean Accuracy", y = "Accuracy", x = NULL) + 
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

p_rounds <- ggplot(summary_stats, aes(x = Condition, y = Avg_Rounds, fill = Model_Type)) +
  geom_bar(stat="identity", position=position_dodge(width=0.8), width=0.7, color="black") +
  geom_errorbar(aes(ymin = Avg_Rounds - SE_Rounds, ymax = Avg_Rounds + SE_Rounds), position=position_dodge(width=0.8), width=0.2) +
  facet_wrap(~ Strategy) + scale_fill_manual(values = fill_colors) + theme_report() +
  labs(title = "B: Completed Rounds", y = "Rounds", x = NULL) +
  theme(strip.text = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank())

p_score <- ggplot(summary_stats, aes(x = Condition, y = Avg_Score, fill = Model_Type)) +
  geom_bar(stat="identity", position=position_dodge(width=0.8), width=0.7, color="black") +
  geom_errorbar(aes(ymin = Avg_Score - SE_Score, ymax = Avg_Score + SE_Score), position=position_dodge(width=0.8), width=0.2) +
  facet_wrap(~ Strategy) + scale_fill_manual(values = fill_colors) + theme_report() +
  labs(title = "C: Total Score", y = "Total Score", x = "Condition") +
  theme(strip.text = element_blank())

final_plot <- p_ac / p_rounds / p_score + plot_layout(guides = "collect") & theme(legend.position = 'bottom')

ggsave("figures/Supplementary_Fig_2.png", plot = final_plot, width = 8, height = 10, dpi = 300)
cat("Supplementary Fig 2 generated successfully.\n")