# This script generates figures in the main text

rm(list = ls())
library(dplyr)
library(tidyr)
library(ggplot2)
library(foreach)
library(doParallel)

output_dir <- "Final_Figures"
if (!dir.exists(output_dir)) dir.create(output_dir)

grid_params <- expand.grid(
  Patience = seq(0.2, 1.0, by = 0.2), 
  Condition = c("H", "L", "HL", "LH"), 
  stringsAsFactors = FALSE
)
subjectnum_test <- 10

cl <- makeCluster(detectCores() - 1)
registerDoParallel(cl)

# Simulation 1:  The Speed Paradox 

sim1_data <- foreach(i = 1:nrow(grid_params), .combine = bind_rows) %dopar% {
  suppressMessages(library(dplyr))
  suppressMessages(library(tidyr))
  source("model_functions(1).R", local = TRUE) 
  
  p <- grid_params$Patience[i]
  c <- grid_params$Condition[i]
  params <- data.frame(Subject=1:subjectnum_test, a_schema=0.2, h_schema=1000, Beta_N=0.2, Beta_Var=0.3, 
                       a_generic=0.1, h_generic=1500, Beta_gN=0.1, Beta_gVar=0.2, w=0.3, Phi=20, 
                       decay_speed=0.999, decay_speed_thres=0.999, theta_shift=3, timevar=0.0001, 
                       modeltimestep=0.061, patience=p, thres_schema=50*p, thres_item_inter=6*p, thres_item_final=13.75*p)
  
  res <- tryCatch({ simulation(params, type=c, exp_type="painting", sim.mode="whole", scale.confi.init=FALSE, model.version=1) }, 
                  error = function(e) { list(allresult_processed=NULL) })
  if (!is.null(res$allresult_processed) && nrow(res$allresult_processed) > 0) {
    res$allresult_processed %>% group_by(Subject) %>% summarise(Total_Score=sum(performance, na.rm=TRUE), .groups="drop") %>% mutate(Patience=p, Condition=c, Status="Success")
  } else {
    data.frame(Subject=NA, Total_Score=0, Patience=p, Condition=c, Status="Failed")
  }
}

# Simulation 2: Strict Economy (Comparison: Old vs New)

sim2_data <- foreach(i = 1:nrow(grid_params), .combine = bind_rows) %dopar% {
  suppressMessages(library(dplyr))
  suppressMessages(library(tidyr))
  source("model_functions(1).R", local = TRUE) 
  
  computeAC_performance <- function(risk, Schema_res, Item_EI, Outputs_cho, ThisPhase, schemainfo) {
    max_count <- max(table(Schema_res))
    base_payoff <- sum(Item_EI$payoff[Item_EI$ID %in% Outputs_cho[ThisPhase,c("Cho_1","Cho_2","Cho_3","Cho_4")]])
    if (max_count == 4) { 
      return(list(1.0, base_payoff * 5, 0))
    } else if (max_count == 3) { 
      return(list(0.5, base_payoff * 3, 0))
    } else { 
      return(list(0.0, 0, 0)) 
    }
  }
  assign("computeAC_performance", computeAC_performance, envir = environment())
  
  p <- grid_params$Patience[i]
  c <- grid_params$Condition[i]
  params <- data.frame(Subject=1:subjectnum_test, a_schema=0.2, h_schema=1000, Beta_N=0.2, Beta_Var=0.3, 
                       a_generic=0.1, h_generic=1500, Beta_gN=0.1, Beta_gVar=0.2, w=0.3, Phi=20, 
                       decay_speed=0.999, decay_speed_thres=0.999, theta_shift=3, timevar=0.0001, 
                       modeltimestep=0.061, patience=p, thres_schema=50*p, thres_item_inter=6*p, thres_item_final=13.75*p)
  
  res <- tryCatch({ simulation(params, type=c, exp_type="painting", sim.mode="whole", scale.confi.init=FALSE, model.version=1) }, 
                  error = function(e) { list(allresult_processed=NULL) })
  if (!is.null(res$allresult_processed) && nrow(res$allresult_processed) > 0) {
    res$allresult_processed %>% group_by(Subject) %>% summarise(Total_Score=sum(performance, na.rm=TRUE), .groups="drop") %>% mutate(Patience=p, Condition=c, Status="Success")
  } else {
    data.frame(Subject=NA, Total_Score=0, Patience=p, Condition=c, Status="Failed")
  }
}

stopCluster(cl)

# Plot Figure 1

summary_old <- sim1_data %>% filter(Status == "Success") %>% 
  group_by(Condition, Patience) %>% 
  summarise(Avg_Score = mean(Total_Score), SE_Score = sd(Total_Score)/sqrt(n()), .groups="drop") %>%
  mutate(Economy = "Old")
summary_old$Condition <- factor(summary_old$Condition, levels = c("H", "L", "HL", "LH"))

fig1 <- ggplot(summary_old, aes(x=Patience, y=Avg_Score, color=Condition)) + 
  geom_line(size=1.2) + 
  geom_point(aes(fill=Condition), size=3, shape=21, color="white", stroke=1) + 
  geom_ribbon(aes(ymin=Avg_Score-SE_Score, ymax=Avg_Score+SE_Score, fill=Condition), alpha=0.15, color=NA) + 
  theme_bw(base_size=14) + 
  facet_wrap(~ Condition, ncol=2) + 
  scale_x_continuous(breaks=seq(0.2, 1.0, 0.2)) + 
  scale_color_manual(values=c("H"="#f8766d", "L"="#7cae00", "HL"="#00bfc4", "LH"="#c77cff")) +
  scale_fill_manual(values=c("H"="#f8766d", "L"="#7cae00", "HL"="#00bfc4", "LH"="#c77cff")) +
  labs(title="Non-linear Impact of Patience on Total Reward (N=10)", 
       x="Patience Ratio (Thresholds & Exploration Time Scale)", 
       y="Average Total Score") + 
  theme(strip.background=element_rect(fill="#f0f0f0"), strip.text=element_text(face="bold"), legend.position="none")

ggsave(file.path(output_dir, "Figure_1_Speed_Paradox.png"), fig1, width=10, height=8, dpi=300)


# Plot Figure 2: 

summary_new <- sim2_data %>% filter(Status == "Success") %>% 
  group_by(Condition, Patience) %>% 
  summarise(Avg_Score = mean(Total_Score), SE_Score = sd(Total_Score)/sqrt(n()), .groups="drop") %>%
  mutate(Economy = "New")

combined_summary <- bind_rows(summary_old, summary_new)
combined_summary$Condition <- factor(combined_summary$Condition, levels = c("H", "L", "HL", "LH"))
combined_summary$Economy <- factor(combined_summary$Economy, levels = c("Old", "New"))

fig2 <- ggplot(combined_summary, aes(x=Patience, y=Avg_Score, color=Economy, group=Economy)) + 
  geom_line(size=1.2) + 
  geom_point(aes(fill=Economy), size=3, shape=21, color="white", stroke=1) + 
  geom_ribbon(aes(ymin=Avg_Score-SE_Score, ymax=Avg_Score+SE_Score, fill=Economy), alpha=0.15, color=NA) + 
  theme_bw(base_size=14) + 
  facet_wrap(~ Condition, ncol=2, scales="free_y") + 
  scale_x_continuous(breaks=seq(0.2, 1.0, 0.2)) + 
  scale_color_manual(values=c("Old"="#e41a1c", "New"="#31a354")) +
  scale_fill_manual(values=c("Old"="#e41a1c", "New"="#31a354")) +
  labs(title="", 
       x="Patience", 
       y="Avg_Score") + 
  theme(strip.background=element_rect(fill="#f0f0f0"), strip.text=element_text(face="bold"), legend.position="bottom")

ggsave(file.path(output_dir, "Figure_2_Strict_Economy.png"), fig2, width=10, height=6.5, dpi=300)