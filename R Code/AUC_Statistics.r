###pROC###
library(pROC)

AUC1 <- read.csv("Path_to_File.csv", header=TRUE)
AUC2 <- read.csv("Path_to_File.csv", header=TRUE)
AUC3 <- read.csv("Path_to_File.csv", header=TRUE)
AUC4 <- read.csv("Path_to_File.csv", header=TRUE)
AUC5 <- read.csv("Path_to_File.csv", header=TRUE)

AUC1pROC <- roc(AUC1$Pred, AUC1$Prob)
AUC2pROC <- roc(AUC2$Pred, AUC2$Prob)
AUC3pROC <- roc(AUC3$Pred, AUC3$Prob)
AUC4pROC <- roc(AUC4$Pred, AUC4$Prob)
AUC5pROC <- roc(AUC5$Pred, AUC5$Prob)

plot(AUC1pROC, col = "lightcoral", lwd = 5, legacy.axes = TRUE, xlim=c(1, 0), print.auc = TRUE, print.auc.x = 0.3, print.auc.y = 0.7)
plot(AUC2pROC, col = "green3", add = TRUE, lwd = 5, print.auc = TRUE, print.auc.x = 0.3, print.auc.y = 0.6)
plot(AUC3pROC, col = "turquoise3", add = TRUE, lwd = 5, print.auc = TRUE, print.auc.x = 0.3, print.auc.y = 0.5)
plot(AUC4pROC, col = "plum2", add = TRUE, lwd = 5, print.auc = TRUE, print.auc.x = 0.3, print.auc.y = 0.4)
title(main = "PostDiet", line=2.5)

# Add legend
##legend("bottomright",
      # legend=c("Compiled", "PostDiet M-Mode", "PostDiet PW", "PostDiet Stress-Strain"),
      # col=c("lightcoral", "green3", "turquoise3", "plum2"),
       #lwd=3, cex =0.6,xpd = TRUE, horiz = FALSE)

plot(AUC5pROC, col = "gold1", lwd = 5, legacy.axes = TRUE, xlim=c(1, 0), print.auc = TRUE, print.auc.x = 0.3, print.auc.y = 0.5)
title(main = "PostStress", line=2.5)

plot(roc.s100b, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),
     grid.col=c("green", "red"), max.auc.polygon=TRUE,
     auc.polygon.col="lightblue", print.thres=TRUE)


# Thresholds
ci.thresolds.obj <- ci.thresholds(roc.s100b)
plot(ci.thresolds.obj)

# Specificities
plot(roc.s100b) # restart a new plot
ci.sp.obj <- ci.sp(roc.s100b, boot.n=500)
plot(ci.sp.obj)


# Sensitivities
plot(roc.s100b) # restart a new plot
ci.se.obj <- ci(roc.s100b, of="se", boot.n=500)
plot(ci.se.obj)


####Compare Multiple Classes ROC-AUC####
library(multiROC)

Multi <- data.frame(read.csv(
  file = 'Path_to_File.csv'))

res <- multi_roc(Multi, force_diag=T)
unlist(res$AUC)

multi_roc_auc <- function(true_pred_data, idx) {
  results <- multi_roc(true_pred_data[idx, ])$AUC
  results <- unlist(results)
  return(results)
}

roc_auc_with_ci_res <- roc_auc_with_ci(Multi, conf= 0.95, type='basic', R = 1000)
roc_auc_with_ci_res

#roc_test <- multi_roc(Multi)
#roc_test$Sensitivity
#roc_test$Specificity

n_method <- length(unique(res$Methods))
n_group <- length(unique(res$Groups))
res_df <- data.frame(Specificity= numeric(0), Sensitivity= numeric(0), Group = character(0), AUC = numeric(0), Method = character(0))
for (i in 1:n_method) {
  for (j in 1:n_group) {
    temp_data_1 <- data.frame(Specificity=res$Specificity[[i]][j],
                              Sensitivity=res$Sensitivity[[i]][j],
                              Group=unique(res$Groups)[j],
                              AUC=res$AUC[[i]][j],
                              Method = unique(res$Methods)[i])
    colnames(temp_data_1) <- c("Specificity", "Sensitivity", "Group", "AUC", "Method")
    res_df <- rbind(res_df, temp_data_1)
    
  }
}

ggplot2::ggplot(res_df, ggplot2::aes(x = 1-Specificity, y=Sensitivity)) + ggplot2::geom_path(ggplot2::aes(color = Group), size=2) + ggplot2::geom_segment(ggplot2::aes(x = 0, y = 0, xend = 1, yend = 1),
      colour='grey', linetype = 'dotdash') + ggplot2::theme_bw() + ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5), legend.justification=c(1, 0),
      legend.position=c(0.99, .01), legend.title=ggplot2::element_blank(),
      legend.background = ggplot2::element_rect(fill=NULL, size=0.5,
      linetype="solid", colour ="black"))
