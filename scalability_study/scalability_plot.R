
source("c:\\Users\\Alfonso\\workspace\\thesis_scripts\\scalability_study\\summary.R")

scalability_plot <- function(data, measure_var, value_var, group_var) {
  
  library("ggplot2")
  
  data_se <- summarySE(data, measurevar=measure_var, groupvars=c(value_var, group_var))
  
  data_se[,value_var] = as.factor(data_se[,value_var])
  data_se[,group_var] = as.factor(data_se[,group_var])
  data_se$y_min = data_se[,measure_var] - data_se$ci
  data_se$y_max = data_se[,measure_var] + data_se$ci
  
  s_plot <- ggplot(data_se, aes_string(x=value_var, y=measure_var, group=group_var),
                   environment = environment()) + 
    geom_errorbar(aes(ymin=y_min, ymax=y_max),
                  colour="black", width=0.2, position=position_dodge(0.1),
                  environment = environment()) +
    geom_line(position=position_dodge(0.1), size=0.5, colour="blue") +
    geom_point(position=position_dodge(0.1), size=3)
  
  return(s_plot)
}