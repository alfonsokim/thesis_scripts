
source("c:\\Users\\Alfonso\\workspace\\thesis_scripts\\scalability_study\\summary.R")

scalability_plot <- function(data, measure_var, value_var, group_var, colour="blue", size=0.5) {
  
  library("ggplot2")
  
  data_se <- summarySE(data, measurevar=measure_var, groupvars=c(value_var, group_var))
  
  data_se[,value_var] = as.factor(data_se[,value_var])
  data_se[,group_var] = as.factor(data_se[,group_var])
  data_se$y_min = data_se[,measure_var] - data_se$ci
  data_se$y_max = data_se[,measure_var] + data_se$ci
  
  layers <- list(
    geom_line(
      aes_string(x=value_var, y=measure_var, ymax=max(measure_var), group=group_var),
      data=data_se, position=position_dodge(0.1), size=size, colour=colour, 
      show_guide=TRUE,
      environment = environment()
    ),
    geom_errorbar(
      aes_string(x=value_var, y=measure_var, group=group_var, ymin="y_min", ymax="y_max"),
      data=data_se, colour="black", width=0.2, position=position_dodge(0.1),
      environment = environment()
    ),
    geom_point(
      aes_string(x=value_var, y=measure_var, ymax=max(measure_var), group=group_var),
      data=data_se, position=position_dodge(0.1), size=3,
      colour=colour, show_guide=TRUE,
      environment = environment()
    )
  )
  
  return(layers)
}