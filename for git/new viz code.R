library(ggplot2)
library(gridExtra)
library(ggpubr)
library(dplyr)
library(tidyr)

#we assume the data file contains the continuous variables values and their SHAP values
#the data in this file was generated and doesn't contain real data
df_join <- read.csv("COPD_Read30_features_and_shapValues_generated1.csv", stringsAsFactors = T)

#SHAP Exploration plot 

###y=NumberEDVisitsInTheLastHalfYear
min_value<-floor(min(df_join$NumberEDVisitsInTheLastHalfYear))
max_value<-ceiling(max(df_join$NumberEDVisitsInTheLastHalfYear))  
p_a <- ggplot(df_join, aes(x = NumberEDVisitsInTheLastHalfYear, y = NumberEDVisitsInTheLastHalfYear_SHAP_value, color = NumberEDVisitsInTheLastHalfYear_SHAP_value)) +
  geom_point(size = 1, alpha = 0.7) +
  scale_color_gradientn(name = "SHAP Value", colours = c("blue", "lightgrey", "red")) +
  #scale_x_continuous(breaks = seq(min_value,max_value, by = 1)) + 
  scale_x_continuous(limits = c(min_value,max_value)) + 
  labs(x = "Number ED Visits In The Last Half Year") +
  theme_minimal() +
  theme(axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank())

###y=age
min_value<-floor(min(df_join$Age))
max_value<-ceiling(max(df_join$Age)) 
p_b <- ggplot(df_join, aes(x = Age, y = Age_SHAP_Value, color = Age_SHAP_Value)) +
  geom_point(size = 1, alpha = 0.7) +
  scale_color_gradientn(name = "SHAP Value", colours = c("blue", "lightgrey", "red")) +
  #scale_x_continuous(breaks = seq(min_value,max_value, by = 10)) + 
  scale_x_continuous(limits = c(min_value,max_value)) +
  labs(x = "Age") +
  theme_minimal() +
  theme(axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank())

###y=NumberOfCasesPerPhysician
min_value<-floor(min(df_join$NumberOfCasesPerPhysician))
max_value<-ceiling(max(df_join$NumberOfCasesPerPhysician))
p_c <- ggplot(df_join, aes(x = NumberOfCasesPerPhysician, y = NumberOfCasesPerPhysician_SHAP_value, color = NumberOfCasesPerPhysician_SHAP_value)) +
  geom_point(size = 1, alpha = 0.7) +
  scale_color_gradientn(name = "SHAP Value", colours = c("blue", "lightgrey", "red")) +
  #scale_x_continuous(breaks = seq(min_value,max_value, by = 20)) + 
  scale_x_continuous(limits = c(min_value,max_value)) +
  labs(x = "Number Of Cases Per Physician") +
  theme_minimal() +
  theme(axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank())

p_a_leg <- p_a+theme(legend.position="right", legend.direction = "vertical")
get_legend<-function(p_a){
  tmp <- ggplot_gtable(ggplot_build(p_a))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}
leg <- get_legend(p_a_leg)

#one legend
ggarrange(p_a,p_b,p_c, 
          labels = c("A", "B", "C"),
          ncol = 1, nrow = 3, legend = "right", legend.grob = leg,#"bottom"
          label.y = 1.05) + theme(plot.margin = margin(t = 0.5, unit = "cm"))
##all legends
ggarrange(p_a,p_b,p_c, 
          labels = c("A", "B", "C"),
          ncol = 1, nrow = 3, legend = "right", legend.grob = ,#"bottom"
          label.y = 1.05) + theme(plot.margin = margin(t = 0.5, unit = "cm"))

#####Unified SHAP Exploration plot
#scale
df_join$z1_Age <- scale(df_join$Age)[,1]
df_join$z1_NumberOfCasesPerPhysician <- scale(df_join$NumberOfCasesPerPhysician)[,1]
df_join$z1_NumberEDVisitsInTheLastHalfYear <- scale(df_join$NumberEDVisitsInTheLastHalfYear)[,1]
df_join$s1_Age_SHAP_Value <- df_join$Age_SHAP_Value
df_join$s1_NumberOfCasesPerPhysician_SHAP_value <- df_join$NumberOfCasesPerPhysician_SHAP_value
df_join$s1_NumberEDVisitsInTheLastHalfYear_SHAP_value <- df_join$NumberEDVisitsInTheLastHalfYear_SHAP_value


df_wide <- subset(df_join, select = c("X", "z1_Age", "z1_NumberOfCasesPerPhysician", "z1_NumberEDVisitsInTheLastHalfYear",
                                      "s1_Age_SHAP_Value", "s1_NumberOfCasesPerPhysician_SHAP_value", "s1_NumberEDVisitsInTheLastHalfYear_SHAP_value"))

df_long1 <- gather(df_wide, key = "Variable", value = "Value", starts_with("z1"))
df_long2 <- gather(df_wide, key = "Variable", value = "Value", starts_with("s1"))

long <- data.frame(Feature_name=df_long1$Variable,Feature_value=df_long1$Value,Shap_value=df_long2$Value)

long <- long %>% mutate(Feature_name = recode(Feature_name, "z1_NumberEDVisitsInTheLastHalfYear" = "NumberEDVisitsInTheLastHalfYear", 
                                              "z1_Age" = "Age", "z1_NumberOfCasesPerPhysician" = "NumberOfCasesPerPhysician"))

long$Feature_name <- factor(long$Feature_name, levels = c("NumberOfCasesPerPhysician", "Age", "NumberEDVisitsInTheLastHalfYear"))

min_value<-floor(min(long$Feature_value))
max_value<-ceiling(max(long$Feature_value))
ggplot(long, aes(x = Feature_value, y = Feature_name, color = Shap_value)) +
  geom_point(size = 4, alpha = 0.7) +
  scale_color_gradientn(name = "SHAP_Value", colours = c("blue", "lightgrey", "red")) +
  scale_x_continuous(breaks = seq(min_value,max_value, by = 0.5)) + 
  labs(x = "Feature value")+
  theme_minimal() +
  theme(axis.title.y = element_blank())

