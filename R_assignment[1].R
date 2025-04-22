# PP2421
# Data set source- https://ndap.niti.gov.in/dataset/7225 (National Data & Analytics Platform)

# Load necessar libraries
library(rvest)
library(httr)
library(xml2)
library(tidyverse)
library(dplyr)
library(naniar)
library(gtExtras)
library(ggplot2)
library(here)
library(skimr)
library(janitor)
library(dplyr)
library(mice)
library(ggplot2)
library(car)
library(gganimate)
library(viridis)
library(RColorBrewer)
library(plotly)
library(GGally)
library(ggrepel)
library(factoextra)
library(cluster)
data_1<-NDAP_REPORT_7225
View(data_1)

## Exploring the data
str(data_1)
skim_without_charts(data_1)
glimpse(data_1)
data_2<-data_1 %>%
  rename(Amount_invested_cr= 'Amount invested for building houses under pradhan mantri awas yojana',
         Amount_sanctioned_cr = 'Amount sanctioned for building houses under pradhan mantri awas yojana',
         Amount_released_cr = 'Amount released for building houses under pradhan mantri awas yojana',
         Sacntioned_house = 'Houses sanctioned under pradhan mantri awas yojana mission',
         Grounded_house = 'Houses grounded under pradhan mantri awas yojana mission',
         Completed_house  = 'Houses completed construction under pradhan mantri awas yojana mission') %>%
  select(-Country,-'State lgd code')
View(data_2)
##missing values
miss_var_summary(data_2) %>%
  gt()%>%
  gt_theme_excel()%>%
  tab_header(title = "Missing Values")
gg_miss_var(data_2)
md.pattern(data_2,rotate.names=T)
# There are Missing values in Amount invested,Amount sanctioned & Amount released. We will check what will be the suitable method to replace the NA/Missing Values
# For that we will check the outliers by QQ plot
qqPlot(data_2$Amount_released_cr, main = "Q-Q Plot of Amount Released")
qqPlot(data_2$Amount_sanctioned_cr, main = "Q-Q Plot of Amount Sanctioned")
qqPlot(data_2$Amount_invested_cr, main = "Q-Q Plot of Amount Invested")
qqPlot(data_2$Completed_house)
# With the help of QQ-plot we concluded that the graph is positive skewed. So Median will be the suitable value to replace 
# Imputation
data_2$Amount_invested_cr[is.na(data_2$Amount_invested_cr)] <- median(data_2$Amount_invested_cr, na.rm = TRUE)
data_2$Amount_sanctioned_cr[is.na(data_2$Amount_sanctioned_cr)] <- median(data_2$Amount_sanctioned_cr, na.rm = TRUE)
data_2$Amount_released_cr[is.na(data_2$Amount_released_cr)] <- median(data_2$Amount_released_cr, na.rm = TRUE)
colSums(is.na(data_2))
duplicated(data_2,incomparables = F)

# Describing and Summarizing
summary(data_2)
colSums(is.na(data_2))
as<-data_2 %>%
  group_by(State) %>%
  summarise(Lower=min(Completed_house),
            Average=mean(Completed_house),
            Upper=max(Completed_house),
            )
  View(as)
table(data_2$State)
table(data_2$City)
table(data_2$State,data_2$City)

# Merging data set
merge_data<-full_join(data_2,party_data,by="State")
merge_data
View(merge_data)
colSums(is.na(merge_data))
md.pattern(merge_data,rotate.names = T)
# It will showing NA values because State include UT also & UTs have ruled by legislature 
Phaltan<-merge_data %>%
  filter(City=="Phaltan") %>%
  drop_na()
View(Phaltan)
# We are focusing only on State data ignore all UTs

#Hypothesis-1
# "The amount released is significantly lower than the amount sanctioned, indicating inefficiencies or delays in fund disbursement processes"

ggpairs(merge_data[,c("Amount_invested_cr","Amount_sanctioned_cr","Amount_released_cr")])
# We have applied 'paired T-Test' because we are comparing two numerical variables (continuous)
t.test(merge_data$Amount_released_cr, merge_data$Amount_sanctioned_cr,
       paired = TRUE, alternative = "less")
#“There is a statistically significant gap between the funds sanctioned and the funds actually released under the scheme, with the released amounts being consistently and significantly lower.”
#(-Inf, -13.82) means that with 95% confidence, the true average difference between sanctioned and released is at least ₹13.82 crores, favoring sanctioned > released

# Hypothesis-2
# "States that receive higher amounts of funds released tend to complete more houses under the scheme"
set.seed(456)
ggplot(merge_data, aes(x = Amount_released_cr, y = Completed_house, label = State)) +
  geom_point(color = "darkred", shape = 18, size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "goldenrod", linewidth = 1) +
  geom_text_repel(size = 3, max.overlaps = 100) +
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal() +
  labs(
    title = "Funds Released vs Houses Completed",
    subtitle = "States that receive higher funds tend to complete more houses",
    x = "Funds Released",
    y = "Houses Completed",
    caption = "Text labels represent State names"
  )


#Spearmans rank correlation

cor.test(merge_data$Amount_released_cr,merge_data$Completed_house,method = "spearman",exact = F)
#Spearman's rank correlation test shows a strong positive and statistically significant association between funds released and houses completed across states (ρ = 0.90, p < 0.001). This suggests that states receiving higher funding under the scheme tend to complete more houses, even when the data is non-normally distributed.
# A rho value of 0.90 suggests that, as funds released increase, the number of houses completed also tends to increase, and this relationship is very consistent across states
# States that receive higher amounts of funds released tend to complete more houses under the scheme.


# animation bar graph indicating the houses completed in states and showing state wise political party

plot<-merge_data %>% 
  drop_na() %>%
  ggplot(aes(x=State,
             y=Completed_house,
             colour = party))+
  geom_line()+
  theme_classic()+
  scale_color_viridis(discrete = T)+
  labs(title="Graph",
       x="State",
       y="Completed_House",
       color="Party")+
  theme(plot.title=element_text(size=3,
                                color = "lightpink"),
        axis.text.x=element_text(angle=90,vjust=0.2,hjust=0.8))+
  transition_reveal(Completed_house)
animate(plot, renderer = gifski_renderer())


# Hypothesis-3
# Finding Political influence w.r.t Houses Completed

merge_data$party<-as.factor(merge_data$party)
merge_data$State<-as.factor(merge_data$State)
merge_data$Compleded_house<-scale(merge_data$Completed_house)
sum(is.na(merge_data$Completed_house))
sum(is.na(merge_data$Amount_released_cr))
md.pattern(merge_data,rotate.names=T) # It will showing NA values because State include UT also & UTs have ruled by legislature 
names(merge_data)
clean_data <- merge_data %>% drop_na(Completed_house, Amount_released_cr)

fviz_nbclust(clean_data[, c( "Completed_house","Amount_released_cr")], kmeans, method = "wss")
# Variables scaled
clean_data$Completed_house <- scale(clean_data$Completed_house)
clean_data$Amount_released_cr <- scale(clean_data$Amount_released_cr)
# K-means cluster
set.seed(42)
kmeans_result <- kmeans(merge_data[, c("Completed_house", "Amount_released_cr")], centers = 3, nstart = 25)
# Adding cluster 
clean_data$Cluster <- as.factor(kmeans_result$cluster)
# Visualize
set.seed(123)
fviz_cluster(kmeans_result, 
             data = clean_data[, c("Completed_house", "Amount_released_cr")], 
             geom = "point",
             ellipse.type = "convex",
             palette = "jco",
             ggtheme = theme_minimal())
# Interpretation of Visulization
# Cluster 1 (Blue Circles)- These are low perfomers—states or parties that received low funding and low complete in  houses
#  Cluster 2 (Yellow Triangles)- Possibly includes regions with Higher  success or best allocation of funds and completion of houses
# Cluster 3 (Gray Squares)- This cluster likely includes moderate-performing regions in both funding and completion of houses
final_summary <- clean_data %>%
  group_by(Cluster) %>%
  summarise(
    States = paste(unique(State), collapse = ", "),
    Parties = paste(unique(party), collapse = ", "),
    .groups = 'drop'
  )

View(final_summary)
table(final_summary)
View(table(final_summary))


#"The clustering and visual analyses reveal that political party affiliation alone does not  significantly influence the number of houses completed under PMAY. 
# It shows mixed results Even though Cluster 2, contains some political influence(Gujarat) with higher funds and completed houses, other elements  suggest regions with significant development initiatives, efficacy in policy implementation. 
# In contrast, Clusters 1 and 3 highlight areas with moderate or limited progress, pointing to potential gaps in resource allocation."

