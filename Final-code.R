library(tidyverse)
library(corrplot)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(ggpubr)
library(ggExtra)
library(mdsr)
library(dbplyr)
library(forcats)
library(tidyr)
options(scipen=999)
library(readxl)
library(openxlsx)


############################ Datasets #############################
recent.grads.students <- read.csv("recent-grads-2012.csv",header=TRUE)
grads.students <- read.csv("grad-students-2012.csv",header=TRUE)
women_stem_data <- read.csv("women-stem-2012.csv", header=TRUE)
data <- read.csv("earnings-2013-2019.csv")
#column <- 
data[,c(5:12)]<-  sapply(data[,c(5:12)], as.numeric)
data <- data[,-1]

############################ Resources ############################
#During our process of creating the data visualizations, we have looked for and  
#studied some tutorials and coding resources online for taking references as below:
#source: https://github.com/rfordatascience/tidytuesday/blob/master/data/2019/2019-03-05/jobs_gender.csv

######################### Initial data exploration ##########################
#some of data (early) cleaning steps:

#for question 1 
#find top 10 most common/popular majors for graduate education
number.of.students <- grads.students[with(grads.students,order(-Grad_total)),]
number.of.students <- number.of.students[1:10,]
#create new variables to categorize graduate and undergraduate
higher.degrees <- select(number.of.students,Major,Grad_median)
bachelor.degrees <- subset(recent.grads.students,recent.grads.students$Major %in% higher.degrees$Major)
bachelor.degrees <- select(bachelor.degrees, Major, Median)
higher.degrees <- higher.degrees[with(higher.degrees,order(Major)),]
bachelor.degrees <- bachelor.degrees[with(bachelor.degrees,order(Major)),]
higher.degrees <- cbind(higher.degrees,bachelor.degrees$Median) 
colnames(higher.degrees) <- c("Major", "Graduate", "Undergraduate")
higher.degrees <- higher.degrees %>% gather(Degrees, Salary,  Undergraduate,Graduate)

#for question 2
#create two subsets
stem <- recent.grads.students %>% filter(recent.grads.students$Major_category %in% women_stem_data$Major_category)
non.stem <- recent.grads.students %>% filter(!(recent.grads.students$Major_category %in% women_stem_data$Major_category))
#create new variables to categorize STEM vs non STEM
recent.grads.students$stem.major = ifelse(recent.grads.students$stem.major %in% stem$Major_category,"STEM","Non STEM")
mean(stem$Median)
stem.med.salary <- select(stem, Median ,Major_category ) 
mean(non.stem$Median)
nonstem.med.salary <- select(non.stem, Median ,Major_category )
stem_s <- mean(stem$Median)
non_stem_s <- mean(non.stem$Median)
salary = c(stem_s, non_stem_s)
field <- c("STEM","Non STEM")
employment_stem <- mean(stem$Unemployment_rate)
employment_non_stem<- mean(non.stem$Unemployment_rate)
employment <- c(employment_stem,employment_non_stem)
salary_STEM <- data.frame(field,salary,employment)
# salary_STEM <- salary_STEM %>% gather(variable,value,salary,employment)
# 
# ggplot(data = salary_STEM, aes(field,value)) + 
#   geom_col(aes(fill = factor(variable)),position='dodge')

med.salary <- rbind(stem.med.salary,nonstem.med.salary)
med.salary <- med.salary %>% 
  mutate(type = as.factor(ifelse(Major_category %in% stem.med.salary$Major_category, "Stem","Non-Stem")))

#for question 3
#make stem/non-stem classification
stemdata=within(data,{
  IS_STEM='NON-STEM'
  IS_STEM[major %in% c("Natural Resources, Construction, and Maintenance","Management, Business, and Financial","Education, Legal, Community Service, Arts, and Media","Service","Sales and Office","Production, Transportation, and Material Moving")]='NON-STEM'
  IS_STEM[major %in% c("Healthcare Practitioners and Technical")]='STEM'
  IS_STEM[major %in% c("Computer, Engineering, and Science")]='STEM'
  IS_STEM[occupation %in% c("Sales engineers")]='STEM'
})


earning_2013 <- subset(stemdata, data$year == "2013")
colnames(earning_2013) <- c("Year","Occupation","Major",
                            "Total_workers","Men","Women","Percent_women",
                            "Earning","Men_earning","Women_earning",
                            "Compare_to_men","STEM")

#calculate gender ratio
library(dplyr)
earning_2013 <-  earning_2013 %>% arrange(desc(Earning))
earning_2013_gender <- earning_2013 %>% gather(Gender, Earning_both, Men_earning,Women_earning)
earning_2013_gender$Percent_men = (earning_2013_gender$Men/earning_2013_gender$Total_workers)*100

#Men earning in top 20 works:
earning_2013_topcareer <- earning_2013[1:10,]
earning_2013_topcareer <- earning_2013_topcareer %>% gather(Gender, Earning_both, Men_earning,Women_earning)
earning_2013_topcareer$X <- ifelse(earning_2013_topcareer$Gender == "Women_earning",1,0)

#Men earning in middle 20 works: 
earning_2013_middlecareer <- earning_2013[251:260,]
earning_2013_middlecareer <- earning_2013_middlecareer %>% gather(Gender, Earning_both, Men_earning,Women_earning)
earning_2013_middlecareer$X <- ifelse(earning_2013_middlecareer$Gender == "Women_earning",1,0)

#Men earning in low 20 works:
earning_2013_lowcareer <- earning_2013[513:522,]
earning_2013_lowcareer <- earning_2013_lowcareer %>% gather(Gender, Earning_both, Men_earning,Women_earning)
earning_2013_lowcareer$X <- ifelse(earning_2013_lowcareer$Gender == "Women_earning",1,0)

#over-year function 
process <- function(file,year) {
  
  colnames(file) <- c("Year","Occupation","Major",
                      "Total_workers","Men","Women","Percent_women",
                      "Earning","Men_earning","Women_earning",
                      "Compare_to_men")
  earning <- subset(file,file$Year == year)
  earning_year <- earning %>% arrange(desc(Earning))
  earning_top<- earning_year[1:10,]
  earning_top$Difference <- earning_top$Men_earning - earning_top$Women_earning
  
  earning_avg <- earning_year[251:260,]
  earning_avg$Difference <- earning_avg$Men_earning - earning_avg$Women_earning
  
  earning_low <- earning_year[513:522,]
  earning_low$Difference <-  earning_low$Men_earning -  earning_low$Women_earning
  return(list(earning_year, earning_top$Difference,
              earning_avg$Difference,
              earning_low$Difference))
}
earning_2013 <- process(data,2013)
diff_2013 <- cbind("2013",mean(na.omit(earning_2013[[2]])),mean(na.omit(earning_2013[[3]])),mean(na.omit(earning_2013[[4]])))

earning_2014 <- process(data,2014)
diff_2014 <- cbind("2014",mean(na.omit(earning_2014[[2]])),mean(na.omit(earning_2014[[3]])),mean(na.omit(earning_2014[[4]])))

earning_2015 <- process(data,2015)
diff_2015 <- cbind("2015",mean(na.omit(earning_2015[[2]])),mean(na.omit(earning_2015[[3]])),mean(na.omit(earning_2015[[4]])))

earning_2016 <- process(data,2016)
diff_2016 <- cbind("2016",mean(na.omit(earning_2016[[2]])),mean(na.omit(earning_2016[[3]])),mean(na.omit(earning_2016[[4]])))

earning_2017 <- process(data,2017)
diff_2017 <- cbind("2017",mean(na.omit(earning_2017[[2]])),mean(na.omit(earning_2017[[3]])),mean(na.omit(earning_2017[[4]])))

earning_2018 <- process(data,2018)
diff_2018 <- cbind("2018",mean(na.omit(earning_2018[[2]])),mean(na.omit(earning_2018[[3]])),mean(na.omit(earning_2018[[4]])))

earning_2019 <- process(data,2019)
diff_2019 <- cbind("2019",mean(na.omit(earning_2019[[2]])),mean(na.omit(earning_2019[[3]])),mean(na.omit(earning_2019[[4]])))


diff <- as.data.frame(rbind(diff_2013,diff_2014,diff_2015,diff_2016,diff_2017,diff_2018,diff_2019))
colnames(diff) <- c("Year","Top_10","Middle_10", "Bottom_10")
diff <- diff %>% gather(Group, Diff_in_mean, Top_10,Middle_10,Bottom_10)
diff$Group <- as.factor(diff$Group)
diff$Diff_in_mean <- as.numeric(diff$Diff_in_mean)
diff$Year <- as.factor(diff$Year)
levels(diff$Group) <- c("Bottom 10","Middle 10", "Top 10")

#################### Research Question 1 ##########################
#1.What are the highest-earning majors?
figure1 <- recent.grads.students %>%
  arrange(desc(Median))%>% #re-arrage the median column in descending order to identify highest to lowest earnings
  select(Major, Major_category, P25th, P75th, Median)%>% #grab some necessary variables
  mutate(Major = fct_reorder(Major, Median))%>% head(10) %>% #re-order and select first 10 majors
  ggplot(aes(Major, Median, color = Major_category))+
  scale_color_brewer(palette = "Set1")+ theme_bw()+
  geom_point()+geom_errorbar(aes(ymin= P25th, ymax = P75th))+coord_flip() +
  labs(subtitle="ranging from 25th to 75th percentile", y="Median Salary", x="", title="Figure 3: TOP 10 HIGHEST EARNINGS MAJORS", color="Major Categories")

figure1 

#2.What are the lowest-earning majors?
figure2 <- recent.grads.students %>%
  arrange(desc(Median))%>%  #re-arrage the median column in descending order to identify highest to lowest earnings
  select(Major, Major_category, Median, P25th, P75th)%>%  #grab some necessary variables
  mutate(Major = fct_reorder(Major, Median))%>% tail(10) %>% #re-order and select last 10 majors
  ggplot(aes(Major, Median, color = Major_category))+
  scale_color_brewer(palette = "Paired")+ theme_bw()+
  geom_point()+ geom_errorbar(aes(ymin= P25th, ymax = P75th))+ coord_flip() + 
  labs(subtitle="ranging from 25th to 75th percentile", y="Median Salary", x="", title="Figure 2: TOP 10 LOWEST EARNINGS MAJORS",color="Major Categories")
figure2

#3.How the earnings vary overtime (undergrad - grad) across top 10 popular majors of graduate education?
figure3 <- ggplot(higher.degrees, aes(x=Major, y= Salary, fill= Degrees))+
  geom_col(position = "dodge")+
  labs(subtitle="across top 10 popular graduate education", y="Median Salary", x="", title="Figure 3: GRADUATE AND UNGRADUATE MEDIAN SALARY",color="Degrees")+
  scale_fill_brewer(palette = "Dark2")+coord_flip()
figure3 

#################### Research Question 2 ########################
#1.Is STEM majors'median salary higher than non-STEM majors?
figure4 <- ggplot(data = salary_STEM, aes(x = field, y= salary)) +
  geom_bar(aes(fill=field),stat = "identity", width = 0.4)+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_brewer(palette="Set2")+
  labs(x= "Field", y = "Unemployment Rate")+
  labs(title = "Figure 9: UNEMPLOYMENT RATE BETWEEN STEM AND NON-STEM",fill="Field",
       subtitle= "in 2012")+
  geom_text(label = round(salary,2), hjust = 0.5, vjust = 0)
figure4

#2.Is non-STEM majors' unemployment rate higher than STEM majors? 
figure5 <- ggplot(data = salary_STEM, aes(x = field,y=salary)) + 
  geom_bar(aes(fill=field),stat = "identity", width = 0.4)+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_brewer(palette="Set2")+
  labs(x= "Field", y = "Median Salary", title = "Figure 5: MEDIAN SALARY BETWEEN STEM AND NON-STEM", fill="Field") 
figure5

#################### Research Question 3 ########################
#1.How popular each major category is across both genders
genderbreakdown <- earning_2013_gender %>% gather(Sex, Number, Men,Women)
figure6 <- ggplot(genderbreakdown, aes(x=Major, y= Number, fill = Sex))+
  geom_col()+ scale_fill_brewer(palette = "Dark2")+coord_flip()+
  labs(x="", y="Number of Students", title = "Figure 2: MAJOR CATEGORIES ACROSS GENDERS", subtitle = "in year 2013")
figure6
#do men major in STEM more than women do?
STEMmajor <- filter(earning_2013_gender, STEM =="STEM")
sum(STEMmajor$Men) #13408348
sum(STEMmajor$Women) #11403340

#2. How do male/female dominated majors relate to median earnings?
a <- ggplot(earning_2013_gender, 
            aes(x = Percent_men,
                y = Earning, color=Major)) +
  geom_jitter(aes(size = Men),alpha = 0.7) + 
  #stat_cor(label.x=0.5, label.y=185000)+ stat_regline_equation(label.x=0.5, label.y=195000)+
  geom_smooth(method="lm",se=F)+
  theme(legend.position = "bottom")+
  labs(x="Male-dominated (%)",y="Earning($)",subtitle="in year 2013, sized by total of men workers", title="Figure 7: EARNINGS AND GENDER RATIO")
b <- ggplot(earning_2013_gender, 
            aes(x = Percent_women,
                y = Earning, color=Major)) +
  geom_jitter(aes(size = Women),alpha = 0.7) + 
  #stat_cor(label.x=0.5, label.y=185000)+ stat_regline_equation(label.x=0.5, label.y=195000)+
  geom_smooth(method="lm",se=F)+
  theme(legend.position = "bottom")+
  labs(x="Female-dominated (%)",y="Earning($)",subtitle="in year 2013, sized by total of women workers")
figure7 <- ggarrange(a,b,ncol = 1, nrow = 2)
figure7

############################ Regression ############################
#STEM EFFECT: Do men earn more because STEM majors earn more?
earning_2013 <- subset(stemdata, data$year == "2013")
colnames(earning_2013) <- c("Year","Occupation","Major",
                            "Total_workers","Men","Women","Percent_women",
                            "Earning","Men_earning","Women_earning",
                            "Compare_to_men","STEM")
earning_2013_sub <-  earning_2013 %>% arrange(desc(Earning)) %>% filter(STEM == "STEM")%>%
  gather(Gender, Earning_both, Men_earning,Women_earning)

mod4 <- lm(Earning_both ~Gender,earning_2013_sub)
summary(mod4)

model2 = lm(Earning ~ STEM, data = earning_2013)
summary(model2)
anova(model2)

#Graph
#Get the mean values from the data frame and store them in vector
mean <- earning_2013 %>%  
  group_by(Major) %>% 
  summarize(average = mean(Earning)) %>% 
  ungroup() 
figure8 <- ggplot(data = earning_2013, aes(Major,Earning,color= STEM))+ geom_jitter(alpha = 0.5, width=.2) + 
  geom_boxplot(notch = FALSE)+coord_flip()+scale_color_brewer(palette = "Set2")+
  geom_line(data = mean, mapping = aes(x = Major, y = average, group=2),color="black")+
  labs(x="", y="Total Earnings", title = "Figure 11: EARNINGS BETWEEN STEM AND NON-STEM", subtitle = "by major category, in 2013",color="Field")
figure8

#GENDER EFFECT: Do men earn more because men earn more? 
model1a = lm(Earning_both ~ Gender, data = earning_2013_topcareer,subset = Gender %in% c("Men_earning","Women_earning"))
summary(model1a)
plot1a <- ggplot(earning_2013_topcareer, aes(x=Occupation,y=Earning_both,color= Gender ))+
  geom_point()+geom_line(aes(group=Occupation),color = "grey")+ coord_flip()

model1b = lm(Earning_both ~ Gender, data = earning_2013_middlecareer,subset = Gender %in% c("Men_earning","Women_earning"))
summary(model1b)
plot1b <- ggplot(earning_2013_middlecareer, aes(x=Occupation,y=Earning_both,color= Gender ))+
  geom_point()+geom_line()+ coord_flip()

model1c = lm(Earning_both ~ Gender, data = earning_2013_lowcareer,subset = Gender %in% c("Men_earning","Women_earning"))
summary(model1c)
plot1c <- ggplot(earning_2013_lowcareer, aes(x=Occupation,y=Earning_both,color= Gender ))+
  geom_point()+geom_line()+ coord_flip()

#Graph
earning_2013 <- subset(data, data$year == "2013")
colnames(earning_2013) <- c("Year","Occupation","Major",
                            "Total_workers","Men","Women","Percent_women",
                            "Earning","Men_earning","Women_earning",
                            "Compare_to_men")
earning_2013 <-  earning_2013 %>% arrange(desc(Earning))

earning_2013_top <- earning_2013[1:4,]
earning_2013_top$Group <- c("Top","Top","Top","Top")
earning_2013_middle <- earning_2013[253:256,]
earning_2013_middle$Group <- c("Middle","Middle","Middle","Middle")
earning_2013_low <- earning_2013[519:522,]
earning_2013_low$Group <- c("Bottom","Bottom","Bottom","Bottom")

graph <- rbind(earning_2013_top,earning_2013_middle,earning_2013_low) 
graph_gender <- graph %>% gather(Gender, Earning_both, Men_earning,Women_earning)
graph_gender$Gender <- as.factor(graph_gender$Gender)
levels(graph_gender$Gender) <- c("Men","Women")

figure9 <- ggplot(graph_gender,aes(x = fct_reorder(Occupation,Earning,.desc = TRUE), y = Earning_both, color = Gender))+
  geom_rect(aes(xmin = -Inf, xmax = Occupation, ymin = -Inf, ymax = Inf, fill = factor(Group)),
            alpha = 0.5) +
  geom_point(size = 2.5) +
  geom_line(aes(group =Occupation),color = "black")+
  theme_classic()+coord_flip()+facet_wrap(~Group)+
  scale_color_manual(values=c('black','brown'))+
  scale_fill_brewer()+theme(legend.position="bottom")+
  labs(y="Average earning ($)",x= "Occupation", fill = "Group",
       title = "Figure 7: EARNING DIFFERENCE AMONGST EARNING GROUP ACROSS GENDERS",
       subtitle = "in 2013")

figure9

# Earning differences over years
par(mfrow=c(1, 1))
figure10 <- ggplot(diff, aes(x=as.factor(Year), y= Diff_in_mean, fill = Group))+
  geom_bar(stat = "identity", position = "dodge")+
  geom_line(size = 1.5, color="black", group = 0 )+
  scale_fill_brewer(palette = "YlOrRd")+
  facet_wrap(~Group)+
  labs(y="Earning difference ($)",x= "Year", fill = "Group",
       title = "Figure 8: EARNING DIFFERENCE AMONGST EARNING GROUP FROM 2013-2016")+
  theme_classic()+theme(axis.text.x = element_text(size = 13),axis.title = element_text(size = 15))
figure10

#STEM and GENDER EFFECT
earning_2013_gender<-earning_2013_gender[complete.cases(earning_2013_gender$Earning_both),]

model3 <- lm(Earning_both ~ Gender + STEM, data = earning_2013_gender)
summary(model3)

#STEM and GENDER INTERACTION EFFECT
model4 <- lm(Earning_both ~ Gender + STEM + Gender*STEM, data = earning_2013_gender)
summary(model4)

#Their interaction doesnt change the R2 value => the added variable
#doesnt play a sig role in explaining the salary differences. 
#It makes sense because each contributes sig to the y value already.




































