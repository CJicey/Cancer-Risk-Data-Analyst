#Summary for Age, Calories, and Fat.
summary(cr$Age)
summary(cr$Calories)
summary(cr$Fat)

#Box plots and histograms for Age, Calories, and Fat.
hist(cr$Age,main="Figure 1: Histogram and Boxplot for Age",
     col="seagreen2",xlab="Age",labels = T,ylab = "",
     axes = T, ylim=c(-100,100),border="black")
boxplot(cr$Age,horizontal = T,xlab = "Age",
        col = "seagreen2",add = T,at = -50,boxwex = 175)

hist(cr$Calories,main="Figure 2: Histogram and Boxplot for Calories",
     col="blue1",xlab="Calories",labels = T,ylab = "",
     axes = T, ylim=c(-100,200),border="black")
boxplot(cr$Calories,horizontal = T,xlab = "Calories",
        col = "blue1",add = T,at = -50,boxwex = 175)

hist(cr$Fat,main="Figure 3: Histogram and Boxplot for Fat",
     col="purple1",xlab="Fat",labels = T,ylab = "",
     axes = T, ylim=c(-100,200),border="black")
boxplot(cr$Fat,horizontal = T,xlab = "Fat",
        col = "purple1",add = T,at = -50,boxwex = 175)

#agecat variable has been created
cr$agecat <- ifelse(cr$Age <= 35, "Young age", 
             ifelse(cr$Age <= 45, "Middle age", 
             ifelse(cr$Age <= 55, "Old age", "Elder")))

#frequency table 
table(cr$agecat)

#pie chart
frequency <- as.data.frame(table(cr$agecat))
names(frequency) <- c("AGE", "COUNT")

ggplot(frequency, aes(x = "", y = COUNT, fill = factor(AGE))) +
  geom_bar(stat = "identity") +
  ggtitle("Pie Chart for Age") +
  geom_text(aes(label = COUNT), position = position_stack(vjust = 0.5), size = 4) +
  theme_classic() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  labs(fill = "Age", x = NULL, y = NULL) +
  coord_polar("y") +
  scale_fill_manual(values = c("seagreen", "blue1", "purple1","yellow1"))

#ordered bar chart
agecat_table <- table(ordered(cr$agecat, 
                levels = c("Young age","Middle age","Old age", "Elder")))

barplot(agecat_table, main = "Ordered Barchart for Age", 
                      xlab = "Ages",
                      col = c("yellow1","blue1","purple1","seagreen"))

#all contingency tables
cr$Gender <- factor(cr$Gender, 
                    levels = c(1, 2), 
                    labels = c("Male", "Female"))

cr$SmokeStat <- factor(cr$SmokeStat, 
                    levels = c(1, 2, 3), 
                    labels = c("Never", "Former", "Current"))

table(cr$Gender,cr$SmokeStat)

round(prop.table(table(cr$Gender,cr$SmokeStat)),2)
round(prop.table(table(cr$Gender,cr$SmokeStat),1),2)
round(prop.table(table(cr$Gender,cr$SmokeStat),2),2)

#100% stacked bar chart
cr$SmokeStat <- factor(cr$SmokeStat, 
                       levels = c(1, 2, 3), 
                       labels = c("Never", "Former", "Current"))

props <- round(prop.table(table(cr$SmokeStat, cr$Gender),1),digits=1)
props_df  <- as.data.frame(props) 
names(props_df) <- c("SmokeStat","Gender","Prop") 

ggplot(data = props_df, aes(x = SmokeStat, y = Prop, 
                            fill = Gender, label = Prop)) +
  geom_bar(stat = "identity", show.legend = TRUE) +
  geom_text(size = 4.5, position = position_stack(vjust = 0.25))  +
  theme_stata() + 
  theme(legend.position = "bottom") + 
  scale_fill_manual(values = c("lightblue","seagreen2")) +
  ggtitle("Proportion of Age Group by Smoking")

#mean for Fat in the variable agecat
mean_fat <- aggregate(Fat ~ agecat, data = cr, FUN = mean)
mean_fat$Fat <- round(mean_fat$Fat, 1)
mean_fat

#observations
set.seed(7777777)
nrow(cr)

cr_samp <- cr[sample(1:nrow(cr),85,replace=F),]
View(cr_samp)

CI <- function(x, alpha = .05, dec = 3){ 
  n <- sum(!is.na(x))
  conf_level <- (1-alpha)*100
  me <- qt(1-alpha/2, n-1)*sd(x, na.rm=T)/sqrt(n)
  lower <- round(mean(x, na.rm = T) - me, digits = dec)
  upper <- round(mean(x, na.rm = T) + me, digits = dec)
  mean <- round(mean(x,na.rm = T), digits = dec)
  {limits <-data.frame(cbind(variable = 
                       deparse(substitute(x)), n, c.level = conf_level, mean, 
                       me = round(me, digits = dec), lower, upper))}
  print(limits) 
  rm(n, conf_level, lower, upper, mean)}

CI(cr_samp$Calories,dec=2)
#I am 95% confident that the mean charges for the population is between: 
#$1,677.20 and $1,987.71


#Five analytical tools for question 9
#New variable for calories
cr$calcat <- ifelse(cr$Calories <= 1000, "Low intake", 
             ifelse(cr$Calories <= 2000, "Medium intake", 
             ifelse(cr$Calories <= 3000, "High intake", "Extreme intake")))

#scatter plot
plot(x = cr$Calories,y = cr$Age, xlab = "Calories" , ylab = "Age")
abline(lm(cr$Age~cr$Calories))

#100% stacked bar chart
props <- round(prop.table(table(cr$calcat, cr$agecat),1),digits=1)
props_df  <- as.data.frame(props) 
names(props_df) <- c("Calories","Age","Prop") 

ggplot(data = props_df, aes(x = Age, y = Prop, 
                            fill = Calories, label = Prop)) +
  geom_bar(stat = "identity", show.legend = TRUE) +
  geom_text(size = 4.5, position = position_stack(vjust = 0.25))  +
  theme_stata() + 
  theme(legend.position = "bottom") + 
  scale_fill_manual(values = c("lightblue","seagreen2","purple1","yellow1")) +
  ggtitle("Figure 2: Proportion of Calories by Age")

#contingency table

table(cr$agecat,cr$calcat)

#stratified analysis

aggregate(cr$Calories~cr$agecat, FUN=mean)

#side-by-side box plot
boxplot(cr$Calories~cr$agecat, horizontal = T,
        col = c("purple1", "red1", "seagreen1", "blue1"), 
        xlab = "Calories", 
        ylab = "Age",
        main = "Figure 5: Boxplots")


