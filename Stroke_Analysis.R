library(dplyr)
library(ggplot2)

stroke <- read.csv("stroke.csv")
str(stroke)

library(summarytools)
descr(stroke)

table(stroke$gender)

withstroke <- stroke %>% filter(stroke==1)
wostroke <-  stroke %>% filter(stroke==0)
stat <- ifelse(stroke$stroke==1,"stroke","no stroke")
sum(stroke$stroke)

table(stroke$gender,stat)

hist(stroke$age)

mean(stroke$age)

withstroke %>% ggplot(aes(age, fill=gender)) + geom_density(alpha=0.2) + ggtitle("Stroke by Age in Male and Female")

mean(withstroke$age)

stat <- ifelse(stroke$stroke==1,"stroke","no stroke")
highblood <- ifelse(stroke$hypertension==1,"with hypertension","without hypertension")
table(highblood, stat)

stat <- ifelse(stroke$stroke==1,"stroke","no stroke")
heart.disease <- ifelse(stroke$heart_disease==1,"with heart disease","without heart disease")
table(heart.disease, stat)
sum(stroke$hypertension==1 & stroke$heart_disease==1)

sum(stroke$hypertension==1 & stroke$heart_disease==1 & stroke$stroke==1)

hist(stroke$avg_glucose_level)
mean(stroke$avg_glucose_level)
withstroke %>% ggplot(aes(avg_glucose_level, fill=gender)) + geom_density(alpha=0.2) + ggtitle("Stroke and Glucose Level by Gender")

withstroke %>% ggplot(aes(age, avg_glucose_level, color=gender)) + geom_point() + ggtitle("Stroke and Glucose Level over Time")

bmi_ <- ifelse(stroke$bmi=="N/A",0,stroke$bmi)
bmi_ <- as.numeric(bmi_)
hist(bmi_)

bmi_withstroke <- ifelse(withstroke$bmi=="N/A",0,withstroke$bmi)
bmi_withstroke <- as.numeric(bmi_withstroke)
hist(bmi_withstroke)

stroke %>% ggplot(aes(ever_married)) + geom_bar(width=0.2) + ggtitle("Marriage status All Participants")

ever.married <- stroke$ever_married
table(ever.married,stat)

stroke %>% ggplot(aes(smoking_status)) + geom_bar(width=0.2) + ggtitle("Smoking All Participants")
smoke <- stroke$smoking_status
table( smoke, stat)

qqnorm(stroke$avg_glucose_level) # draw qq plot
qqline(stroke$avg_glucose_level)

sh_test1 <- shapiro.test(stroke$avg_glucose_level[0:5000])
sh_test1

res1 <-cor.test(stroke$avg_glucose_level, stroke$stroke,  method = "spearman")
res1

stroke$bmi <- as.numeric(stroke$bmi)
stroke$bmi <- gsub("N/A",mean(stroke$bmi),stroke$bmi)
qqnorm(as.numeric(stroke$bmi)) # draw qq plot
qqline(as.numeric(stroke$bmi))

sh_test1 <- shapiro.test(stroke$bmi[0:5000])
sh_test1

res1 <-cor.test(stroke$bmi, stroke$stroke,  method = "spearman")
res1

qqnorm(stroke$age)
qqline(stroke$age)

sh_test1 <- shapiro.test(stroke$age[0:5000])
sh_test1

res1 <-cor.test(stroke$age, stroke$stroke,  method = "spearman")
res1

res1 <-cor.test(stroke$avg_glucose_level, stroke$heart_disease,  method = "spearman")
res1

qqnorm(stroke$hypertension)
qqline(stroke$hypertension)

sh_test1 <- shapiro.test(stroke$hypertension[0:5000])
sh_test1

res1 <-cor.test(stroke$hypertension, stroke$stroke,  method = "spearman")
res1























