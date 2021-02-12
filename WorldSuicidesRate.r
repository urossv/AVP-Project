df <- read.csv(".\\master.csv")
head(df)

#Napraviću nov dataframe koji će predstavljati sređen prethodni dataframe
reworked.df <- df

#Potrebno je preimenovati kolone
names(reworked.df)[1] <- "country"

#Prvo obrisati suvišne tačke na krajevima naziva kolona
dots.delete <- function (name){
    return(sub("\\.{2,}","", name))
}

names(reworked.df) <- sapply(names(reworked.df), dots.delete)

#Sad je potrebno promeniti "_" u "." da bi imali iste notacije u kolonama
column.notation <- function (column){
    return(gsub("_", ".", column))
}

names(reworked.df) <- sapply(names(reworked.df), column.notation)

#Menjamo"HDI" u malo "hdi"
names(reworked.df) <- sapply(names(reworked.df), tolower)

#gdp.for.year iz string u numeric
reworked.df$gdp.for.year <- sapply(reworked.df$gdp.for.year, function(x) {gsub(",", "", x)})
reworked.df$gdp.for.year <- sapply(reworked.df$gdp.for.year, as.numeric)

#Potrebno napraviti faktore
reworked.df$sex <- as.factor(reworked.df$sex)
reworked.df$age <- factor(reworked.df$age, level=c(
"5-14 years", "15-24 years","25-34 years", "35-54 years", "55-74 years", "75+ years"), ordered = TRUE)

#unique(reworked.df$generation)

reworked.df$generation <- factor(reworked.df$generation, level=c(
"G.I. Generation", "Silent", "Boomers", "Generation X", "Millenials", "Generation Z"))

head(reworked.df)

apply(reworked.df, 2, function(x) sum(is.na(x)))

#Proverimo da li su za svaku državu obezbeđeni podaci za iste godine
aggregate(reworked.df["year"], list(country = reworked.df$country), unique)

#1. Koje države imaju najveću stopu suicida
#Za odgovor na ovo pitanje koristiću godinu sa najviše opservacija
which.max(table(reworked.df$year))

year2009 <- as.data.frame(reworked.df[reworked.df$year == 2009, ])

year2009.suicides.agg <- aggregate(year2009[c(
    "suicides.no", "population")], list(country = year2009$country), sum)

year2009.suicides.agg$suicides.100k.pop <- year2009.suicides.agg$suicides.no / year2009.suicides.agg$population

year2009.suicides.agg <- year2009.suicides.agg[
    order(year2009.suicides.agg$suicides.100k.pop, decreasing = TRUE), ]

head(year2009.suicides.agg)

#2. Koje starosne grupe su sklonije suicidu?
#Računaće se za sve države. Za razliku od prošlog primera neću računati nove vrednosti suicides.100k.pop, već ću
#sabrati postojeće. Takva sumirana vrednost nije ista kao i ona koja se dobija suicides.no/population ali smatram
#da oslikava istu pojavu

year2009.age.agg <- aggregate(year2009["suicides.100k.pop"], list(age=year2009$age), sum)

#install.packages("RColorBrewer")

library(ggplot2)
library(RColorBrewer)

ggplot(year2009.age.agg, aes(x=age, y=suicides.100k.pop, fill=age)) + 
geom_bar(stat="identity", color="black") + 
labs(x="Starosne grupe", y="Sumirana stopa suicida") + 
scale_fill_brewer(palette="Blues")

#3. Koji pol je skloniji suicidu?

year2009.sex.agg <- aggregate(year2009["suicides.100k.pop"], list(sex = year2009$sex), sum)

ggplot(year2009.sex.agg, aes(x=sex, y=suicides.100k.pop, fill=sex)) + 
geom_bar(stat="identity", color="black") + 
labs(x="Pol", y="Sumirana stopa suicida") + 
scale_fill_brewer(palette="Blues")

sex.age.agg <- aggregate(year2009["suicides.100k.pop"], list(sex = year2009$sex, age = year2009$age), sum)

ggplot(sex.age.agg, aes(x = age, y = suicides.100k.pop, fill = sex)) +
geom_bar(stat="identity", color="black", position = "dodge") + 
scale_fill_brewer(palette="Paired") +
labs(x = "Starosne grupe", y= "Summirana stopa suicida", title="Stopa suicida po starosnim grupama i polu")

#4. U kojoj generaciji je suicid zastupljeniji?
generation <- aggregate(year2009["suicides.100k.pop"], list(generation=year2009$generation), sum)
generation

#5. Od čega zavisi stopa suicida?
#Za odgovor na ovo pitanje moram koristiti i vairjablu sa HDI vrednostim.
#Početna hipoteza je da stopa zavisi od HDI-ja i GDP-ja

hdi.df <- as.data.frame(reworked.df[is.na(reworked.df$hdi.for.year) == FALSE, ])
which.max(table(hdi.df$year))

#Uzeta je gdoina 2010. zbog najvećeg broja HDI vrednosti

year2010 <- as.data.frame(hdi.df[hdi.df$year == 2010, ])
head(year2010)

year2010.agg <- aggregate(
    year2010[ , c("hdi.for.year", "gdp.for.year", "gdp.per.capita")], list(country = year2010$country), unique)

year2010.agg2 <- aggregate(year2010[, c("suicides.no", "population")], list(country = year2010$country), sum)
year2010.agg2$suicides.100k.pop <- year2010.agg2$suicides.no / year2010.agg2$population
year2010.agg[c("suicides.no", "population", "suicides.100k.pop")] <- year2010.agg2[
    , c("suicides.no", "population", "suicides.100k.pop")]

head(year2010.agg)

shapiro.test(year2010.agg$suicides.100k.pop)
shapiro.test(year2010.agg$hdi.for.year)

ggplot(year2010.agg, aes(hdi.for.year, suicides.100k.pop)) + geom_point()

cor.test(year2010.agg$hdi.for.year, year2010.agg$suicides.100k.pop, method="spearman")

shapiro.test(year2010.agg$gdp.per.capita)
cor.test(year2010.agg$gdp.per.capita, year2010.agg$suicides.100k.pop, method="spearman")

ggplot(year2010.agg, aes(gdp.per.capita, suicides.100k.pop)) + geom_point()

#Za desicion tree ću uzezi isti ovaj df jer sadrži najviše hdi vrednosti
#install.packages(c("rpart", "rpart.plot"))

library(rpart)
library(rpart.plot)

train.data <- as.data.frame(year2010.agg)
third.q <- quantile(train.data$suicides.100k.pop, 0.75)

train.data$high.rate <- ifelse(train.data$suicides.100k.pop > third.q, "Yes", "No")
train.data$high.rate <- as.factor(train.data$high.rate)
train.data[c("country", "suicides.100k.pop")] <- NULL

head(train.data)

tree1 <- rpart(high.rate ~. , train.data, method = "class")
print(tree1)
rpart.plot(tree1)

#Probaću još jedan model kojem ću proslediti ceo dataset
train.data2 <- as.data.frame(year2010)
q3 <- quantile(train.data2$suicides.100k.pop, 0.75)

train.data2$high.rate <- ifelse(train.data2$suicides.100k.pop > q3, "Yes", "No")
train.data2$high.rate <- as.factor(train.data2$high.rate)
train.data2[c("country", "year", "suicides.100k.pop", "country.year")] <- NULL

head(train.data2)

tree2 <- rpart(high.rate ~. , train.data2, method = "class")
print(tree2)
rpart.plot(tree2)

#Napraviću stablo na osnovu celog data seta a ne samo jedne godine
#install.packages(caret)

library(caret)

quant3 <- quantile(hdi.df$suicides.100k.pop, 0.75)
hdi.df$high.rate <- ifelse(hdi.df$suicides.100k.pop > quant3, "Yes", "No")
hdi.df$high.rate <- factor(hdi.df$high.rate)
hdi.df[c("country", "year", "suicides.100k.pop", "country.year")] <- NULL

head(hdi.df)

train.indices <- createDataPartition(hdi.df$high.rate, p=0.8, list=FALSE)
train.data3 <- hdi.df[train.indices, ]
test.data <- hdi.df[-train.indices, ]

tree3 <-rpart(high.rate ~. , train.data3, method = "class")
print(tree3)
rpart.plot(tree3)

#S obzirom da je stablo veliko te se plot ne vidi dobro, kod ispod pravi čitljivu sliku.
#png("FinalClassTree1.png", width=1280, height=960)
#rpart.plot(tree3)
#dev.off()


tree3.pred <- predict(tree3, test.data, type = "class")
tree3.cm <- table(true = test.data$high.rate, predicted = tree3.pred)
tree3.cm

compute.eval.metrics <- function(cmatrix) {
TP <- cmatrix[2,2]
TN <- cmatrix[1,1] 
FP <- cmatrix[1,2]
FN <- cmatrix[2,1]
acc <- sum(diag(cmatrix)) / sum(cmatrix)
precision <- TP / (TP + FP)
recall <- TP / (TP + FN)
F1 <- 2*precision*recall / (precision + recall)
c(accuracy = acc, precision = precision, recall = recall, F1 = F1)}

tree3.eval <- compute.eval.metrics(tree3.cm)
tree3.eval
