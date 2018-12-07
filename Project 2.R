### Lori Kim
######### Project 2 Part A

# set working directory
p = "/Volumes/LEXAR/DATA SCIENCE/Projects/Project 2/"
setwd(p)

# read the P02_Corporate tax.csv file
corpt = read.csv(file = "P02_Corporate tax.csv") #putting the reading of the csv file into a variable

# ypcg = GDP per capita growth rate, average from 2000 to 2015
# ctax = Corporate tax rate (%), average from 2000 to 2008
# ypc2000 = GDP per capita (US$) in 2000
# dty = Debt to GDP ratio (%), average from 2000 to 2008
# trade = Trade (imports and exports) as percentage of GDP (%)
# ihc = Index of country's human capital
# y2000 = GDP in 2000 (economy size, US billion $)

## Three regression equations (pg. 55)
# Multiple Linear Regression Model: GDP & Corporate Tax Rate
eq1 = lm(ypcg ~ ctax, data = corpt)
summary(eq1)

eq2 = lm(ypcg ~ ctax + ypc2000, data = corpt)
summary(eq2)

eq3 = lm(ypcg ~ ctax + ypc2000  + (ctax * dty), data = corpt)
summary(eq3)

# Chart similar to Figure 4
plot(csv[,"ctax"],csv[,"ypcg"], xlab="Average Corporate Tax Rate'00-'08", ylab="Average GDP per capita Growth '00-'15") 
abline(lm(ypcg ~ ctax, data=csv), col="red") # add a regression line

## Best fitted with different variables
eq4 = lm(ypcg ~ ctax + ypc2000 + dty, data = corpt)
summary(eq4)

eq4a = lm(ypcg ~ ctax + ypc2000 + dty + trade, data = corpt)
summary(eq4a)

eq4b = lm(ypcg ~ ctax + ypc2000 + dty + trade  + ihc +y2000 , data = corpt)
summary(eq4b)

eq4c = lm(ypcg ~ ctax + dty + trade + ihc + y2000, data = corpt)
summary(eq4c)

eq4d = lm(ypcg ~ ctax + ypc2000 + (ctax * dty) + ihc, data = corpt)
summary(eq4d)

# The best model is eq4d = lm(ypcg ~ ctax + ypc2000 + (ctax * dty) + ihc, data = corpt)
## This is the best fitted model because as explained in the article, if corporate tax rises then
## factory productivity will also be affected in the opposite direction because if corporate tax is lower then there will be less tax imposed on high factory productivity.
## Also, the 2000 GDP in the US also shows that corporate tax is strongly correlated with GDP
## because in the year 2000 the US corporate tax remained the same rate while other countries lowered their corporate tax, which caused
## a significant increase in their 2000 GDP. Also the debt to GDP ratio is a significant variable in that if the country is in a considerable amount of debt
## compared to that year's GDP then the GDP will be nulled by the debt.



######### Part B
library(dplyr)

### Initialize variable 'p' above
# getting the csv files in "ACS_Dp02 data/"
file = "ACS_Dp02 data/" #paste(p, "ACS_Dp02 data/", sep = "")
fileList = list.files(path = paste(p, file, sep = ""), pattern = "*.csv")
fileList = paste(file, fileList, sep ="") # 'ACS_Dp02 data/' + file name ex: "ACS_Dp02 data/ACS_09_5YR_DP02_metadata.csv"

# reading the with_ann csv
ann1 = read.csv( file = fileList[2],skip = 1 )
ann2 = read.csv( file = fileList[4],skip = 1 )
ann3 = read.csv( file = fileList[6],skip = 1 )
ann4 = read.csv( file = fileList[8],skip = 1 )
ann5 = read.csv( file = fileList[10],skip = 1 )
ann6 = read.csv( file = fileList[12],skip = 1 )
ann7 = read.csv( file = fileList[14],skip = 1 )
ann8 = read.csv( file = fileList[16],skip = 1 )

colnames(ann1) = gsub("..Estimate", "", colnames(ann1))

## Variables
#2: Id2
#3: Geography
#232: Adult Population
#238: % in less than 9th grade
#242: % in 9th to 12 grade
#246, #250, #254, #258, #262

# calculating chci '09-'16
chci1 = (1/100) * ( (50 * ann1[238]) + (100 * ann1[242]) + (120 * ann1[246]) + (130 * ann1[250]) + (140 * ann1[254]) + (190 * ann1[258]) + (230 * ann1[262]) )
chci2 = (1/100) * ( (50 * ann2[238]) + (100 * ann2[242]) + (120 * ann2[246]) + (130 * ann2[250]) + (140 * ann2[254]) + (190 * ann2[258]) + (230 * ann2[262]) )
chci3 = (1/100) * ( (50 * ann3[238]) + (100 * ann3[242]) + (120 * ann3[246]) + (130 * ann3[250]) + (140 * ann3[254]) + (190 * ann3[258]) + (230 * ann3[262]) )
chci4 = (1/100) * ( (50 * ann4[238]) + (100 * ann4[242]) + (120 * ann4[246]) + (130 * ann4[250]) + (140 * ann4[254]) + (190 * ann4[258]) + (230 * ann4[262]) )
chci5 = (1/100) * ( (50 * ann5[238]) + (100 * ann5[242]) + (120 * ann5[246]) + (130 * ann5[250]) + (140 * ann5[254]) + (190 * ann5[258]) + (230 * ann5[262]) )
chci6 = (1/100) * ( (50 * ann6[238]) + (100 * ann6[242]) + (120 * ann6[246]) + (130 * ann6[250]) + (140 * ann6[254]) + (190 * ann6[258]) + (230 * ann6[262]) )
chci7 = (1/100) * ( (50 * ann7[238]) + (100 * ann7[242]) + (120 * ann7[246]) + (130 * ann7[250]) + (140 * ann7[254]) + (190 * ann7[258]) + (230 * ann7[262]) )
chci8 = (1/100) * ( (50 * ann8[238]) + (100 * ann8[242]) + (120 * ann8[246]) + (130 * ann8[250]) + (140 * ann8[254]) + (190 * ann8[258]) + (230 * ann8[262]) )

chci09 = data.frame(id = ann1[2], county = ann1[3], chci09=chci1)
chci10 = data.frame(id = ann2[2], chci10=chci2)
chci11 = data.frame(id = ann3[2], chci11=chci3)
chci12 = data.frame(id = ann4[2], chci12=chci4)
chci13 = data.frame(id = ann5[2], chci13=chci5)
chci14 = data.frame(id = ann6[2], chci14=chci6)
chci15 = data.frame(id = ann7[2], chci15=chci7)
chci16 = data.frame(id = ann8[2], chci16=chci8)
# Adult Population data frames
pop1 = data.frame(id = ann1[2], pop09 = ann1[232], pop10 = ann2[232], pop11 =ann3[232], pop12=ann4[232], pop13=ann5[232])
pop2 = data.frame(id = ann8[2], pop14=ann6[232], pop15=ann7[232], pop16=ann8[232])

chci = merge(chci09, chci10, by = "Id2", all = TRUE)
chci = merge(chci, chci11, by = "Id2", all = TRUE)
chci = merge(chci, chci12, by = "Id2", all = TRUE)
chci = merge(chci, chci13, by = "Id2", all = TRUE)
chci = merge(chci, chci14, by = "Id2", all = TRUE)
chci = merge(chci, chci15, by = "Id2", all = TRUE)
chci = merge(chci, chci16, by = "Id2", all = TRUE)
# merge Adult Population
chci = merge(chci, pop1, by = "Id2", all = TRUE)
chci = merge(chci, pop2, by = "Id2", all = TRUE)

# changing column names
colnames(chci) = c("id","county","chci09","chci10","chci11","chci12","chci13","chci14","chci15","chci16","pop09","pop10","pop11","pop12","pop13","pop14","pop15","pop16")

# calculating growth and growth rate
chci$chcig = chci$chci16-chci$chci09
chci$chcigr = (chci$chci16-chci$chci09) / chci$chci09
chci$popg = chci$pop16-chci$pop09
chci$popgr = (chci$pop16-chci$pop09) / chci$pop09

write.csv( chci, "chci.csv")


### ordering
empty = c()
for (i in 1:length(chci)) {
  empty[i] = NA
}

# ordering by top 20 counties
chci20 = arrange(chci, desc(chci$chci16)) [1:20,]
chci20 = rbind(chci20, empty)
chci20 = rbind(chci20, arrange(chci, desc(chci$chcig)) [1:20,])
chci20 = rbind(chci20, empty)
chci20 = rbind(chci20, arrange(chci, desc(chci$pop16)) [1:20,])
chci20 = rbind(chci20, empty)
chci20 = rbind(chci20,arrange(chci, desc(chci$popg)) [1:20,])
chci20 = rbind(chci20, empty)
chci20 = rbind(chci20,arrange(chci, desc(chci$popgr)) [1:20,])
chci20 = rbind(chci20, empty)

# ordering by bottom 20 counties
chci20 = rbind(chci20,arrange(chci, chci$chci16) [1:20,])
chci20 = rbind(chci20, empty)
chci20 = rbind(chci20,arrange(chci, chci$chcig) [1:20,])
chci20 = rbind(chci20, empty)
chci20 = rbind(chci20,arrange(chci, chci$popg) [1:20,])
chci20 = rbind(chci20, empty)
chci20 = rbind(chci20,arrange(chci, chci$popgr) [1:20,])

write.csv( chci20, "chci.top20.csv")
