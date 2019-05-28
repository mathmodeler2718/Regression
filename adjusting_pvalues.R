#Matthew Anderson
#9.23.2018
#Hw1 adjustig p values 

library(readxl)
data <- read_excel("~/VCU/Biomedical_Data_1/HW1/data.xlsx")

test_1_pvalues<-data$test1
test_2_pvalues<-data$test2

test_1_bonf<-p.adjust(test_1_pvalues, method="bonferroni", n=20)
test_2_bonf<-p.adjust(test_2_pvalues, method="bonferroni", n=20)

which(test_1_bonf<.05)
which(test_2_bonf<.05)

test_1_holm<-p.adjust(test_1_pvalues, method="holm", n=20)
test_2_holm<-p.adjust(test_2_pvalues, method="holm", n=20)

which(test_1_holm<.05)
which(test_2_holm<.05)

test_1_fdr<-p.adjust(test_1_pvalues, method="fdr", n=20)
test_2_fdr<-p.adjust(test_2_pvalues, method="fdr", n=20)

which(test_1_fdr<.05)
which(test_2_fdr<.05)



