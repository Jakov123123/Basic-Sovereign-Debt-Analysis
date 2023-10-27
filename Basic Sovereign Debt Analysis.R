### Basic Sovereign Debt Analysis ###

### FOR A BRIEF EXPLANATION PLEASE READ THE ATTACHED WORD DOCUMENT ###

#let us first import the data properly
#install.packages("readxl")
library("readxl")
centralGovernmentDebt = read_excel("Central Government Debt to GDP.xls")
creditRating = read.csv("Credit Rating by Country.csv", sep = ";")
interest = read_excel("Interest Paid on Public Debts to GDP.xls")

#we will restrict ourselves only to data from 2020 (which in itself might not be the best
#idea because of the specific economic impacts of the pandemic)

#we will reorganize the data
interest = interest[c('Interest paid on public debt, percent of GDP (% of GDP)', '2020')]
colnames(interest) = c('country', 'interest_paid')
interest = interest[-1,]

creditRating = creditRating[order(creditRating$country),]

centralGovernmentDebt = centralGovernmentDebt[c('Central Government Debt (Percent of GDP)', '2020')]
colnames(centralGovernmentDebt) = c('country', 'central_government_debt')
centralGovernmentDebt = centralGovernmentDebt[-1,]
centralGovernmentDebt = data.frame(centralGovernmentDebt)

#turning values to numeric
centralGovernmentDebt[,2] = as.numeric(centralGovernmentDebt[,2])

#cleaning NAs
centralGovernmentDebt = centralGovernmentDebt[complete.cases(centralGovernmentDebt), ]

#doing the same with interest
interest = data.frame(interest)
interest[,2] = as.numeric(interest[,2])
interest = interest[complete.cases(interest), ]

d = merge(creditRating, interest, by = "country", all = FALSE)
d = merge(d, centralGovernmentDebt, by = "country", all = FALSE)

#let's code the credit rating to numbers so we may correlate (0 is Default and 22 is AAA+)
map_credit_to_numeric = function(credit_rating) {
  rating_values = c("D" = 0, "C" = 1, "CCC-" = 2, "CCC" = 3, "CCC+" = 4,
                     "B-" = 5, "B" = 6, "B+" = 7, "BB-" = 8, "BB" = 9, "BB+" = 10,
                     "BBB-" = 11, "BBB" = 12, "BBB+" = 13, "A-" = 14, "A" = 15, "A+" = 16,
                     "AA-" = 17, "AA" = 18, "AA+" = 19, "AAA-" = 20, "AAA" = 21, "AAA+" = 22)
  
  if (credit_rating %in% names(rating_values)) {
    return(rating_values[credit_rating])
  } else {
    return(NA)
  }
}

d$SWI = sapply(d$SWI, map_credit_to_numeric)

#cleaning the data again
d_clean = d[complete.cases(d), ]

cor(d_clean$SWI, d_clean$interest_paid)
#[1] -0.2858445
cor(d_clean$SWI, d_clean$central_government_debt)
#[1] -0.4119688

#Our first observation and the answer to our first question (discussion in the document)

#let us turn to answering the second question
breaks = c(-0.5, 5, 10, 15, 20, 22)
hist(d_clean$SWI, main = "The histogram of countries with credit ratings numerated from 0
     (Default) to 22 (AAA+)", breaks = breaks, xlab = "", ylab = "")
?hist
#let us create classes based on the specified borders
classes = cut(d_clean$SWI, breaks = breaks)
count_table = table(classes)

expected = rnorm(d_clean$SWI, mean = mean(d_clean$SWI), sd = sd(d_clean$SWI))
expected = prop.table(expected)
expected_counts = diff(pnorm(breaks, mean = mean(d_clean$SWI), sd = sd(d_clean$SWI)))
chi_squared_test = chisq.test(count_table, p = prop.table(expected_counts))
#our second observation and answer to our second question (discussed in the word document)

#our third question and our third observation (discussed in the word document)
cor(d_clean$interest_paid, d_clean$central_government_debt)