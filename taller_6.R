library(wooldridge)
library(modelsummary)
library(tidyverse)
library(plm)

# punto 1



# punto 2

# a
View(rental)
pols_2 <- lm(lrent ~ y90 + lpop + lavginc + pctstu,
              data=rental)
summary(pols_2)
# b


# c

diff_2 <- plm(lrent ~ y90 + lpop + lavginc + pctstu,
              index = c("city", "year"),
              method = "fd",
              data=rental)

summary(diff_2)


# d
fe_2 <- plm(lrent ~ y90 + lpop + lavginc + pctstu,
              index = c("city", "year"),
              method = "within",
            data=rental)

summary(fe_2)



# punto 3
View(jtrain)
# a

fe_3 <- plm(hrsemp ~ d88 + d89 + grant + grant_1 + lemploy, 
              model='within',
              index=c('fcode', 'year'),
              data=jtrain)
# punto 4

# a

# b

murder_filt <- murder |> filter(year != 87)

pols_4 <- lm(mrdrte ~ d93 + exec + unem,
             data=murder_filt)
summary(pols_4)

# c

fd_4 <- plm(mrdrte ~ d93 + exec + unem,
            data=murder_filt,
            index=c("id", "year"))
summary(fd_4)
View(murder)
# d


# e

murder_filt <- murder_filt |> filter(state != "TX")

fd_4_texas <- plm(mrdrte ~ d93 + exec + unem,
                  data=murder_filt,
                  index=c("id", "year"),
                  method="fd")

summary(fd_4_texas, vcov = vcovHC)
# f

fe_4_f <- plm(mrdrte ~ d90 + d93 + exec + unem,
              data=murder,
              index=c("id", "year"),
              method="within")
summary(fe_4_f)

# punto 5

