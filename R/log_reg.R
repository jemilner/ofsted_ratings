library(dplyr)
library(mice)

set.seed(1)

#read in imputated data
imputated_data <- readRDS("data/imputed_data.rds")

#get a complete data set
complete_tbl <- mice::complete(imputated_data, 1)

#sample same number of outstanding and not-outstanding rows
num_oustanding <- sum(complete_tbl$OUTSTANDING == 1)
num_not_outstanding <- sum(complete_tbl$OUTSTANDING == 0)

my_tbl <- bind_rows(
    outstanding_tbl <- complete_tbl %>%
        filter(OUTSTANDING == 1),
    other_tbl <- complete_tbl %>%
        filter(OUTSTANDING == 0) %>%
        slice(sample(1:num_not_outstanding, size = num_oustanding))
)

#set training indices
train <- sample(c(TRUE, FALSE), size = nrow(my_tbl), replace = T, prob = c(0.8, 0.2))

#not including pnorg
outstanding_fit <- glm(OUTSTANDING ~ ISPOST16 + NOR + PSENELSE + PSENELK + #SCHOOLTYPE +
                           PNUMENGFL + PNUMFSMEVER + PUPILTEACHERRATIO + PUPILTARATIO + 
                           PUPILSUPPORTRATIO + MIXED + ADMPOL_SELECTIVE + RELCHAR_YES +
                           MINORGROUP_ACADEMY + PUPILTARATIOLOG + PUPILSUPPORTRATIOLOG,
                       data = my_tbl,
                       family = binomial,
                       subset = train)
outstanding_probs <- predict(outstanding_fit,
                             newdata = my_tbl[!train,],
                             type = "response")

pred_rating <- ifelse(outstanding_probs > 0.5, 1, 0)

true_rating = my_tbl$OUTSTANDING[!train]
table(pred_rating, true_rating)

mean(pred_rating == true_rating)
