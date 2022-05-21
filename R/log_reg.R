library(dplyr)
library(mice)
library(glmnet)

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
#true values
true_rating <- my_tbl %>%
    slice((1:nrow(my_tbl))[!train]) %>%
    pull(OUTSTANDING)

###################
## basic log reg ##
###################
basic_fit <- glm(OUTSTANDING ~ ISPOST16 + NOR + PSENELSE + PSENELK + #SCHOOLTYPE +
                     PNUMENGFL + PNUMFSMEVER + PUPILTEACHERRATIO + PUPILTARATIO + 
                     PUPILSUPPORTRATIO + MIXED + ADMPOL_SELECTIVE + RELCHAR_YES +
                     MINORGROUP_ACADEMY + PUPILTARATIOLOG + PUPILSUPPORTRATIOLOG,
                 data = my_tbl,
                 family = binomial,
                 subset = train)
basic_probs <- predict(outstanding_fit,
                       newdata = my_tbl[!train,],
                       type = "response")

basic_rating <- ifelse(basic_probs > 0.5, 1, 0)

table(basic_rating, true_rating)
mean(basic_rating == true_rating)

#######################
## regulated log reg ##
#######################
reg_cov <- model.matrix(OUTSTANDING ~ ISPOST16 + NOR + PSENELSE + PSENELK + #SCHOOLTYPE +
                            PNUMENGFL + PNUMFSMEVER + PUPILTEACHERRATIO + PUPILTARATIO + 
                            PUPILSUPPORTRATIO + MIXED + ADMPOL_SELECTIVE + RELCHAR_YES +
                            MINORGROUP_ACADEMY + PUPILTARATIOLOG + PUPILSUPPORTRATIOLOG,
                        data = my_tbl)[,-1]

reg_outcome <- my_tbl %>% pull(OUTSTANDING)

#0 = full ridge, 1 = full lasso
reg_alpha <- 0.9

#obtain best lambda (reg penalty term) estimate in terms of MSE using cross validation
lambda_cv <- cv.glmnet(
    x = reg_cov,
    y = reg_outcome,
    alpha = reg_alpha
)

plot(lambda_cv)
plot(lambda_cv$glmnet.fit, "lambda")

lambda_min <- lambda_cv$lambda.min
lambda_1se <- lambda_cv$lambda.1se

reg_model <- glmnet(
    x = reg_cov,
    y = reg_outcome,
    alpha = reg_alpha, 
    lambda = lambda_min
)

#compare basic and reg coeffs
signif(
    cbind(
        basic = basic_fit$coefficients,
        reg = c(reg_model$a0,  as.vector(reg_model$beta))
    ),
    3
)

reg_probs <- predict.glmnet(
    reg_model,
    newx = my_tbl %>%
        select(-c(URN, SCHOOLTYPE, OUTSTANDING)) %>%
        slice((1:nrow(my_tbl))[!train])%>%
        as.matrix(),
    type = "response"
)

reg_rating <- ifelse(reg_probs > 0.5, 1, 0)

table(reg_rating, true_rating)
mean(reg_rating == true_rating)
