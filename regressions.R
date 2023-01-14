# set working directory
working_directory = ""
setwd(working_directory)

# install and load libraries
# install.packages("jtools")
library(jtools)

# load data
# data available from American National Election Studies: https://electionstudies.org/data-center/2022-pilot-study/
anes_2022_pilot = read.csv("./anes_pilot_2022_csv_20221214/anes_pilot_2022_csv_20221214.csv")

# select certain variables for analysis
anes_simplified <- data.frame(caseid = anes_2022_pilot$caseid, weight = anes_2022_pilot$weight, house_vote = anes_2022_pilot$house22t, house_vote_party = anes_2022_pilot$house22p,
                              covid19 = anes_2022_pilot$impstem_covid19, abortion = anes_2022_pilot$impstem_abortion, climate = anes_2022_pilot$impstem_climate,
                              CoL = anes_2022_pilot$impstem_CoL, crime = anes_2022_pilot$impstem_crime, jobs = anes_2022_pilot$impstem_jobs,
                              immigration = anes_2022_pilot$impstem_illegal_immigration, guns = anes_2022_pilot$impstem_gun_policy, 
                              taxes = anes_2022_pilot$impstem_taxes, healthcare = anes_2022_pilot$impstem_health_care, voting_rights = anes_2022_pilot$impstem_voting_rights,
                              voter_fraud = anes_2022_pilot$impstem_voter_fraud, public_school_education = anes_2022_pilot$impstem_public_school_educ, public_school_funding = anes_2022_pilot$impstem_public_school_funding,
                              fundamentalist_evangelical = anes_2022_pilot$idfundevan, born_again = anes_2022_pilot$pew_bornagain, 
                              gender = anes_2022_pilot$gender, race_w = anes_2022_pilot$rwh, race_b = anes_2022_pilot$rbl, education = anes_2022_pilot$educ, marital_status = anes_2022_pilot$marstat)

# set N/A weight to 0
anes_simplified$weight[is.na(anes_simplified$weight)] <- 0

# create vote variable, 0 = did not vote for house candidate (or skip), 1 = voted
anes_simplified$vote <- ifelse(anes_simplified$house_vote == 1, 1, 0)

# create evangelical variable, 0 = not evangelical, 1 = evangelical or both evangelical and fundamentalist
anes_simplified$evangelical <- ifelse(anes_simplified$fundamentalist_evangelical == 2 | anes_simplified$fundamentalist_evangelical == 3, 1, 0)

# adjust born_again variable to match above, 0 = not born again, 1 = born again
anes_simplified$born_again <- ifelse(anes_simplified$born_again == 2, 0, 1)

# change categories of independent variables to start at 0 instead of 1
anes_simplified$race_w <- ifelse(anes_simplified$race_w == 2, 0, 1)
anes_simplified$race_b <- ifelse(anes_simplified$race_b == 2, 0, 1)
anes_simplified$education <- anes_simplified$education - 1
anes_simplified$marital_status <- anes_simplified$marital_status -1
anes_simplified$gender <- anes_simplified$gender - 1

# convert categorical variables to factors
anes_simplified$race_w <- factor(anes_simplified$race_w)
anes_simplified$race_b <- factor(anes_simplified$race_b)
anes_simplified$education <- factor(anes_simplified$education)
anes_simplified$marital_status <- factor(anes_simplified$marital_status)
anes_simplified$gender <- factor(anes_simplified$gender)
anes_simplified$vote <- factor(anes_simplified$vote)
anes_simplified$evangelical <- factor(anes_simplified$evangelical)
anes_simplified$born_again <- factor(anes_simplified$born_again)

# save anes_simplified as a csv file
write.csv(anes_simplified, "./anes_simplified.csv", row.names = FALSE)



# simple model - regress evangelical and born_again on vote
simple_model_evangelical <- glm(vote ~ evangelical, data = anes_simplified, weights = anes_simplified$weight, family = "binomial")
summ(simple_model_evangelical)
simple_model_born_again <- glm(vote ~ born_again, data = anes_simplified, weights = anes_simplified$weight, family = "binomial")
summ(simple_model_born_again)


# interactions but not with race
interaction_model_evangelical <- glm(vote ~ evangelical + education + evangelical:education + marital_status + evangelical:marital_status 
                        + gender + evangelical:gender, data = anes_simplified, weights = anes_simplified$weight, family = "binomial")
summ(interaction_model_evangelical)
interaction_model_born_again <- glm(vote ~ born_again + education + born_again:education + marital_status + born_again:marital_status 
                                     + gender + born_again:gender, data = anes_simplified, weights = anes_simplified$weight, family = "binomial")
summ(interaction_model_born_again)


# include interaction terms for race
race_model_white_evangelical <- glm(vote ~ evangelical + race_w + evangelical:race_w, data = anes_simplified, weights = anes_simplified$weight, family = "binomial")
summ(race_model_white_evangelical)
race_model_white_born_again <- glm(vote ~ born_again + race_w + born_again:race_w, data = anes_simplified, weights = anes_simplified$weight, family = "binomial")
summ(race_model_white_born_again)
race_model_black_evangelical <- glm(vote ~ evangelical + race_b + evangelical:race_b, data = anes_simplified, weights = anes_simplified$weight, family = "binomial")
summ(race_model_black_evangelical)
race_model_black_born_again <- glm(vote ~ born_again + race_b + born_again:race_b, data = anes_simplified, weights = anes_simplified$weight, family = "binomial")
summ(race_model_black_born_again)

# interaction model with race interactions
race_interaction_white_evangelical <- glm(vote ~ evangelical + race_w + evangelical:race_w + education + evangelical:education + marital_status + evangelical:marital_status 
                             + gender + evangelical:gender, data = anes_simplified, weights = anes_simplified$weight, family = "binomial")
summ(race_interaction_white_evangelical)
race_interaction_white_born_again <- glm(vote ~ born_again + race_w + born_again:race_w + education + born_again:education + marital_status + born_again:marital_status 
                                         + gender + born_again:gender, data = anes_simplified, weights = anes_simplified$weight, family = "binomial")
summ(race_interaction_white_born_again)
race_interaction_black_evangelical <- glm(vote ~ evangelical + race_b + evangelical:race_b + education + evangelical:education + marital_status + evangelical:marital_status 
                              + gender + evangelical:gender, data = anes_simplified, weights = anes_simplified$weight, family = "binomial")
summ(race_interaction_black_evangelical)
race_interaction_black_born_again <- glm(vote ~ born_again + race_b + born_again:race_b + education + born_again:education + marital_status + born_again:marital_status 
                                         + gender + born_again:gender, data = anes_simplified, weights = anes_simplified$weight, family = "binomial")
summ(race_interaction_black_born_again)

# subset data into white respondents, black respondents, white evangelicals, and black evangelicals
anes_simplified_black <- subset(anes_simplified, race_b == 1)
anes_simplified_white <- subset(anes_simplified, race_w == 1)

# simple models from data subsets
subset_white_model_evangelical <- glm(vote ~ evangelical, data = anes_simplified_white, weights = anes_simplified_white$weight, family = "binomial")
summ(subset_white_model_evangelical)
subset_white_model_born_again <- glm(vote ~ born_again, data = anes_simplified_white, weights = anes_simplified_white$weight, family = "binomial")
summ(subset_white_model_born_again)

subset_black_model_evangelical <- glm(vote ~ evangelical, data = anes_simplified_black, weights = anes_simplified_black$weight, family = "binomial")
summ(subset_black_model_evangelical)
subset_black_model_born_again <- glm(vote ~ born_again, data = anes_simplified_black, weights = anes_simplified_black$weight, family = "binomial")
summ(subset_black_model_born_again)
