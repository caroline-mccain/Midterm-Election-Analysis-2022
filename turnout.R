# set working directory
working_directory = ""
setwd(working_directory)

# load data
# data is a modified version of the ANES 2022 pilot, available from American National Election Studies: https://electionstudies.org/data-center/2022-pilot-study/
anes_data = read.csv("./anes_simplified.csv")

# subset data
anes_overall_evangelical <- subset(anes_data, evangelical == 1)
anes_overall_born_again <- subset(anes_data, born_again ==1)
anes_simplified_black <- subset(anes_data, race_b == 1)
anes_simplified_black_evangelical <- subset(anes_simplified_black, evangelical == 1)
anes_simplified_black_born_again <- subset(anes_simplified_black, born_again == 1)
anes_simplified_white <- subset(anes_data, race_w == 1)
anes_simplified_white_evangelical <- subset(anes_simplified_white, evangelical == 1)
anes_simplified_white_born_again <- subset(anes_simplified_white, born_again == 1)

# calculate turnout figures
overall_turnout = weighted.mean(anes_data$house_vote, anes_data$weight)
overall_evangelical_turnout = weighted.mean(anes_overall_evangelical$house_vote, anes_overall_evangelical$weight)
overall_born_again_turnout = weighted.mean(anes_overall_born_again$house_vote, anes_overall_born_again$weight)

overall_black_turnout = weighted.mean(anes_simplified_black$house_vote, anes_simplified_black$weight)
overall_black_evangelical_turnout = weighted.mean(anes_simplified_black_evangelical$house_vote, anes_simplified_black_evangelical$weight)
overall_black_born_again_turnout = weighted.mean(anes_simplified_black_born_again$house_vote, anes_simplified_black_born_again$weight)

overall_white_turnout = weighted.mean(anes_simplified_white$house_vote, anes_simplified_white$weight)
overall_white_evangelical_turnout = weighted.mean(anes_simplified_white_evangelical$house_vote, anes_simplified_white_evangelical$weight)
overall_white_born_again_turnout = weighted.mean(anes_simplified_white_born_again$house_vote, anes_simplified_white_born_again$weight)
