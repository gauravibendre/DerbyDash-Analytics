

library(readxl)
Barn <- read_excel("Derby.xlsx",sheet = "Barn List")
leaderboard <- read_excel("Derby.xlsx",sheet = "Leaderboard")
Schedule <- read_excel("Derby.xlsx",sheet = "Schedule of the Day")
Equibase <- read_excel("Derby.xlsx",sheet = "Equibase")
Draw <- read_excel("Derby.xlsx",sheet = "2022 Post Position Draw")

#1
##Check Missing values
missing_values_Barn <- colSums(is.na(Barn))
print(missing_values_Barn)

## The missing value columns are ML Odds, config
## and, we do not need these two columns, so we decide to drop it
Barn <- Barn[, !(colnames(Barn) %in% c("ML Odds", "config"))]

##Check Duplicates
duplicated_rows <- duplicated(Barn)
# Print the result, We do not have any duplicates in dataset
print(duplicated_rows)
## Drop useless columns that do not affect our analysis
Barn <- Barn[, !(colnames(Barn) %in% c("Final Workout", "Color","Sire"))]

#2

##Check Missing values
missing_values_leaderboard <- colSums(is.na(leaderboard))
print(missing_values_leaderboard)

## The missing value columns are Exercise Rider, Training Time, Ship In Date, Notes,Stall
## and, we do not need these two columns, so we decide to drop it
leaderboard <- leaderboard[, !(colnames(leaderboard) %in% c("Exercise Rider", "Training Time",
                                                            "Ship In Date","Notes","Stall"))]
##Check Duplicates
duplicated_rows_l <- duplicated(leaderboard)
# Print the result, We do not have any duplicates in dataset
print(duplicated_rows_l)

#3
##Check Missing values
missing_values_Equibase <- colSums(is.na(Equibase))
print(missing_values_Equibase)

## The missing value columns are Exercise Rider, Training Time, Ship In Date, Notes,Stall
## and, we do not need these two columns, so we decide to drop it
Equibase <- Equibase[, !(colnames(Equibase) %in% c("M/E"))]
##Check Duplicates
duplicated_rows_E <- duplicated(Equibase)
# Print the result, We do not have any duplicates in dataset
print(duplicated_rows_l)
#Edit column to form we want

Equibase$`Horse Name (Jockey)`<- gsub("\\s*\\([^()]*\\)", "", Equibase$`Horse Name (Jockey)`)

colnames(Equibase)[3] <- "Horse"

Equibase$`Day Since Last Raced` <- as.numeric(difftime(Sys.Date(), Equibase$`Last Raced`, units = "days"))
#delete the columns we do not want
Equibase <- Equibase[, !(colnames(Equibase) %in% c("comments","PP"))]

#4 
df_merged <- merge(Draw, Equibase, by = "Horse", all = TRUE)


library(dplyr)

df_merged[7, c("Last Raced", "PGM", "Wgt", "horse_rating", "Odds", "Day Since Last Raced")] <- c('2022-04-06', 20, 126, "60-40",19.8,"362")
df_merged[12, c("Last Raced", "PGM", "Wgt", "horse_rating", "Odds", "Day Since Last Raced")] <- c('2022-04-02', 22, 126, "60-40",35.0,"376")
df_merged$`Last Raced` <- format(as.Date(df_merged$`Last Raced`), "%Y-%m-%d")

df2_merged <- merge(df_merged, Barn, by="Horse", all= TRUE)
df2_merged <- df2_merged[, !(colnames(df2_merged) %in% c("Rank", "Last Race","PGM",
                                    "Jockey.y"))]
df2_merged <- df2_merged %>% 
  rename(Jockey = Jockey.x)
library(dplyr)

df2_merged$Record <- sub("\\$.+", "", df2_merged$Record)
df2_merged$Record <- gsub("-(?!.*-)", "", df2_merged$Record, perl=TRUE)

# Split the Record column into separate parts
record_split <- strsplit(df2_merged$Record, "-")

# Extract the different parts and assign them to new columns
df2_merged$TotalRaces <- sapply(record_split, `[`, 1)
df2_merged$FirstPlaceWins <- sapply(record_split, `[`, 2)
df2_merged$SecondPlaceWins <- sapply(record_split, `[`, 3)
df2_merged$ThirdPlaceWins <- sapply(record_split, `[`, 4)


Jockey <- read_excel("Derby2.0.xlsx",sheet = "JockeyDataset")

df3_merged <- merge(df2_merged, Jockey, by = "Jockey", all = TRUE)

Horse <- read_excel("Derby2.0.xlsx",sheet = "HorseDataset")
df4_merged <- merge(df3_merged, Horse, by = "Horse", all = TRUE)

install.packages("openxlsx")
library(openxlsx)

write.xlsx(df4_merged, "Big Horse Dataset.xlsx", colName=TRUE)

#Other Datasets

#Attendance by Time
Attendance <- read_excel("Derby2.0.xlsx",sheet = "AttendanceDataset")
Attendance$`Number of Attendees` <- format(Attendance$`Number of Attendees`, big.mark = ",")

write.xlsx(Attendance, "Attendance By Time.xlsx", colName=TRUE)

#Attendance by Year
YearlyAttendance <- read_excel("Derby2.0.xlsx",sheet = "YearlyAttendance")
#dropping other horse races other than Kentucky Derby

YearlyAttendance <- YearlyAttendance[, c("Year", "Kentucky Derby Saturday")]

#dropping year 2020 because "All races listed held behind closed doors" 
YearlyAttendance <- YearlyAttendance[-2, ]
YearlyAttendance$`Kentucky Derby Saturday` <- sub("\\+$", "", YearlyAttendance$`Kentucky Derby Saturday`)

write.xlsx(YearlyAttendance, "Attendance By Year.xlsx", colName=TRUE)




