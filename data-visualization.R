# Load libraries
library(dplyr)
library(ggplot2)
library(psych)
library(lubridate)

# Load data
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
df <- read.csv("data_cleaned.csv")

## Login Attempts

# Count logins per account (here, our proxy is 1 transaction = 1 login event)
login_freq <- df %>%
  group_by(AccountID) %>%
  summarise(LoginEvents = n())

# Plot distribution
ggplot(login_freq, aes(x = LoginEvents)) +
  geom_histogram(bins = 30, fill = "black", color = "white") +
  labs(title = "Login Frequency per Account",
       x = "Number of Login Events",
       y = "Number of Accounts") +
  theme_classic()

# Summary stats
describe(login_freq$LoginEvents)

## Investigating high login attempts (anything > 5 to be conservative)
high_login_freq <- login_freq %>% filter(LoginEvents > 5)
flagged_df <- merge(df, high_login_freq, by = 'AccountID')

# Order account information
flagged_df <- flagged_df %>%
  arrange(AccountID, TransactionDate)

## Transaction Locations

user_primary_location <- df %>%
  group_by(AccountID, Location) %>%
  summarise(txn_count = n(), .groups = "drop") %>%
  arrange(AccountID, desc(txn_count)) %>%
  group_by(AccountID) %>%
  slice(1) %>%
  rename(PrimaryLocation = Location)

# Join back to main df
df <- df %>%
  left_join(user_primary_location, by = "AccountID") %>%
  mutate(OutOfPatternLocation = Location != PrimaryLocation)

# How often do users transact outside of their primary locations?
df %>%
  group_by(OutOfPatternLocation) %>%
  summarise(count = n()) %>%
  mutate(percent = count / sum(count))

ggplot(df, aes(x = OutOfPatternLocation, fill = OutOfPatternLocation)) +
  geom_bar() +
  labs(title = "Transactions in vs. Outside Primary Location",
       x = "Out-of-Pattern Location", y = "Number of Transactions") +
  theme_minimal() +
  scale_fill_manual(values = c("grey", "black")) + 
  theme_classic()

# Are these outside of primary transactions unrealistic?

# Ensure TransactionDate is in datetime format
df$TransactionDate <- as.POSIXct(df$TransactionDate)

# Sort by user and time
df <- df %>%
  arrange(AccountID, TransactionDate) %>%
  group_by(AccountID) %>%
  mutate(
    PrevLocation = lag(Location),
    PrevTime = lag(TransactionDate),
    TimeDiffHours = as.numeric(difftime(TransactionDate, PrevTime, units = "hours")),
    LocationJump = Location != PrevLocation,
    GeoVelocityFlag = LocationJump & TimeDiffHours < 6
  ) %>%
  ungroup()

# How many suspicious jumps?
table(df$GeoVelocityFlag)

# Take a look at the df! 
df %>%
  filter(GeoVelocityFlag) %>%
  select(AccountID, TransactionDate, PrevTime, Location, PrevLocation, TimeDiffHours)

# Ensure TransactionDate is a datetime
df$TransactionDate <- as.POSIXct(df$TransactionDate)

df <- df %>%
  arrange(AccountID, TransactionDate) %>%
  group_by(AccountID) %>%
  mutate(
    PrevLocation = lag(Location),
    PrevTime = lag(TransactionDate),
    TimeDiffDays = as.numeric(difftime(TransactionDate, PrevTime, units = "days")),
    LocationJump = Location != PrevLocation,
    GeoVelocityFlag = LocationJump & TimeDiffDays <= 2
  ) %>%
  ungroup()

# Zoom into suspicious jumps that happened within 1 day
jump_1day_df <- df %>%
  filter(LocationJump, !is.na(TimeDiffDays), TimeDiffDays <= 1)

jump_counts <- jump_1day_df %>%
  count(AccountID, name = "NumJumps") %>%
  arrange(desc(NumJumps))

library(ggplot2)

# Plot top 10 users with most jumps within 1 day
top10 <- jump_counts %>% slice_max(NumJumps, n = 10)

ggplot(top10, aes(x = reorder(AccountID, NumJumps), y = NumJumps)) +
  geom_col(fill = "orangered") +
  coord_flip() +
  labs(
    title = "Top Users by Suspicious Location Jumps Within 1 Day",
    x = "Account ID",
    y = "Number of Suspicious Jumps"
  ) +
  theme_bw()

