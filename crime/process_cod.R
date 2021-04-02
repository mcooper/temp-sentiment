library(tidyverse)

setwd('~/tweets/crime/CrimeOpenDatabase')

d1 <- read.csv('crime_open_database_core_2009.csv')
d2 <- read.csv('crime_open_database_extended_2009.csv')


cols <- c("offense_type_id", "location_id", "agency_id", "incident_id",
  "offense_id", "date", "hour", "report_date_flag", "fips", "location_name",
  "offense_name", "offense_category_name", "month")

oldnames <- tolower(c("Aggravated Assault", "All Other Larceny", "Animal Cruelty",
  "Arson", "Assisting or Promoting Prostitution", "Betting/Wagering",
  "Bribery", "Burglary/Breaking & Entering", "Counterfeiting/Forgery",
  "Credit Card/Automated Teller Machine Fraud", "Destruction/Damage/Vandalism of Property",
  "Drug Equipment Violations", "Drug/Narcotic Violations", "Embezzlement",
  "Extortion/Blackmail", "False Pretenses/Swindle/Confidence Game",
  "Fondling", "Gambling Equipment Violation", "Hacking/Computer Invasion",
  "Human Trafficking, Commercial Sex Acts", "Human Trafficking, Involuntary Servitude",
  "Identity Theft", "Impersonation", "Incest", "Intimidation",
  "Justifiable Homicide", "Kidnapping/Abduction", "Motor Vehicle Theft",
  "Murder and Nonnegligent Manslaughter", "Negligent Manslaughter",
  "Operating/Promoting/Assisting Gambling", "Pocket-picking", "Pornography/Obscene Material",
  "Prostitution", "Purchasing Prostitution", "Purse-snatching",
  "Rape", "Robbery", "Sexual Assault With An Object", "Shoplifting",
  "Simple Assault", "Sodomy", "Sports Tampering", "Statutory Rape",
  "Stolen Property Offenses", "Theft From Building", "Theft From Coin-Operated Machine or Device",
  "Theft From Motor Vehicle", "Theft of Motor Vehicle Parts or Accessories",
  "Weapon Law Violations", "Welfare Fraud", "Wire Fraud"))

newnames <- unique(d1$offense_group)
