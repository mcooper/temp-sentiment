library(data.table)

# Definition of violent crimes: https://www.law.cornell.edu/uscode/text/18/16

###############################
# read nibrs data
###############################

crim <- fread('~/tweets/crime/all_crime_data.csv')

violent <- c('Homicide Offenses', 'Animal Cruelty', 'Arson', 'Assault Offenses',
             'Burglary/Breaking & Entering', 'Destruction/Damage/Vandalism of Property',
             'Kidnapping/Abduction',  'Robbery')

crim <- crim[ , .(homicides_nibrs=sum(offense_category_name == 'Homicide Offenses'), 
                assault_nibrs=sum(offense_category_name == 'Assault Offenses'),
                violent_nibrs=sum(offense_category_name %in% violent)),
            .(fips, date)]

crim$date <- as.character(crim$date)
crim <- crim[!is.na(crim$fips), ]
crim$fips <- substr(100000 + crim$fips, 2, 6)

##############################
# Read COD data
##############################
setwd('~/tweets/crime/CrimeOpenDatabase/')

cod <- rbindlist(lapply(list.files(pattern='core'), 
                        function(x) fread(x, colClasses=rep('character', 14))))

cod$fips <- substr(cod$census_block, 1, 5)
cod$date <- substr(cod$date_single, 1, 10)

violent <- c("arson", "assault offenses", "burglary/breaking & entering", 
             "destruction/damage/vandalism of property", "kidnapping/abduction",
             "robbery", "homicide offenses")

cod <- cod[ , .(homicides_cod=sum(offense_group == 'homicide offenses'),
                assault_cod=sum(offense_group == 'assault offenses'),
                violent_cod=sum(offense_group %in% violent)),
           .(fips, date)]

##################################
# Read in population data
#################################
pop <- rbindlist(lapply(list.files('~/tweets/suicide', pattern='^Population.*txt$', full.names=T),
                        read.delim))
pop <- pop[ , c("Population", "County.Code", "Yearly.July.1st.Estimates")]
pop <- pop[!is.na(pop$Population)]
names(pop) <- c('population', 'fips', 'year')
pop$fips <- substr(100000 + pop$fips, 2, 6)

##############################
# Combine
##############################
comb <- merge(crim, cod, all=T)
comb$year <- as.integer(substr(comb$date, 1, 4))
comb <- merge(comb, pop, all=T, by=c('year', 'fips'))
comb <- comb[!is.na(comb$date), ]

fwrite(comb, '../crime_cod_nibrs.csv', row.names=F)







