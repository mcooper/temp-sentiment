library(sf)
library(tidyverse)
library(foreach)
library(doParallel)

setwd('/home/ubuntu/tweets/')

#Because it is 5-year average, use the middle year of the 5.  So 2011 is 2007-2011, so most accurate to 2009

#Convert all to 2019 dollars
#https://data.bls.gov/cgi-bin/cpicalc.pl?cost1=100&year1=201106&year2=201906
convert <- data.frame(year=2011:2018,
                      conv=c(1.134772, 1.1161985, 1.0969534, 1.0746823, 
                             1.0733538, 1.0627546, 1.0456737, 1.0164848))

cl <- makeCluster(64, outfile = '')
registerDoParallel(cl)

for (year in 2009:2019){
  system(paste0('~/telegram.sh "', year, '"'))
  data_year <- ifelse(year < 2017, year + 2, 2018)

  sp <- read_sf(paste0('wealth/nhgis0002_shape/US_blck_grp_', data_year, '.shp'))
  dat <- read.csv(list.files(path='wealth/nhgis0003_csv/',
                             full.names=T,
                             pattern=paste0(data_year, '5_', data_year, '.*grp_E.csv')))


  names(dat)[38:94] <- c("TOTAL_POPULATION", "RACE_TOT", "RACE_NHISP_TOTXX", "RACE_WHITE", 
                         "RACE_BLACK", "RACE_INDIAN", "RACE_ASIAN", "RACE_OTHER1", "RACE_OTHER2",
                         "RACE_OTHER3", "RACE_OTHER4", "RACE_OTHER5", "RACE_HISP", 
                         "RACE_HISPXX1", "RACE_HISPXX2", "RACE_HISPXX3", "RACE_HISPXX4", 
                         "RACE_HISPXX5", "RACE_HISPXX6", "RACE_HISPXX7", "RACE_HISPXX8", 
                         "RACE_HISPXX9", "TRAN_TOT", "TRAN_CAR1", "TRAN_CAR2", "TRAN_CAR3", 
                         "TRAN_CAR4", "TRAN_CAR5", "TRAN_CAR6", "TRAN_CAR7", "TRAN_CAR8", 
                         "TRAN_PUB1", "TRAN_PUB2", "TRAN_PUB3", "TRAN_PUB4", "TRAN_PUB5", 
                         "TRAN_PUB6", "TRAN_CAR9", "TRAN_EXP1", "TRAN_EXP2", "TRAN_EXP3", 
                         "TRAN_OTHER", "TRAN_WFH", "TIME_TOT", "TIME_LT5", "TIME_5_9",
                         "TIME_10_14", "TIME_15_19", "TIME_20_24", "TIME_25_29", "TIME_30_34", 
                         "TIME_35_39", "TIME_40_44", "TIME_45_59", "TIME_60_89", "TIME_GT90",
                         "INCOME_PERCAP")

  dat <- dat %>%
    mutate(RACE_WHITE = RACE_WHITE/RACE_TOT,
           RACE_BLACK = RACE_BLACK/RACE_TOT,
           RACE_OTHER = (RACE_OTHER1 + RACE_OTHER2 + RACE_OTHER3 + RACE_OTHER4 + 
                         RACE_OTHER4 + RACE_OTHER5 + RACE_INDIAN + RACE_ASIAN)/RACE_TOT,
           RACE_HISP = RACE_HISP/RACE_TOT,
           FIPS_COUNTY = substr(1000 + COUNTYA, 2, 5),
           FIPS_STATE = substr(100 + STATEA, 2, 3)) %>%
    select(RACE_WHITE, RACE_BLACK, RACE_OTHER, RACE_HISP, INCOME_PERCAP,
           GISJOIN, FIPS_STATE, FIPS_COUNTY)

	dat$INCOME_PERCAP <- dat$INCOME_PERCAP*convert$conv[convert$year==data_year]

  m <- merge(sp, dat)

  fs <- list.files('tweets/', full.names=T, pattern=paste0(year, '.*csv$'))

  foreach(f=fs, .packages=c('sf', 'tidyverse')) %dopar% {
		#Read in and spatialize data, handle empty files
    twt <- tryCatch({
        		read.csv(f) %>%
							select(-pure_text) %>%
              na.omit %>%
							st_as_sf(coords=c('lon', 'lat'), crs=4326, remove=F) %>%
							st_transform(crs=st_crs(m))
        }, error = function(err) {
            print(paste("Empty:  ", f))
						0
        })

		if (!'sf' %in% class(twt)){
			return(NULL)
		}
	
		#Run spatial process and write	
		st_join(twt, m) %>%
			st_drop_geometry %>%
			select(id, tweet_created_at, INCOME_PERCAP, lat, lon,
             matches("FIPS"), matches("RACE")) %>%
			write.csv(gsub('tweets', 'wealth/results', f), row.names=F)
		
		print(f)
	}
}

system('~/telegram.sh "Done with Wealth Processing!')


