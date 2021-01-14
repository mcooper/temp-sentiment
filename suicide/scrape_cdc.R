library(tidyverse)
library(rvest)
library(RSelenium)
library(devtools) #Must include devtools and stringi https://github.com/ropensci/RSelenium/issues/186
library(stringi)

rD <- rsDriver(browser='firefox', port=4545L, verbose=F)
remDr <- rD[['client']]

remDr$navigate("https://wonder.cdc.gov/ucd-icd10.html")

### MANUAL PREPARATION ####
# Click Accept
#
# In Part 1: Set Group Results by to: County, Year and Month
# In Part 2: Cick on the "Census Regions" radio button
# In Part 6: Click the radio button for "Injury Intent and Mechanism", then in Injurty Intent select "Suicide"
# In Part 7: Check "Export Results" and uncheck "Show Totals"

#Must select and deselect an element from cens-r to clear it
remDr$findElement(using = 'xpath', value = paste0("//select[@id='codes-D76.V10']/option[@value='CENS-R1']"))$clickElement()

for (year in 2014:2019){
  #For some reason years dont update, must be manually set
  print(paste0("Set year to ", year))
  readline(prompt="Press [enter] to continue")
  for (cens in paste0('CENS-R', 1:4)){
    print(cens)
    
    #Clear Name
    remDr$findElement(using = 'id', value = 'TO_title')$clearElement()
    
    #Set Name
    name <- paste0('Suicides.', year, '.', cens)
    remDr$findElement(using = 'id', value = 'TO_title')$sendKeysToElement(list(name))

    #Selecting Census Regions
    remDr$findElement(using = 'xpath', value = paste0("//select[@id='codes-D76.V10']/option[@value='", cens, "']"))$clickElement()

    #Hit Download Option
    remDr$findElement(using = 'id', value = 'submit-button1')$clickElement()

    Sys.sleep(45)

    #Deselect Census Regions
    remDr$findElement(using = 'xpath', value = paste0("//select[@id='codes-D76.V10']/option[@value='", cens, "']"))$clickElement()
    
  }
}
