#####################################################################
# To quickly download all MICS Data, it is necessary to 
# automate a web browser with selenium
# This may require some set up:

from selenium import webdriver
import time
import os

################################
#Download data
################################

driver = webdriver.Chrome()

driver.get('https://crime-data-explorer.app.cloud.gov/downloads-and-docs')

statebox = driver.find_element_by_id('nibrs-state')
states = [x.get_attribute('label') for x in statebox.find_elements_by_tag_name('option')]
states = [x for x in states if x != 'Location']

yearbox = driver.find_element_by_id('nibrs-year')

f = open('download', 'w')
for state in states:
    statebox.send_keys(state)

    years = [x.get_attribute('label') for x in yearbox.find_elements_by_tag_name('option')] 
    years = [x for x in years if x in ['2009', '2010', '2011', '2012', '2013', '2014', '2015', 
                                       '2016', '2017', '2018', '2019']]

    time.sleep(1)

    for year in years:
        yearbox.send_keys(year)

        link = driver.find_element_by_id('nibrs-link').get_attribute('text')
        print(link)

        f.write(link + '\n')

        time.sleep(1)

f.close()
