
from scraper import *

print 'generating header list'
headers_list = get_headers_list()
zip_path = 'data/Kickstarter_2017-03-15T22_20_55_874Z.zip'
print 'reading url list'
projs_urls = urls_to_srape_zip(zip_path)
print 'scraping'
scrape(projs_urls, headers_list, debug=True, batch_cnt=7, save_step=10000)