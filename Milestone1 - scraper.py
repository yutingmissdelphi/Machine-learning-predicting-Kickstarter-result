import datetime
import zipfile
from kick_parser import *
import json
from urlparse import urlparse
import pandas as pd
import urllib2
import random
from bs4 import BeautifulSoup


def main():
    # simulate browser request header
    headers_list = get_headers_list()
    # get project urls from meta data
    file_paths = ['data/Kickstarter00{}.csv'.format(i+1) for i in range(4)]
    projs_urls = urls_to_srape(file_paths)
    # begin scraping
    scrape(projs_urls, headers_list, debug=True)


def url_without_query_string(url):
    """ remove query string in url"""
    o = urlparse(url)
    return o.scheme + "://" + o.netloc + o.path
    
def get_headers_list():
    # fake useragent
    from fake_useragent import UserAgent
    ua = UserAgent()
    headers_list = [ua.chrome, ua.google, ua['google chrome'], ua.firefox, ua.ff,'Mozilla/5.0 (Macintosh; Intel Mac OS X 10.9; rv:32.0) Gecko/20100101 Firefox/32.0']
    return headers_list
    
    
def urls_to_srape_zip(zip_file):
    df_list = []
    with zipfile.ZipFile(zip_file, 'r') as f:

        for fn in f.namelist():
            df = pd.read_csv(f.open(fn))
            df = df[(df.state=='failed') | (df.state=='successful')]

            # project main page
            df['proj'] = df.urls.apply(lambda x: url_without_query_string(json.loads(x)['web']['project']))
            # df['update'] = df.proj.apply(lambda x: x+'/updates')
            # community page
            df['community'] = df.proj.apply(lambda x: x+'/community')
            # creator bio page
            df['creator-bio'] = df.proj.apply(lambda x: x+'/creator_bio')
            # creator full profile page
            df['creator-full'] = df.creator.apply(lambda x: json.loads(x)['urls']['web']['user']+'/about')

            keep_cols = ['id', 'slug', 'proj', 'community', 'creator-bio', 'creator-full']
            df_keep = df[keep_cols]
            df_list.append(df_keep)
    df = pd.concat(df_list, ignore_index=True)
    return df

def urls_to_srape(file_paths):
    # get all urls for each project
    df_list = []
    for fp in file_paths:
        df = pd.read_csv(fp)
        df = df[df.state=='failed' | df.state=='successful']
        
        # project main page
        df['proj'] = df.urls.apply(lambda x: url_without_query_string(json.loads(x)['web']['project']))
        # df['update'] = df.proj.apply(lambda x: x+'/updates')
        # community page
        df['community'] = df.proj.apply(lambda x: x+'/community')
        # creator bio page
        df['creator-bio'] = df.proj.apply(lambda x: x+'/creator_bio')
        # creator full profile page
        df['creator-full'] = df.creator.apply(lambda x: json.loads(x)['urls']['web']['user']+'/about')

        keep_cols = ['id', 'slug', 'proj', 'community', 'creator-bio', 'creator-full']
        df_keep = df[keep_cols]
        df_list.append(df_keep)
    return pd.concat(df_list, ignore_index=True)


def random_headers(headers_list):
    headers = {'User-Agent': random.choice(headers_list)}
    return headers
    
    
def req_html(proj_id, url, headers_list, timeout=10):
    """request for html by url with randome headers
    return:
        if request success: html and error=None 
        if request encounter error: html=None and error
    """
    headers = random_headers(headers_list)
    req = urllib2.Request(url, headers=headers)
    html = None
    error = None
    # catch url request error
    try:
        response = urllib2.urlopen(req,timeout=10)
        html = response.read()
    except urllib2.HTTPError, e:
        error = 'HTTPError = ' + str(e.code)
    except urllib2.URLError, e:
        error = 'URLError = ' + str(e.reason)
    except Exception as e:
        error = e
        
    if error is not None:
        error = (proj_id, url, error)
    return html, error
    

def scrape(projs_urls, headers_list, data_dir='data/', save_step=500, batch_cnt=0, debug=False, stop_batch=999999):
    """
    batch_cnt=0: start from No. batch_cnt, which means start from idx=batch_cnt*save_step. 
                           E.g. batch_cnt = 30, save_step=500 meaning it will start from idx=15000
    stop_batch=999999: stop before No. stop_batch, which means it stop when idx = stop_cnt*save_step 
                           (not including idx=stop_cnt*save step). 
                           E.g. stop_batch = 31, save step = 500, it will stop at idx=15500(the max idx scraped = 15499
    """
    errors = []
    batch_res = []
    error_file_name = data_dir + 'url_req_errors-{}_{}-{}.csv'
    batch_file_name = data_dir + 'projs-{}_{}-{}.csv'
 
    def parse_html_or_save_errors(proj_id, parser, page_name):
        # handle request error
        html, error = req_html(proj_id, url, headers_list)
        if html:
            soup = BeautifulSoup(html)
            parser(soup, res, debug=debug)
            res['has_'+page_name] = 1
        else:
            errors.append(error)
            res['has_'+page_name] = 0
            
    def save_batch(batch_cnt, len_batch):
        # save parsed data in memory to disk
        start = batch_cnt * save_step
        end = start + len_batch - 1
        # save and clear errors
        df_errors = pd.DataFrame(errors, columns=['id','url', 'error'])
        df_errors.to_csv(error_file_name.format(start, end, batch_cnt), encoding='utf-8')
        del errors[:]
        # save and clear scraped result
        df_res = pd.DataFrame.from_dict(batch_res)
        df_res.to_csv(batch_file_name.format(start, end, batch_cnt), encoding='utf-8')
        del batch_res[:]

    for idx, row in projs_urls.iterrows():
        # skip the first N batch
        if idx<batch_cnt*save_step:
            if idx==batch_cnt*save_step-1:
                print 'skip idx = %d and everything before it' % idx, datetime.datetime.now()
            continue
        # stop at some batch
        if idx>=stop_batch*save_step:
            print 'stop scraping before idx =', idx
            break
        proj_id = row['id']
        res = {'id': proj_id, 'slug': row.slug, 'url': row.proj}
        
        # parse project page
        col = 'proj'
        url = row[col]
        parse_html_or_save_errors(proj_id, parse_proj_page, col)
        
        # parse project community page
        col = 'community'
        url = row[col]
        parse_html_or_save_errors(proj_id, parse_commnunity_page, col)
        
        # parse creator bio page
        col = 'creator-bio'
        url = row[col]
        parse_html_or_save_errors(proj_id, parse_creator_bio_page, col)
        
        # parse creator full profile page
        col = 'creator-full'
        url = row[col]
        parse_html_or_save_errors(proj_id, parse_creator_full_page, col)
        
        batch_res.append(res)
        if (idx+1) % save_step == 0:
            print 'save %dth batch' % batch_cnt, 'each batch: %d rows' % save_step, datetime.datetime.now()
            save_batch(batch_cnt, len(batch_res))
            batch_cnt += 1
            
    # if there are scraped pages left
    if batch_res:
        print 'save last batch of %d rows' % len(batch_res), datetime.datetime.now()
        save_batch(batch_cnt, len(batch_res))


if __name__ == "__main__":
    main()
