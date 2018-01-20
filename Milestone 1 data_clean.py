# import packages
import json
import pandas as pd
import numpy as np
import locale
import zipfile
import glob
locale.setlocale( locale.LC_ALL, 'english_USA' )


def magnitude(x):
    import math
    return int(math.log10(x))
    
    
def get_int(x):
    """
    parse int from strings, e.g. '1,200'
    """
    # x=NaN
    if pd.isnull(x):
        return x
    # try whether string x can be directed cast as int
    try:
        return int(x)
    # if can't, use the package locale to parse it
    except:
        return locale.atoi(x)


def parse_timestamp(t):
    """UNIX time format"""
    from datetime import datetime
    return datetime.fromtimestamp(t).strftime('%Y-%m-%d %H:%M:%S')
    
# ##################################
# data fields
# ##################################
id_cols = [u'id']
int_cols = [
    # kick meta fields
    'spotlight', 'staff_pick',
    # scraped fields
    u'#backer', u'#bio_backed', u'#bio_created', u'#collaborators',
    u'#comments', u'#new_backer', u'#pledges', u'#return_backer', u'#upd',
    u'#websites', u'has_community', u'has_creator-bio',
    u'has_creator-full', u'has_proj', u'is_small_community', u'user_deleted',
    'has_facebook', '#friend_facebook', 'bio_auto_verified' ]

linguistic_cols = ['name','blurb', u'description_html', u'bio']
pledged_cols = ['goal', 'goal_magnitude', 'pledged', 'pledge_ratio', 'state', 'currency', 'static_usd_rate', 'usd_pledged']
category_cols = ['category_name', 'category_parent']

time_cols = [u'profile_joined_time', 'deadline','state_changed_at','created_at','launched_at']

location_cols = [
    # kick_meta
    'proj_country', 'proj_state', 'proj_location_type', 'proj_location_name', 'proj_location_display_name',
    # scraped
    'bio_loc', u'profile_loc', u'backer_cities', u'backer_countries'
]

reward_cols = [u'pledges']


# ######################################
# read pandas Dataframe from list of csv files
# ######################################
def concat_file(file_paths,index_col=None):
    df_list = []
    for fp in file_paths:
        df = pd.read_csv(fp, index_col=index_col)
        df_list.append(df)
    return pd.concat(df_list, ignore_index=True)

def read_df_from_zip(zip_file):
    df_list = []
    with zipfile.ZipFile(zip_file, 'r') as f:
        for fn in f.namelist():
            df = pd.read_csv(f.open(fn))
            df_list.append(df)
    df = pd.concat(df_list, ignore_index=True)
    return df

# ##############################
# cleaning functions
# ##############################

def clean_kick_meta(kick_meta):
    # parse location data from json format
    kick_meta['proj_country'] = kick_meta['location'].apply(lambda x: json.loads(x)['country'] if not pd.isnull(x) else '')
    kick_meta['proj_state'] = kick_meta['location'].apply(lambda x: json.loads(x)['state'] if not pd.isnull(x) else '')
    kick_meta['proj_location_type'] = kick_meta['location'].apply(lambda x: json.loads(x)['type'] if not pd.isnull(x) else '')
    kick_meta['proj_location_name'] = kick_meta['location'].apply(lambda x: json.loads(x)['name'] if not pd.isnull(x) else '')
    kick_meta['proj_location_display_name'] = kick_meta['location'].apply(lambda x: json.loads(x)['displayable_name'] if not pd.isnull(x) else '')
    # parse category data from json format
    kick_meta['category_name'] = kick_meta['category'].apply(lambda x: json.loads(x)['name'])
    kick_meta['category_parent'] = kick_meta['category'].apply(lambda x: json.loads(x)['slug'].split('/')[0])
    # calculate the ratio of pledged over goal
    kick_meta['pledge_ratio'] = kick_meta[['goal', 'pledged']].apply(lambda x: float(x.pledged)/x.goal, axis=1)
    # transform unix timestamp to human readable format
    for col in time_cols[1:]:
        kick_meta[col] = kick_meta[col].apply(parse_timestamp)

def clean_bio_facebook(x):
    if isinstance(x, (str,unicode)):
        if 'friend' in x:
            return x.replace('friends','').replace('friend','')
        else:
            return np.nan
    return x
        
def clean_scraped(scraped):
    # clean scraped
    print scraped['bio_auto_verified'].value_counts().to_dict()
    scraped['bio_auto_verified'] = scraped['bio_auto_verified'].apply(lambda x: 0 if pd.isnull(x) or x==0 else 1)
    print scraped['bio_auto_verified'].value_counts().to_dict()
    # parse facebook feature
    scraped['has_facebook'] = scraped['bio_facebook'].apply(lambda x: 1 if x!='Not connected' else 0)
    scraped['#friend_facebook'] = scraped['bio_facebook'].apply(clean_bio_facebook)
    scraped['#friend_facebook'] = scraped['#friend_facebook'].apply(get_int)


    
def clean_int_cols(merged, cols):
    # clean int field
    for col in cols:
        merged[col] = merged[col].apply(get_int)
        
        
# ####################################
# columns of scraped that are not used
# ####################################
discard_cols = ['profile_badge', 'bio_facebook', u'slug', u'profile_joined_time', u'blog_mention_html']
debug_cols = [u'len_badge', u'len_bckr', u'len_blog', u'len_cities_div',
       u'len_comments', u'len_countries_div', u'len_des_div', u'len_details',
       u'len_facebook', u'len_full_bio', u'len_header', u'len_header_p',
       u'len_joined', u'len_location', u'len_new_backer_div',
       u'len_return_backer_div', u'len_rew_div', u'len_tipsys', u'len_upd']
       

if __name__ =="__main__":
    
    # read and clean scraped project page data
    file_paths = glob.glob('data/projs-*.csv')
    scraped = concat_file(file_paths, index_col=0)
    scraped = scraped[(scraped.has_proj!=0) & (scraped['has_creator-bio']!=0)]
    print 'cleaning scraped data...'
    clean_scraped(scraped)
    
    # read and clean kick meta data
    # file_paths = ['data/Kickstarter00{}.csv'.format(i+1) for i in range(4)]
    # kick_meta = concat_file(file_paths)
    print 'reading df from zip file'
    kick_meta = read_df_from_zip('data/Kickstarter_2017-03-15T22_20_55_874Z.zip')
    clean_kick_meta(kick_meta)
    
    # merge meta and project page data
    print 'merging scraped data and kick_meta...'
    merged = scraped.merge(kick_meta, left_on='id', right_on='id')
    merged['goal_magnitude'] = merged.goal.apply(magnitude)
    clean_int_cols(merged, int_cols)
    
    # divided into different DataFrame
    print 'dividing into dff dfs'
    df_url = merged[id_cols + ['url']]
    # df_linguitic = merged[id_cols+linguistic_cols]
    df_main = merged[id_cols + pledged_cols + category_cols + int_cols]
    df_time = merged[id_cols + time_cols]
    df_location = merged[id_cols + location_cols]
    df_reward = merged[id_cols + reward_cols]
    
    # save to disk
    print 'saving data to disk'
    df_url.set_index('id').to_csv('data/projects_url.csv', encoding='utf-8')
    # df_linguitic.set_index('id').to_csv('data/projects_linguistic.csv', encoding='utf-8')
    df_main.set_index('id').to_csv('data/projects_features.csv', encoding='utf-8')
    df_time.set_index('id').to_csv('data/projects_time.csv', encoding='utf-8')
    df_location.set_index('id').to_csv('data/projects_location.csv', encoding='utf-8')
    df_reward.set_index('id').to_csv('data/projects_rewards.csv', encoding='utf-8')
    
