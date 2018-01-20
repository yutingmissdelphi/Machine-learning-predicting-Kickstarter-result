# coding=utf8
# a parser for kickstarter.com project pages
import json


class BackerCity:
    
    def __init__(self, city, country, cnt):
        self.city = city
        self.country = country
        self.cnt = cnt
    
    def to_dict(self):
        return {'city': self.city, 'country': self.country, 'cnt': self.cnt}
    
    def to_json(self):
        return json.dumps(self.to_dict())


class BackerCountry:
    
    def __init__(self, country, cnt):
        self.country = country
        self.cnt = cnt
    
    def to_dict(self):
        return {'country': self.country, 'cnt': self.cnt}
    
    def to_json(self):
        return json.dumps(self.to_dict())
        

class Pledge:
    def __init__(self, money, description, detail,limit, num_backers):
        self.money = money.encode('utf-8') if money is not None else money
        self.description = description.encode('utf-8') if description is not None else description
        self.detail = detail
        self.limit = limit.encode('utf-8') if limit is not None else limit
        self.num_backers = num_backers.encode('utf-8') if num_backers is not None else num_backers
        
    def __str__(self):
        return '{}, {}, {}'.format(self.money, self.limit, self.num_backers)
        
    def to_dict(self):
        return {'money': self.money, 'description': self.description, 'detail': self.detail, 'limit': self.limit, '#backer': self.num_backers}
        
    def to_json(self):
        return json.dumps(self.to_dict())
        

# parse rewards/pledges html on the right column
def parse_pledges(pledges_sp):
#     len_money_per_pledge = set()
#     len_description_per_pledge = set()
#     len_backer_per_pledge = set()
    pledges = []
    for pledge in pledges_sp:
        money_ = pledge.find_all('span', class_='money')
        # make sure there's only one span for money for each pledge(except live project)
        # len_money_per_pledge.add(len(money_))
        # make pledge without reward have 0 amount, so far only live projects have this option
        money = money_[0].text.strip() if len(money_) else '0'

        
        description = pledge.find_all('div', class_='pledge__reward-description pledge__reward-description--expanded')
        # make sure there's only one div for description for each pledge(except live project)
        # len_description_per_pledge.add(len(description))

        description_text = []
        # remove btn "less" before get the reward description text
        for des in description:
            btn_a = des.find_all(attrs={"role": "button"})
            for a in btn_a:
                a.extract()
            description_text.append(des.text.strip())
        description_text = '\n'.join(description_text)
        # print description_text

        detail = {}
        # pledge detail: estimated time, ship to
        for sp in pledge.find_all('div', class_='pledge__detail'):
            label = sp.find('span', class_='pledge__detail-label').text
            time_tag = sp.find('time')
            if time_tag:
                info = time_tag['datetime']
            else:
                info = sp.find('span', class_='pledge__detail-info').text.strip()
            detail[label] = info

        # backers statues
        backers_stat = pledge.find_all('div', class_='pledge__backer-stats')
        # make sure there's only one div for backers statues for each pledge
        # len_backer_per_pledge.add(len(backers_stat))
        
        # live projects have non-reward pledge, without "limit" and "# backers" div
        limit = backers_stat[0].find('span', class_='pledge__limit') if len(backers_stat) else None
        limit = limit.text.strip() if limit else 'NULL'
        num_backers = backers_stat[0].find('span', class_='pledge__backer-count') if len(backers_stat) else None
        num_backers = num_backers.text.strip() if num_backers else 'NULL'

        pledges.append(Pledge(money, description_text, detail, limit, num_backers).to_dict())

    return pledges

def parse_proj_page(soup, res, debug=False):
    if debug:
        parse_proj_page_debug(soup,res)
        
    backer_html = soup.find_all('div', class_='js-backers_count')
    if backer_html:  # for pledge<goal project, there could be two backer divs
        res['#backer'] = backer_html[0]['data-backers-count'] 
    else:  # for pledge>goal project, parse #backers this way
        for d in soup.find_all('div', class_='mb0'):
            if d.h3 and d.div and d.div.text.strip()=='backers' and 'mb0' in d.h3['class']:
                res['#backer'] = d.h3.text.strip()
            else:
                res['#backer'] = None
    
    # description_container = soup.find('div', class_='description-container')
    # res['description_html'] = description_container.encode('utf-8')
    
    update_html = soup.find('a', class_='js-load-project-updates')
    res['#upd'] = update_html.find('span').text.strip()

    comments_html = soup.find('a', class_='js-load-project-comments')
    res['#comments'] = comments_html['data-comments-count']

    # blog_mention_div = soup.find('div', class_='NS_projects__blog_mentions')
    # res['blog_mention_html'] = blog_mention_div.encode('utf-8')
    
    rewards_div = soup.find('div', class_='js-project-rewards')
    if rewards_div:
        pledges_sp = rewards_div.find_all('div', class_='pledge__info')
        res['#pledges'] = len(pledges_sp)
        pledges = parse_pledges(pledges_sp)
        res['pledges'] = json.dumps(pledges)
    

def parse_proj_page_debug(soup, res):
    
    backer_html = soup.find_all('div', class_='js-backers_count')
    if backer_html:  # for pledge<goal project, there could be two backer divs
        res['len_bckr'] = len(backer_html)
    else:  # for pledge>goal project, parse #backers this way
        for d in soup.find_all('div', class_='mb0'):
            if d.h3 and d.div and d.div.text.strip()=='backers' and 'mb0' in d.h3['class']:
                res['len_bckr'] = 1
            else:
                res['len_bckr'] = 0                
    
    description_container = soup.find_all('div', class_='description-container')
    res['len_des_div'] = len(description_container)
    
    # make sure only one update count (Yes)
    update_html = soup.find_all('a', class_='js-load-project-updates')
    res['len_upd'] = len(update_html)

    # make sure only one comments count (yes)
    comments_html = soup.find_all('a', class_='js-load-project-comments')
    res['len_comments'] = len(comments_html)

    # make sure only one blog mention (yes)
    blog_mention_div = soup.find_all('div', class_='NS_projects__blog_mentions')
    res['len_blog'] = len(blog_mention_div)
    
    # make sure only one rewards list
    rewards_div = soup.find_all('div', class_='js-project-rewards')
    res['len_rew_div'] = len(rewards_div)
    

def parse_creator_bio_page(soup, res, debug=False):
    if debug:
        # make sure there's only one header tag
        header = soup.find_all('header')
        res['len_header'] = len(header)
        # one <p> in <header>
        res['len_header_p'] = len(header[0].find_all('p'))
        # make sure there's only one
        details = soup.find_all('div', class_='creator-bio-details')
        res['len_details'] = len(details)
        # make sure there's only one
        res['len_tipsys'] = len(details[0].find_all('span', 'tipsy_s'))
        # make sure there's only one
        res['len_facebook'] = len(details[0].find_all('div', 'facebook py2 border-bottom f5'))
    
    header_p = soup.find('header').find('p')
    res['bio_loc'] = header_p.text.strip() if header_p else 'NULL'
    
    bio = soup.find(id='bio')
    # this is a truncated version of bio, in case that the author deleted his/her own personal page
    # res['bio'] = bio.find('div', class_='readability').text.strip()
    
    # auto_verified, connected facebook, # created, # backed
    details = bio.find('div', class_='creator-bio-details')
    tipsy_s = details.find('span', 'tipsy_s')
    res['bio_auto_verified'] = tipsy_s['title'] if tipsy_s else 'NULL'
    res['bio_facebook'] = details.find('div', 'facebook py2 border-bottom f5').text.strip()  # value range: Not-connected/N friends/Name/connected
    created_backed = details.find('div', 'created-projects').text
    created, backed = created_backed.split(u'·')
    res['#bio_created'] = created.replace('created','').strip().replace('First','1')
    res['#bio_backed'] = backed.replace('backed', '').strip()
    
    collaborators = soup.find(id='collaborators')
    res['#collaborators'] = len(collaborators.find_all('li')) if collaborators else 0
    websites = soup.find(id='websites')
    res['#websites'] = len(websites.find_all('li')) if websites else 0


def parse_creator_full_page(soup, res, debug=False):
    # user can delete his/her account
    user_exist = soup.find(id='running-board-wrap')
    if not user_exist:
        res['user_deleted'] = 1
        return
    # user account is still active
    res['user_deleted'] = 0
    
    if debug:
        # only 1
        full_bio = soup.find_all('p', class_='mb3 navy-600')
        res['len_full_bio'] = len(full_bio)
        joined = soup.find_all('span', class_='joined')
        res['len_joined'] = len(joined)
        # 1 or 0
        location = soup.find_all('span', class_='location')
        res['len_location'] = len(location)
        badge = soup.find_all('span', class_=['creator-badge', 'repeat-creator-badge'])
        res['len_badge'] = len(badge)

    # each page has one
    # res['bio'] = soup.find('p', class_='mb3 navy-600').text.strip()
    res['profile_joined_time'] = soup.find('span', class_='joined').text.replace('Joined', '').strip()
    
    # there could be none in some pages
    location = soup.find('span', class_='location')
    res['profile_loc'] = location.text.strip() if location else 'NULL'
    badge = soup.find('span', class_=['creator-badge', 'repeat-creator-badge'])
    res['profile_badge'] = badge.text.strip() if badge else 'NULL'




def parse_commnunity_page(soup, res, debug=False):
    
    # no geographic information if the community is small
    small_comm = soup.find('div', class_='community-section__small_community')
    if small_comm:
        res['is_small_community'] = 1
        return
    res['is_small_community'] = 0
    
    if debug:
        # check if len = 1
        cities_div = soup.find_all('div', class_='community-section__locations_cities')
        res['len_cities_div'] = len(cities_div)
        countries_div = soup.find_all('div', class_='community-section__locations_countries')
        res['len_countries_div'] = len(countries_div)
        new_backer_div = soup.find_all('div', class_='new-backers')
        res['len_new_backer_div'] = len(new_backer_div)
        return_backer_div = soup.find_all('div', class_='existing-backers')
        res['len_return_backer_div'] = len(return_backer_div)
        
    
    cities = soup.find('div', class_='community-section__locations_cities').find_all('div', class_='location-list__item js-location-item')
    backer_cities = []
    for c in cities:
        cityname = c.find('div', 'primary-text js-location-primary-text').text.strip()
        countryname = c.find('div', 'secondary-text js-location-secondary-text').text.strip()
        cnt = c.find('div', 'tertiary-text js-location-tertiary-text').text.replace('backers', '').replace('backer', '').strip()
        bc = BackerCity(city=cityname, country=countryname, cnt=cnt)
        backer_cities.append(bc.to_dict())
    res['backer_cities'] = json.dumps(backer_cities)
    
    backer_countries = []
    countries = soup.find('div', class_='community-section__locations_countries').find_all('div', class_='location-list__item js-location-item')
    for c in countries:
        countryname = c.find('div', 'primary-text js-location-primary-text').text.strip()
        cnt = c.find('div', 'tertiary-text js-location-tertiary-text').text.replace('backers', '').replace('backer', '').strip()
        bc = BackerCountry(country=countryname, cnt=cnt)
        backer_countries.append(bc.to_dict())
    res['backer_countries'] = json.dumps(backer_countries)

    res['#new_backer'] = soup.find('div', class_='new-backers').find(class_='count').text.strip()
    res['#return_backer'] = soup.find('div', class_='existing-backers').find(class_='count').text.strip()
    
