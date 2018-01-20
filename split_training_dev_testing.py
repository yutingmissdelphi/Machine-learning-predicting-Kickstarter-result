from data_clean import get_int
import pandas as pd
import numpy as np
import json


def rewards_features(df_rewards):
    import re
    df_rewards.pledges = df_rewards.pledges.apply(json.loads)
    df_rewards['num_rewards'] = df_rewards.pledges.apply(len)

    df_rewards['num_with_limits'] = df_rewards.pledges.apply(lambda x: len([p['limit'] for p in x if (p['limit']!='NULL' and p['limit']!='Reward no longer available')]))
    df_rewards['num_with_limits_pcnt'] = df_rewards.apply(lambda x: float(x['num_with_limits'])/ x['num_rewards'] if x['num_rewards'] else 0, axis=1)

    df_rewards['moneys'] = df_rewards.pledges.apply(lambda x: np.array([get_int(re.findall(r'[-+]?\d+(?:,\d+)?[\.]?\d*',p['money'])[0]) for p in x]))

    df_rewards['money_max'] = df_rewards.moneys.apply(lambda x: x.max() if len(x) else 0)
    df_rewards['money_min'] = df_rewards.moneys.apply(lambda x: x.min() if len(x) else 0)
    df_rewards['money_median'] = df_rewards.moneys.apply(lambda x: np.median(x) if len(x) else 0)
    df_rewards['money_std'] = df_rewards.moneys.apply(lambda x: np.std(x) if len(x) else 0)

    df_rewards['money_max_pcnt'] = df_rewards.apply(lambda x: x.money_max*1.0/x.goal, axis=1)
    df_rewards['money_min_pcnt'] = df_rewards.apply(lambda x: x.money_min*1.0/x.goal, axis=1)
    df_rewards['money_median_pcnt'] = df_rewards.apply(lambda x: x.money_median*1.0/x.goal, axis=1)
    df_rewards['money_std_pcnt'] = df_rewards.apply(lambda x: x.money_std*1.0/x.goal, axis=1)
    return df_rewards.drop(['goal','pledges', 'moneys'], axis=1)


# read data
df = pd.read_csv('data/projects_features.csv')
df_rewards = pd.read_csv('data/projects_rewards.csv')
df_loc = pd.read_csv('data/projects_location.csv')
df_disposable_income = pd.read_csv('data/Disposible income.csv')

# get income feature
disposable_income = df_disposable_income.merge(
    df_loc[['id', 'proj_location_display_name']], 
    left_on='City', right_on=['proj_location_display_name'], 
    how='right')[['id', 'Income']]

# rewards feature
rewards_ftr = rewards_features(df_rewards[~df_rewards.pledges.isnull()].merge(df[['id','goal']]))

# merge feature
df = df.merge(rewards_ftr, how='left').merge(disposable_income, how='left')

# drop useless feature
drop_cols = ['goal', 'pledged', 'static_usd_rate', 'usd_pledged', 'category_name', 'spotlight', 
             'has_community', 'has_creator-bio', 'has_creator-full', 'has_proj', 'user_deleted']
df = df.drop(drop_cols, axis=1).set_index('id')

# split train and test
trainset = df.sample(frac=0.8)
testset = df[~df.index.isin(trainset.index)]


(set(testset.index)|set(trainset.index)) == set(df.index)

# save all data
df.to_csv('data/kick_all.csv')
trainset.to_csv('data/kick_training_set.csv')
testset.to_csv('data/kick_testing_set.csv')

# check distribution of splitting
def distribution(df):
    size = df.shape[0]
    category = df.category_parent.value_counts()/size
    category.index = ['category_'+x for x in category.index]
    state = df.state.value_counts()/size
    state.index = ['state_'+x for x in state.index]
    goal_mag = df.goal_magnitude.value_counts()/size
    goal_mag.index = ['goal_magnitude_'+str(x) for x in goal_mag.index]
    return category.append(state).append(goal_mag)


train_category = distribution(trainset)
all_category = distribution(df)
test_category = distribution(testset)

pd.DataFrame([all_category,train_category,test_category]).T.to_csv('data/distribution of all,train and test.csv')


