
# coding: utf-8

# In[6]:


from pandas import DataFrame
from vaderSentiment.vaderSentiment import SentimentIntensityAnalyzer
analyzer = SentimentIntensityAnalyzer()
tweets = []
compound = []
positive = []
neutral = []
negative = []
with open('Captain_Marvel_Youtube.txt', 'r', encoding ='UTF8') as f:
    for text in f:
        for tweet in text.split('\n'):
            tweets.append(tweet)
tweets = list(filter(None, tweets))
for i in range(0, len(tweets)):
    compound.append(analyzer.polarity_scores(tweets[i])['compound'])
    positive.append(analyzer.polarity_scores(tweets[i])['pos'])
    neutral.append(analyzer.polarity_scores(tweets[i])['neu'])
    negative.append(analyzer.polarity_scores(tweets[i])['neg'])
df = DataFrame({'Tweet': tweets, 'Compound': compound, 'Positive': positive, 'Neutral': neutral, 'Negative': negative})
df.to_csv('sentiment1.csv', encoding='utf-8')
print(df.describe(exclude='object'))


# In[34]:


len(tweets)


# In[37]:


import re

with open('Captain+Marvel100.txt', 'r', encoding ='UTF8') as f:
    for text in f:
        for tweet in text.split('\n'):
            tweets.append(tweet)

t=[]
c=[]
p1=re.compile('old')
p1_=re.compile('Old')
p2=re.compile('grandma')
p2_=re.compile('Grandma')
p3=re.compile('punch')
p3_=re.compile('Punch')
p4=re.compile('Granny')
p4_=re.compile('granny')
p5=re.compile('Hit')
p5_=re.compile('hit')
p6 = re.compile('soldier')

for x in tweets:
    if re.search(p1,x) or re.search(p5, x) or re.search(p5_,x) or re.search(p1_,x) or re.search(p2,x) or re.search(p3,x) or re.search(p4,x) or re.search(p2_,x) or re.search(p3_,x) or re.search(p4_,x):
        if re.search(p6, s):
            pass
        else:
            t.append(x)
    else:
        c.append(x)


# In[40]:


with open('C:/Users/shin/Desktop/GH/cm_twitter_original_grandma.txt', 'w', encoding ='UTF8') as f:
    for text in set(t):
        f.write("%s\n" % text)


# In[41]:


with open('C:/Users/shin/Desktop/GH/cm_twitter_original_grandma_not.txt', 'w', encoding ='UTF8') as f:
    for text in set(c):
        f.write("%s\n" % text)

