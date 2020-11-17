#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue May 19 19:54:27 2020

@author: mingxi
"""
# *** To to : need to match the topic names "stats_scoring_RPlat_original.csv" and the text files of teach materials
#             need to loop through the files 

from sklearn.feature_extraction.text import CountVectorizer
import os
from scipy import spatial
import nltk
from nltk.tokenize import word_tokenize
from nltk.stem import PorterStemmer
import pandas as pd

# load the original data to get the list of students and a list of topics
df = pd.read_csv('/Users/mingxi/Desktop/TEMP/DISS/stats_scoring_RPlat_original.csv')
df = df[['soundname','teach_topic']].drop_duplicates()
df.index = range(len(df))
student = df['soundname']
topic = df['teach_topic']

# create a dictionary to store the (processed) teach material texts 
nltk_stop_words = nltk.corpus.stopwords.words('english')
porter = PorterStemmer()
topic_text = {}
os.chdir('/Users/mingxi/Desktop/TEMP/DISS/Content/TEACH_Materials/text')
for i in os.listdir('/Users/mingxi/Desktop/TEMP/DISS/Content/TEACH_Materials/text'):
    if i != ".DS_Store":
        text = open(i,'r').read()
        text2 = [porter.stem(t.lower()) for t in word_tokenize(text) if t not in nltk_stop_words and t.isalnum()]
        text3 = ' '.join(text2)
        topic_text[i.strip('\.txt')] = text3

# vectorize 1-5 grams for calculating the cosin similarity scores
vectorizer1 = CountVectorizer(ngram_range=(1, 1))
vectorizer2 = CountVectorizer(ngram_range=(2, 2))
vectorizer3 = CountVectorizer(ngram_range=(3, 3))
vectorizer4 = CountVectorizer(ngram_range=(4, 4))
vectorizer5 = CountVectorizer(ngram_range=(5, 5))

# retrieve the clean transriptions for each student and compute the topic-specific cosine simlarity scores
# as well as a holistic cosine similarity score
os.chdir('/Users/mingxi/Desktop/TEMP/DISS/Cleaned_Transcriptions')
out = open('/Users/mingxi/Desktop/TEMP/DISS/Content/topic_specific_cosine_sims.txt','w')
for i in os.listdir('/Users/mingxi/Desktop/TEMP/DISS/Cleaned_Transcriptions'):
    if i != ".DS_Store":    
        resp = open(i,'r').read()
        resp2 = [porter.stem(t.lower()) for t in word_tokenize(resp) if t not in nltk_stop_words and t.isalnum()]
        resp3 = ' '.join(resp2)
        
        teach_topic = topic[student[student == i.strip('\.txt')].index[0]] # because the orders are matched/paired for student and topics
        teach_material = topic_text[teach_topic]
        material_1gram = vectorizer1.fit_transform([teach_material])
        material_2gram = vectorizer2.fit_transform([teach_material])
        material_3gram = vectorizer3.fit_transform([teach_material])
        material_4gram = vectorizer4.fit_transform([teach_material])
        material_5gram = vectorizer5.fit_transform([teach_material])
            
        resp_1gram = vectorizer1.transform([resp3])
        resp_2gram = vectorizer2.transform([resp3])
        resp_3gram = vectorizer3.transform([resp3])
        resp_4gram = vectorizer4.transform([resp3])
        resp_5gram = vectorizer5.transform([resp3])
        
        sim1 = 1 - spatial.distance.cosine(resp_1gram.toarray().tolist()[0], material_1gram.toarray().tolist()[0]) 
        sim2 = 1 - spatial.distance.cosine(resp_2gram.toarray().tolist()[0], material_2gram.toarray().tolist()[0]) 
        sim3 = 1 - spatial.distance.cosine(resp_3gram.toarray().tolist()[0], material_3gram.toarray().tolist()[0]) 
        sim4 = 1 - spatial.distance.cosine(resp_4gram.toarray().tolist()[0], material_4gram.toarray().tolist()[0]) 
        sim5 = 1 - spatial.distance.cosine(resp_5gram.toarray().tolist()[0], material_5gram.toarray().tolist()[0]) 
        # *** issues with resp vector being all zeros
        
        print(sim1, sim2, sim3, sim4, sim5)
        out.write(i+','+str(sim1)+','+str(sim2)+','+str(sim3)+','+str(sim4)+','+str(sim5)+'\n')

out.close()    
    
    
# process the teach materials for all topics (for obtaining the holistic cosine similarity)
all_topics = open('/Users/mingxi/Desktop/TEMP/DISS/Content/all_topics.txt','r').read()
all_topics2 = [porter.stem(t.lower()) for t in word_tokenize(all_topics) if t not in nltk_stop_words and t.isalnum()]
all_topics3 = ' '.join(all_topics2)

os.chdir('/Users/mingxi/Desktop/TEMP/DISS/Cleaned_Transcriptions')
out = open('/Users/mingxi/Desktop/TEMP/DISS/Content/topic_all_cosine_sims.txt','w')
for i in os.listdir('/Users/mingxi/Desktop/TEMP/DISS/Cleaned_Transcriptions'):
    if i != ".DS_Store":    
        resp = open(i,'r').read()
        resp2 = [porter.stem(t.lower()) for t in word_tokenize(resp) if t not in nltk_stop_words and t.isalnum()]
        resp3 = ' '.join(resp2)

        material_1gram = vectorizer1.fit_transform([all_topics3])
        material_2gram = vectorizer2.fit_transform([all_topics3])
        material_3gram = vectorizer3.fit_transform([all_topics3])
        material_4gram = vectorizer4.fit_transform([all_topics3])
        material_5gram = vectorizer5.fit_transform([all_topics3])
            
        resp_1gram = vectorizer1.transform([resp3])
        resp_2gram = vectorizer2.transform([resp3])
        resp_3gram = vectorizer3.transform([resp3])
        resp_4gram = vectorizer4.transform([resp3])
        resp_5gram = vectorizer5.transform([resp3])
        
        sim1 = 1 - spatial.distance.cosine(resp_1gram.toarray().tolist()[0], material_1gram.toarray().tolist()[0]) 
        sim2 = 1 - spatial.distance.cosine(resp_2gram.toarray().tolist()[0], material_2gram.toarray().tolist()[0]) 
        sim3 = 1 - spatial.distance.cosine(resp_3gram.toarray().tolist()[0], material_3gram.toarray().tolist()[0]) 
        sim4 = 1 - spatial.distance.cosine(resp_4gram.toarray().tolist()[0], material_4gram.toarray().tolist()[0]) 
        sim5 = 1 - spatial.distance.cosine(resp_5gram.toarray().tolist()[0], material_5gram.toarray().tolist()[0]) 
        # *** issues with resp vector being all zeros
        
        print(sim1, sim2, sim3, sim4, sim5)
        out.write(i+','+str(sim1)+','+str(sim2)+','+str(sim3)+','+str(sim4)+','+str(sim5)+'\n')

out.close()    


