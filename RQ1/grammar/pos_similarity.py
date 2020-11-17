#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed May 13 13:38:55 2020

@author: Ziwei
"""

from nltk.parse import CoreNLPParser
#from sklearn.feature_extraction.text import CountVectorizer
from sklearn.feature_extraction.text import CountVectorizer
import os
from scipy import spatial


# vectorize the pos sequence
vectorizer1 = CountVectorizer(ngram_range=(1, 1))
vectorizer2 = CountVectorizer(ngram_range=(2, 2))
vectorizer3 = CountVectorizer(ngram_range=(3, 3))
vectorizer4 = CountVectorizer(ngram_range=(4, 4))
vectorizer5 = CountVectorizer(ngram_range=(5, 5))

ted_pos_text = open('/Users/mingxi/Desktop/TEMP/DISS/Grammar/ted_pos.txt','r').read()
ted_1gram_vec = vectorizer1.fit_transform([ted_pos_text])
#print(vectorizer1.get_feature_names())
ted_2gram_vec = vectorizer2.fit_transform([ted_pos_text])
#print(ted_2gram_vec.shape)
ted_3gram_vec = vectorizer3.fit_transform([ted_pos_text])
#print(ted_3gram_vec.shape)
ted_4gram_vec = vectorizer4.fit_transform([ted_pos_text])
#print(ted_4gram_vec.shape)
ted_5gram_vec = vectorizer5.fit_transform([ted_pos_text])
#print(ted_5gram_vec.shape)

# obtain the cosine similariy score for each TEACH response
pos_tagger = CoreNLPParser(url='http://localhost:9000', tagtype='pos')
out = open('/Users/mingxi/Desktop/TEMP/DISS/Grammar/pos_sim_ted.txt','w')
os.chdir('/Users/mingxi/Desktop/TEMP/DISS/Cleaned_Transcriptions')
teach = os.listdir('/Users/mingxi/Desktop/TEMP/DISS/Cleaned_Transcriptions')
soundname = []
for i in teach:
    if i != '.DS_Store': 
        resp = open(i,'r').read()
        pos = list(pos_tagger.tag(resp.split()))
        teach_pos = []
        for j in pos:
            teach_pos.append(j[1]) 
        
        teach_pos_vec1 = vectorizer1.transform([' '.join(teach_pos)])
        sim1 = 1 - spatial.distance.cosine(teach_pos_vec1.toarray().tolist()[0], ted_1gram_vec.toarray().tolist()[0])                              
        #print(sim1)
        
        teach_pos_vec2 = vectorizer2.transform([' '.join(teach_pos)])
        sim2 = 1 - spatial.distance.cosine(teach_pos_vec2.toarray().tolist()[0], ted_2gram_vec.toarray().tolist()[0])                              
        #print(sim2)
        
        teach_pos_vec3 = vectorizer3.transform([' '.join(teach_pos)])
        sim3 = 1 - spatial.distance.cosine(teach_pos_vec3.toarray().tolist()[0], ted_3gram_vec.toarray().tolist()[0])                              
        #print(sim3)
        
        teach_pos_vec4 = vectorizer4.transform([' '.join(teach_pos)])
        sim4 = 1 - spatial.distance.cosine(teach_pos_vec4.toarray().tolist()[0], ted_4gram_vec.toarray().tolist()[0])                              
        #print(sim4)
        
        teach_pos_vec5 = vectorizer5.transform([' '.join(teach_pos)])
        sim5 = 1 - spatial.distance.cosine(teach_pos_vec5.toarray().tolist()[0], ted_5gram_vec.toarray().tolist()[0])                              
        #print(sim5)
        print(sim1, sim2, sim3, sim4, sim5, '\n')
        soundname.append(i)
        out.write(i+','+str(sim1)+','+str(sim2)+','+str(sim3)+','+str(sim4)+','+str(sim5)+','+'\n')

    
    