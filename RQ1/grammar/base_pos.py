#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sat May 16 00:28:43 2020

@author: mingxi
"""


from nltk.parse import CoreNLPParser

pos_tagger = CoreNLPParser(url='http://localhost:9000', tagtype='pos')

# extract pos tags for the Tedlium corpus
base_text = open('/Users/mingxi/Desktop/TEMP/DISS/Grammar/Base/Base_all.txt','r').read()
base_pos = []
for i in base_text.split('.'):
    base_pos.append(list(pos_tagger.tag((i + '.').split())))

base_pos2 = []
for i in base_pos:
    if i[0][0] != '.':
        for j in i:
            base_pos2.append(j[1]) 
    
out = open('/Users/mingxi/Desktop/TEMP/DISS/Grammar/base_pos.txt','w')
out.write('\n'.join(base_pos2))
out.close()