#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sat May 16 00:28:43 2020

@author: mingxi
"""


from nltk.parse import CoreNLPParser

pos_tagger = CoreNLPParser(url='http://localhost:9000', tagtype='pos')

# extract pos tags for the Tedlium corpus
ted_text = open('/Users/mingxi/Desktop/TEMP/DISS/Grammar/TEDLIUM_release2/stm_processed_final.txt','r').read()
ted_pos = []
for i in ted_text.split('.'):
    ted_pos.append(list(pos_tagger.tag((i.capitalize() + '.').split())))

ted_pos2 = []
for i in ted_pos:
    if i[0][0] != '.':
        for j in i:
            ted_pos2.append(j[1]) 
    
out = open('/Users/mingxi/Desktop/TEMP/DISS/Grammar/ted_pos.txt','w')
out.write('\n'.join(ted_pos2))
out.close()