#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Oct 17 21:15:19 2019

@author: ziweizh
"""

import pandas as pd
import os
import numpy as np

English = {
	'consonants': ['B', 'CH', 'D', 'DH', 'F', 'G', 'HH', 'JH', 'K', 'L', 'M', 'N',
	'NG', 'P', 'R', 'S', 'SH', 'T', 'TH', 'V', 'W', 'Y', 'Z', 'ZH'],
	'vowels': [ 'AA', 'AE', 'AH', 'AO', 'AW', 'AY', 'EH', 'ER', 'EY', 'IH', 'IY', 'OW', 'OY', 'UH', 'UW']}

mlf_path = '/Users/mingxi/Desktop/TEMP/DISS/Oral_Defense/sample_mlf/mlf_small'
for mlf in os.listdir(mlf_path):
    if mlf != '.DS_Store':
        os.chdir(mlf_path)
        df = pd.read_csv(mlf, sep = " ", header = None, names = ["start","end","arpa","ll","word"])
        if df['word'].iloc[0] == 'sp':  # check for and remove initial sp
            df2=df.drop(df.index[0])
        if df['word'].iloc[-1] == 'sp':  # check for and remove final sp
            df2=df2.drop(df2.index[-1])
        else:
            df2 = df    
        # note removing the rows changed the row indices
        df2.index = range(len(df2)) # re-assining the row index
        df2['diff'] = df2['end'] - df2['start'] # get the time intervals
        
        # removing fillers, pauses, and noises
        i = 0
        while i < len(df2):
            if df2['word'].iloc[i] in ['UH','HM','EH','sp', '{NS}']:
                df2.at[i, 'word'] = 'XXX'
                i += 1
            elif df2['word'].iloc[i] == 'UHM':
                df2.at[i, 'word'] = 'XXX'
                df2.at[i+1, 'word'] = 'XXX'
                i += 1
            else:
                i += 1
        # the new dataframe without fillers and pauses
        df3 = df2[(df2['word'] != 'XXX')]
        df3.index = range(len(df3))
        
        consonants = []
        vowels = []
        i = 0
        while i < len(df3):         
            if (df3['arpa'][i].strip(str(0)) in English['vowels']) or (df3['arpa'][i].strip(str(1)) in English['vowels']) or (df3['arpa'][i].strip(str(2)) in English['vowels']):
                vowels.append(df3['diff'][i]* 10**(-7))
                i += 1
            elif df3['arpa'][i] in English['consonants']:
                consonants.append(df3['diff'][i]* 10**(-7))
                i += 1
            else:
                print ("Arpa is neither V nor C!!!")
        #print (mlf)
        #print ('Vowel interval list:', vowels)
        #print ('Consonants interval list:', consonants)
        
        percent_v = sum(vowels)/(sum(vowels)+sum(consonants))
        delta_v = np.std(vowels)
        varco_v = delta_v * 100 / np.mean(vowels)
        rpvi_v_li = []
        i = 0 
        while i < len(vowels)-1:
            rpvi_v_li.append(np.absolute(vowels[i+1] - vowels[i]))
            i +=1
        rpvi_v = sum(rpvi_v_li)/(len(vowels)-1)
        npvi_v_li = []
        i = 0 
        while i < len(vowels)-1:
            npvi_v_li.append(np.absolute((vowels[i+1] - vowels[i])/(vowels[i+1] + vowels[i]/2)))
            i +=1
        npvi_v = 100 * sum(npvi_v_li)/(len(vowels)-1)

        percent_c = sum(consonants)/(sum(vowels)+sum(consonants))
        delta_c = np.std(consonants)
        varco_c = delta_c * 100 / np.mean(consonants)
        rpvi_c_li = []
        i = 0 
        while i < len(consonants)-1:
            rpvi_c_li.append(np.absolute(consonants[i+1] - consonants[i]))
            i +=1
        rpvi_c = sum(rpvi_c_li)/(len(consonants)-1)
        npvi_c_li = []
        i = 0 
        while i < len(consonants)-1:
            npvi_c_li.append(np.absolute((consonants[i+1] - consonants[i])/(consonants[i+1] + consonants[i]/2)))
            i +=1
        npvi_c = 100 * sum(npvi_c_li)/(len(consonants)-1)        
        
        
        print(mlf, percent_v, delta_v, varco_v, rpvi_v, npvi_v)
        print(mlf, percent_c, delta_c, varco_c, rpvi_c, npvi_c)