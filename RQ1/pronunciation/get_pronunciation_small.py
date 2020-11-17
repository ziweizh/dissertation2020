# -*- coding: utf-8 -*-
"""
Created on Fri Jul 26 14:43:29 2019

@author: ziweizh
"""

import pandas as pd
import os

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
        
        # get the likelihood score for each word 
        words_series = df3['word'].notnull() # word indicated by 'True'
        words_index= words_series[words_series].index.values # get row indices for words
        ll_words = []
        i = 0
        while i < len(words_index)-1: 
            ll_words.append(sum(df3['ll'].iloc[words_index[i]:words_index[i+1]])) # if the cell is nan, append the corresponding ll
            i += 1
        ll_words.append(sum(df3['ll'].iloc[words_index[i]:len(df3)])) # get the ll sum for the last word
        # ll_words has the ll for each word
        # the number of elements in ll_words matched that of words_index
        
        # The pronunciation features
        L1 = sum(ll_words) # L1:ll for the response (summing over all words)
        L2 = sum(ll_words)/len(words_index) # L2: average likelihoods across all words (tokens acutally since included replications)
        L3 = sum(ll_words)/ len(df3['arpa']) # L3: average likelihoods across all arpabet letters (tokens)
        L4 = sum(ll_words)/ (df3['end'].iloc[-1] * 10**(-7)) # L4: average likelihoods per second; * included fillers and silences
        L5 = sum(ll_words)/ (df3['end'].iloc[-1] * 10**(-7))/len(words_index) # average likelihood density across all words
        
        # use p2tk syllabifier.py to get the syllable durations -- *** is it necessary to know word boundaries???
        syll_str = ' '.join(list(df3['arpa']))
        language = {
        'consonants': ['B', 'CH', 'D', 'DH', 'F', 'G', 'HH', 'JH', 'K', 'L', 'M', 'N',
        'NG', 'P', 'R', 'S', 'SH', 'T', 'TH', 'V', 'W', 'Y', 'Z', 'ZH'],
        'vowels': [ 'AA', 'AE', 'AH', 'AO', 'AW', 'AY', 'EH', 'ER', 'EY', 'IH', 'IY', 'OW', 'OY', 'UH', 'UW'],
        'onsets': ['P', 'T', 'K', 'B', 'D', 'G', 'F', 'V', 'TH', 'DH', 'S', 'Z', 'SH', 'CH', 'JH', 'M',
        'N', 'R', 'L', 'HH', 'W', 'Y', 'P R', 'T R', 'K R', 'B R', 'D R', 'G R', 'F R',
        'TH R', 'SH R', 'P L', 'K L', 'B L', 'G L', 'F L', 'S L', 'T W', 'K W', 'D W',
        'S W', 'S P', 'S T', 'S K', 'S F', 'S M', 'S N', 'G W', 'SH W', 'S P R', 'S P L',
        'S T R', 'S K R', 'S K W', 'S K L', 'TH W', 'ZH', 'P Y', 'K Y', 'B Y', 'F Y',
        'HH Y', 'V Y', 'TH Y', 'M Y', 'S P Y', 'S K Y', 'G Y', 'HH W', '']
        }
        def syllabify(language, word) :
            '''Syllabifies the word, given a language configuration loaded with loadLanguage.
            word is either a string of phonemes from the CMU pronouncing dictionary set
            (with optional stress numbers after vowels), or a Python list of phonemes,
            e.g. "B AE1 T" or ["B", "AE1", "T"]'''

            if type(word) == str :
                word = word.split()

            syllables = [] # This is the returned data structure.

            internuclei = [] # This maintains a list of phonemes between nuclei.

            for phoneme in word :

                phoneme = phoneme.strip()
                if phoneme == "" :
                    continue
                stress = None
                if phoneme[-1].isdigit() :
                    stress = int(phoneme[-1])
                    phoneme = phoneme[0:-1]

                if phoneme in language["vowels"] :
                    # Split the consonants seen since the last nucleus into coda and onset.

                    coda = None
                    onset = None

                    # If there is a period in the input, split there.
                    if "." in internuclei :
                        period = internuclei.index(".")
                        coda = internuclei[:period]
                        onset = internuclei[period+1:]

                    else :
                        # Make the largest onset we can. The 'split' variable marks the break point.
                        for split in range(0, len(internuclei)+1) :
                            coda = internuclei[:split]
                            onset = internuclei[split:]

                            # If we are looking at a valid onset, or if we're at the start of the word
                            # (in which case an invalid onset is better than a coda that doesn't follow
                            # a nucleus), or if we've gone through all of the onsets and we didn't find
                            # any that are valid, then split the nonvowels we've seen at this location.
                            if " ".join(onset) in language["onsets"] \
                            or len(syllables) == 0 \
                            or len(onset) == 0 :
                                break

                    # Tack the coda onto the coda of the last syllable. Can't do it if this
                    # is the first syllable.
                    if len(syllables) > 0 :
                        syllables[-1][3].extend(coda)

                    # Make a new syllable out of the onset and nucleus.
                    syllables.append( (stress, onset, [phoneme], []) )

                    # At this point we've processed the internuclei list.
                    internuclei = []

                elif not phoneme in language["consonants"] and phoneme != "." :
                    raise ValueError("Invalid phoneme: " + phoneme)

                else : # a consonant
                    internuclei.append(phoneme)

            # Done looping through phonemes. We may have consonants left at the end.
            # We may have even not found a nucleus.
            if len(internuclei) > 0 :
                if len(syllables) == 0 :
                    syllables.append( (None, internuclei, [], []) )
                else :
                    syllables[-1][3].extend(internuclei)

            return syllables
        
        syllables =syllabify(language, syll_str)
        nsyll = len(syllables)
        ros = len(syllables)/(df3['end'].iloc[-1] * 10**(-7)) # rate of speech, as nsyll per second
        L6 = L4/ros # L4 normalized by rate of speech
        L7 = L5/ros # L5 normalized by rate of speech
        
        print(L1, L2, L3, L4, L5, L6, L7)
        
        
        # ** did not extract durational feature since the need to train native model 
