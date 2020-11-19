import os
import pandas as pd
import numpy as np
import pronouncing
import re

# helper function for preprocessing mlf files
def preprocess_fave(mlf, mlf_path):
    if mlf != '.DS_Store':
        os.chdir(mlf_path)
        df = pd.read_csv(mlf, sep = " ", header = None, names = ["start","end","arpa","ll","word"])
        # df2: checked for initial and final sp
        if df['word'].iloc[0] == 'sp':  # check for and remove initial sp
            df2=df.drop(df.index[0])
        if df['word'].iloc[-1] == 'sp':  # check for and remove final sp
            df2=df2.drop(df2.index[-1])
        else:
            df2 = df    
        # note removing the rows changed the row indices
        df2.index = range(len(df2)) # re-assining the row index
        df2['diff'] = df2['end'] - df2['start'] # get the time intervals

        return df2

# help function to get syllables
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
        '''https://github.com/dfm/twitterick/blob/master/twitterick/syllabifier.py
        Syllabifies the word, given a language configuration loaded with loadLanguage.
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

class GetFluency():
    def __init__(self):
        print('GetFluency object created...')

    def get_sil(self,df2):
        """
        calculate silent pausing features (sp's)
        """
        #npause_all = df2['word'].value_counts(dropna=False)['sp'] # number of all silent pauses
        subset = df2[(df2['diff'] >=2000000) & (df2['diff'] <=20000000)] # get all invervals within a specified length range
        npause_limit = subset['word'].value_counts(dropna=False)['sp'] # number of silent pauses within specific range (e.g. 0.2 - 2s)
        self.length_pause = subset.loc[subset.word =='sp']['diff'].sum() * 10**(-7)
        mean_pause_length = self.length_pause/npause_limit

        return npause_limit, self.length_pause, mean_pause_length
    
    def get_filler(self, df2):
        """
        calculate filled pausing features
        """
        fillers = df2.loc[df2['word'].isin(['UH','UHM','HM','EH'])]
        nfillers = fillers.shape[0] 
        self.length_fillers = fillers['diff'].sum()* 10**(-7)
        mean_filler_length = self.length_fillers/nfillers

        return nfillers, self.length_fillers, mean_filler_length
    
    def get_rep(self, mlf, trans_dir):
        """
        calculuate repetition and repair features: extract '--' or '-' from transcriptions
        """
        for i in os.listdir(trans_dir):
            if i != '.DS_Store':
                os.chdir(trans_dir)
                filename = re.sub('_','',re.sub('-','', i.strip('\.txt')))
                if filename in mlf:
                    text = open(i,'r').read()
                    nrep1 = len(re.findall(r'[a-z]- ', text))
                    nrep2 = text.count(' -- ')
                    nrep = nrep1 + nrep2

                    return nrep

    def get_mlr(self, df2):
        """
        calculate phonation features: phonation intervals are in between 'sp's' after removing the fillers
        note: sp's are only within the specified range (0.2 - 2s); shorter sps' are subsumed into the correponding phoantion segments
        removing fillers and noises, but keep sp's (in-place): df3
        """
        # removing fillers, pauses, and noises
        i = 0
        while i < len(df2):
            if df2['word'].iloc[i] in ['UH','HM','EH', '{NS}']:
                df2.at[i, 'word'] = 'XXX'
                i += 1
            elif df2['word'].iloc[i] == 'UHM':
                df2.at[i, 'word'] = 'XXX'
                df2.at[i+1, 'word'] = 'XXX' # also removed 'M' in 'UHM'
                i += 1
            else:
                i += 1
        # the new dataframe without fillers, but kept all silent pauses
        df3 = df2[(df2['word'] != 'XXX')]
        df3.index = range(len(df3)) # re-assining the row index
        # remove silent pauses outside specified range (0.2-2s)
        to_remove = df3[(df3['word']=='sp') & ((df3['diff']>20000000) | (df3['diff']<2000000))]
        self.length_pause_invalid = sum(to_remove['diff']) * 10**(-7)
        to_remove_index = list(to_remove.index.values) 
        df4 = df3.drop(df3.index[to_remove_index]) 
        df4.index = range(len(df4)) # re-assining the row index
        # d4: the new dataframe with only the valid sp's 
        
        # calcualte phonation number, duration, and MLR
        # three different cases to calculate number for phonation segments
        if (df4['word'][0] == 'sp') and (df4['word'][len(df4)-1] == 'sp'):
            npho = df4['word'].value_counts(dropna=False)['sp'] - 1
        elif (df4['word'][0] != 'sp') and (df4['word'][len(df4)-1] != 'sp'):
            npho = df4['word'].value_counts(dropna=False)['sp'] + 1
        else:
            npho = df4['word'].value_counts(dropna=False)['sp']
        
        # get total phonation length: total response time - total silent pause time (valid + invalid) + total filler time
        # note: length_pause matched 'df4.loc[df4.word =='sp']['diff'].sum() * 10**(-7)'
        self.length_pho = df2['end'].iloc[-1]* 10**(-7) - self.length_pause - self.length_pause_invalid - self.length_fillers # the length of phonation is the 
        mlr = self.length_pho/npho # mean lengh of runs

        return mlr, df3, df4
    
    def get_speed1(self, df2, df4):
        """
        calculate speed features: removing silent pauses, fillers
        """
        # get speech rate (words/tokens per second/mins) 
        self.token_counts = []
        freq_li = df4['word'].value_counts(dropna=True)
        i = 0
        while i < len(freq_li):
            if freq_li.index[i] != "sp": 
                self.token_counts.append(freq_li[i])
                i += 1
            else:
                i +=1
        self.ntokens = sum(self.token_counts)
        tokens_per_sec = self.ntokens/(df2['end'].iloc[-1] * 10**(-7))
        #tokens_per_min = ntokens/(df2['end'].iloc[-1] * 10**(-7)) * 60
        words_per_sec = len(self.token_counts)/(df2['end'].iloc[-1] * 10**(-7))
        #words_per_min = len(token_counts)/(df2['end'].iloc[-1] * 10**(-7)) * 60
        # denote the denominator (total response time) is based on the response removed of initial silent pauses
        
        return tokens_per_sec, words_per_sec

    def get_speed2(self, df2, df4):    
        # get speech rate (syllables per second/mins)
        arpa_li = []
        df5 = df4[df4['arpa']!='sp']
        df5.index = range(len(df5))
        
        words_series = df5['word'].notnull() # word indicated by 'True'
        words_index= words_series[words_series].index.values # get row indices for words
        i = 0
        while i < len(words_index)-1: 
            arpa_li.append(''.join(df5['arpa'][words_index[i]:words_index[i+1]].to_string(header=False, index=False).split('\n'))) 
            i += 1
        arpa_li.append(''.join(df5['arpa'][words_index[i]:len(df5)].to_string(header=False, index=False).split('\n')))
        
        syll=[pronouncing.syllable_count(str(p)) for p in arpa_li]
        nsyll = sum(syll)
        nsyll_per_sec = nsyll/(df2['end'].iloc[-1] * 10**(-7))
        #nsyll_per_min = nsyll/(df2['end'].iloc[-1] * 10**(-7)) * 60
        ASD = (df2['end'].iloc[-1] * 10**(-7))/nsyll # for seconds
        
        # get articulation rate (denominator is total phonation time)
        tokens_per_sec2 = self.ntokens/self.length_pho
        #tokens_per_min2 = ntokens/length_pho * 60
        words_per_sec2 = len(self.token_counts)/self.length_pho
        #words_per_min2 = len(token_counts)/length_pho * 60
        nsyll_per_sec2 = nsyll/self.length_pho
        #nsyll_per_min2 = 
        ASD2 = self.length_pho/nsyll

        return nsyll_per_sec, ASD, tokens_per_sec2, words_per_sec2, nsyll_per_sec2, ASD2, df5

    def get_speed3(self, df2, df5):
        # get speech rate (syllables per second/mins using syllabifier.py)
        syll_str = ' '.join(list(df5['arpa']))
        syllables = syllabify(language, syll_str)
        nsyll_p2tk = len(syllables)
        nsyll_per_sec_p2tk = len(syllables)/(df2['end'].iloc[-1] * 10**(-7))
        #nsyll2_per_min = nsyll_p2tk/(df2['end'].iloc[-1] * 10**(-7)) * 60
        ASD_p2tk = (df2['end'].iloc[-1] * 10**(-7))/nsyll_p2tk # for seconds
        
        # get articulation rate (denominator is total phonation time) (using syllabifier.py)
        nsyll_per_sec2_p2tk = nsyll_p2tk/self.length_pho
        #nsyll_per_min2 = 
        ASD2_p2tk = self.length_pho/nsyll_p2tk

        return nsyll_per_sec_p2tk, ASD_p2tk, nsyll_per_sec2_p2tk, ASD2_p2tk

class Pronunciation():
    def __init__(self):
        print('Pronunciation object created ...')
    
    def get_ll(self, df2):
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
        
        syll_str = ' '.join(list(df3['arpa']))
        syllables =syllabify(language, syll_str)
        ros = len(syllables)/(df3['end'].iloc[-1] * 10**(-7)) # rate of speech, as nsyll per second
        L6 = L4/ros # L4 normalized by rate of speech
        L7 = L5/ros # L5 normalized by rate of speech

        return L1, L2, L3, L4, L5, L6, L7

class Rhythm():
    def __init__(self):
        print('Rhythm object created ...')
    
    def get_rhythm(self, df2):
        English = {
        'consonants': ['B', 'CH', 'D', 'DH', 'F', 'G', 'HH', 'JH', 'K', 'L', 'M', 'N',
        'NG', 'P', 'R', 'S', 'SH', 'T', 'TH', 'V', 'W', 'Y', 'Z', 'ZH'],
        'vowels': [ 'AA', 'AE', 'AH', 'AO', 'AW', 'AY', 'EH', 'ER', 'EY', 'IH', 'IY', 'OW', 'OY', 'UH', 'UW']}
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

        return percent_v, delta_v, varco_v, rpvi_v, npvi_v, percent_c, delta_c, varco_c, rpvi_c, npvi_c


def main():
    mlf_path = '/Users/mingxi/Desktop/TEMP/DISS/Oral_Defense/sample_mlf/mlf_small'
    for mlf in os.listdir(mlf_path):
        df2 = preprocess_fave(mlf, mlf_path)

        # get fluency features
        fluency = GetFluency()
        npause_limit, length_pause, mean_pause_length = fluency.get_sil(df2)
        nfillers, length_fillers, mean_filler_length = fluency.get_filler(df2)
        nrep = fluency.get_rep(mlf, '/Users/mingxi/Desktop/TEMP/DISS/Original_Transcriptions')
        mlr, df3, df4 = fluency.get_mlr(df2)
        tokens_per_sec, words_per_sec = fluency.get_speed1(df2, df4)
        nsyll_per_sec, ASD, tokens_per_sec2, words_per_sec2, nsyll_per_sec2, ASD2, df5 = fluency.get_speed2(df2, df4)
        nsyll_per_sec_p2tk, ASD_p2tk, nsyll_per_sec2_p2tk, ASD2_p2tk = fluency.get_speed3(df2, df5)
        print(mlf,npause_limit, length_pause, mean_pause_length, nfillers, length_fillers, mean_filler_length, nrep, mlr, tokens_per_sec, words_per_sec, nsyll_per_sec, ASD, tokens_per_sec2, words_per_sec2, nsyll_per_sec2, ASD2, nsyll_per_sec_p2tk, ASD_p2tk, nsyll_per_sec2_p2tk, ASD2_p2tk)

        # get pronunciation features
        pronunciation = Pronunciation()
        L1, L2, L3, L4, L5, L6, L7 = pronunciation.get_ll(df2)
        print(mlf, L1, L2, L3, L4, L5, L6, L7)

        # get rhythm features
        rhythm = Rhythm()
        percent_v, delta_v, varco_v, rpvi_v, npvi_v, percent_c, delta_c, varco_c, rpvi_c, npvi_c = rhythm.get_rhythm(df2)
        print(mlf, percent_v, delta_v, varco_v, rpvi_v, npvi_v, percent_c, delta_c, varco_c, rpvi_c, npvi_c)

if __name__ == "__main__":
    main()