# -*- coding: utf-8 -*-
"""
Created on Fri Aug  2 10:54:57 2019
Updated on Sun Oct 25 2020

@author: ziweizh
"""

import wave
import contextlib
import os
import shutil 
import re


# 1. load the wav and manual transcription files
audio_dir = '/Users/mingxi/Desktop/TEMP/DISS/Oral_Defense/sample_audio' # location of wav files
text_dir = '/Users/mingxi/Desktop/TEMP/DISS/Oral_Defense/sample_transcription' # location of manual transcriptions for FAVE forced alignment
audios = os.listdir(audio_dir)
if '.DS_Store' in audios:
    audios.remove('.DS_Store')
texts = os.listdir(text_dir)
if '.DS_Store' in texts:
    texts.remove('.DS_Store')


# 2. convert the text transcription into FAVE input format
# get duration of each audio file
audio_dur = []
os.chdir('/Users/mingxi/Desktop/TEMP/DISS/Oral_Defense/sample_audio')
for fname in audios:
    with contextlib.closing(wave.open(fname,'r')) as f:
        audio_dur.append(f.getnframes()// float(f.getframerate()))
# convert to FAVE input format (inplace)
os.chdir('/Users/mingxi/Desktop/TEMP/DISS/Oral_Defense/sample_transcription')
i = 0
while i < len(audios):
    content = open(texts[i], 'r').read()
    f = open(texts[i],'w')
    f.write(texts[i][:7]+'\t'+texts[i].strip('.txt')+'\t'+'0'+'\t'+str(audio_dur[i])+'\t'+content.replace('\n', ' ').replace('\r', ''))
    f.close()    
    i += 1

#############################################################
# Now we can perform the FAAV.py alignment using command line
#############################################################

# 3. Processing the FAVE alignment output: mlf files    
# copy the target mlf files (from the tmp folders) to another directory
path = '/Users/mingxi/Desktop/TEMP/DISS/Oral_Defense/sample_fave_tmp'
os.chdir(path)
tmp_li = []
for i in os.listdir(path):
    if 'tmp' in i:
        tmp_li.append(i)
        
mlf_dir = '/Users/mingxi/Desktop/TEMP/DISS/Oral_Defense/sample_mlf'
for i in tmp_li:
    for j in os.listdir(path+'/'+i):
        if re.search(r'aligned.*\.mlf',j):
            src = j
            os.chdir(path+'/'+i)
            shutil.copy(src, mlf_dir)    
    
# remove the first, second, and last line in each align mlf file
os.chdir(mlf_dir)
for i in os.listdir(mlf_dir):
    if i != '.DS_Store':
        with open(i, 'r') as fin:
            data = fin.read().splitlines(True)
        with open(i, 'w') as fout:
            fout.writelines(data[2:-1])
