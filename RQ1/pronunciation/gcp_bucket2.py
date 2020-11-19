#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed May 20 10:08:54 2020

@author: mingxi
"""
# To do: loop through the files

from google.cloud import speech_v1
from google.cloud.speech_v1 import enums
import os
import numpy as np
import pandas as pd

audio_files = os.listdir('/Users/mingxi/Desktop/TEMP/DISS/Pronunciation/GCP/audio_upload')

os.environ["GOOGLE_APPLICATION_CREDENTIALS"]="/Users/mingxi/Desktop/TEMP/DISS/Pronunciation/GCP/My First Project-131ab1c1dd8a.json"
client = speech_v1.SpeechClient()
sample_rate_hertz = 16000
language_code = "en-US"
encoding = enums.RecognitionConfig.AudioEncoding.LINEAR16
config = {
    "sample_rate_hertz": sample_rate_hertz,
    "language_code": language_code,
    "encoding": encoding,
    }

transcription_all = []
confidence_all = []
file_index = []

for i in audio_files:
    storage_uri = "gs://teach_audio/audio_upload/"+i
    audio = {"uri": storage_uri}
    operation = client.long_running_recognize(config, audio)
    response = operation.result()
    
    trans = []
    conf = []
    for result in response.results:
        trans.append(result.alternatives[0].transcript)
        conf.append(result.alternatives[0].confidence)
    
    transcrition = ' '.join(trans)
    confidence = np.mean(conf)
    
    file_index.append(i)
    print(i)
    transcription_all.append(transcrition)
    print(transcrition)
    confidence_all.append(confidence)
    print(confidence)

# write to a csv file
df = pd.DataFrame(list(zip(*[file_index, transcription_all, confidence_all])))
df.columns = ['filename', 'transcription', 'confidence']
df.to_csv('/Users/mingxi/Desktop/TEMP/DISS/Pronunciation/GCP/gcp_output.csv')


