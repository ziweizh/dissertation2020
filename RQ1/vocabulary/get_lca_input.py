from nltk.tokenize import word_tokenize
from nltk.stem import WordNetLemmatizer
from nltk import pos_tag

# get LCA input files
directoryPath='/Users/mingxi/Desktop/TEMP/DISS/Oral_Defense/feature_validation/lca/Cleaned_Transcriptions'
os.chdir(directoryPath)

def tokenizer(text):
    try:  # py3
        all_tokens = [t for t in word_tokenize(text)]
    except UnicodeDecodeError:  # py27
        all_tokens = [t for t in word_tokenize(text.decode('utf-8'))]
    
    return all_tokens

def lemmatization(tokens):
    l = WordNetLemmatizer()
    lemmatized = [l.lemmatize(w) for w in tokens]
    
    return lemmatized

def pos(tokens):
    pos_dict = {}
    tagged = pos_tag(tokens)
    for k, v in tagged:
        pos_dict[k] = v
    
    return pos_dict

for filename in glob.glob( os.path.join(directoryPath, '*') ):
    lemfile = open(filename,"r").read()
    tokens = tokenizer(lemfile)
    lemmas = lemmatization(tokens)
    pos_tags = pos(lemmas)
    lemfile2 = open(filename.strip('.txt')+'_lemfile'+'.txt','w')
    lem_pos = []
    for k, v in pos_tags.items():
        lem_pos.append(k+'_'+v)
    lemfile2.write(' '.join(lem_pos)) 
    lemfile2.close()  

print("generating LCA input files done")

#########################################