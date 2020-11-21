import glob
import math

# helpfer function to buid a dict from corpus
def dict_builder(database_file, number,log): #builds dictionaries from database files
    dict ={}
    
    data_file = database_file.lower().split("\n")
    for entries in data_file:  
        if entries == "":
            continue
        if entries[0] == '#': #ignores first line which contains category information
            continue
            
        entries = entries.split("\t")
        if log == "n": 
            dict[entries[0]]=entries[number]
        if log == "y": 
            if not entries[number] == '0':
                dict[entries[0]]=math.log10(float(entries[number]))

    return dict

# load the corpus
COCA_academic_uni_list = open('COCA_academic_unigram_list.csv', 'rU').read()
COCA_academic_uni_F = dict_builder(COCA_academic_uni_list, 2,"n")

# load the text
indir = "/Users/mingxi/Desktop/TEMP/DISS/Oral_Defense/feature_validation/taales/demo/transcriptions"
inputfile = indir + "/*.txt"
filenames = glob.glob(inputfile)
for filename in filenames:
            text= open(filename, 'rU').read().lower()
            filename_2 = filename.split("/")[-1]
            #This cleans up problematic characters that were killing the program. 
            text = text.replace(".", " ")
            text = text.replace("\n"," ")
            text = text.replace(","," ")
            while "  " in text:
                text = text.replace("  ", " ")

        
            text= text.split(" ") # this is new in 1.4
            
            #Check to make sure target file is not empty:
            nwords2= len(text)
            if nwords2 == 0:
                continue
            
            clean_text = []
            
            punctuation = (".","!","?",",", ":",";", "'",'"')
            
            #this is for individual file output:
            ind_out_dict = {}
            
            for word in text:
                if len(word) < 1: #new in 1.4
                    continue
                if len(word)==1 and word in punctuation:
                    continue
                if word[-1] in punctuation:
                    word = word[:-1]
                if word[0] in punctuation:
                    word = word[1:]
                clean_text.append(word)
            
            coca_text=[]
            
            for words in clean_text:
                if "'" in words:
                        #print words
                        words = words.replace("n't"," n't")
                        #print words
                        words = words. split(" ")
                        #print words
                        #words = re.sub("'", " '",words)
                        for items in words:
                                coca_text.append(items)
                else: coca_text.append(words)

# main functions to get the index
def list_dict_builder(database_file):
    dict ={}
    
    data_file = open(database_file, "rU").read().lower().split("\n")
    
    for entries in data_file:
        if entries[0] == "#":
            continue
        entries = entries.split("\t")
        dict[entries[0]]=entries[1:]
    return dict

word_list_dict = list_dict_builder('master_word_list.txt')

#This function deals with denominator issues that can kill the program:
def safe_divide(numerator, denominator):
    if denominator == 0:
        index = 0
    else: index = numerator/denominator
    return index

# lemma list
lemma_list = open('e_lemma_lower_clean.txt','rU').read().split("\n")
lemma_dict = {}

for line in lemma_list:
    #ignores first lines
    if line[0] is '#':
        continue
    #allows use of each line:
    entries=line.split("\t")
    #creates dictionary entry for each word in line:
    for word in entries:
        lemma_dict[word] = entries[0]

def DataDict_counter(in_text, data_dict,types,null_item,index_dict, ind_out, index_list,index_name):
    #### this is the normal TAALES procedure ####
    counter = 0
    sum_counter = 0

    if types == "cw":
        text = []
        for words in in_text:
            if words in word_list_dict["fw_stop_list"]:
                continue
            else: 
                text.append(words)
        in_text = text

    if types == "fw":
        text = []
        for words in in_text:
            if words not in word_list_dict["fw_stop_list"]:
                continue
            else: 
                text.append(words)
        in_text = text			

    for word in in_text:
        if word in data_dict and data_dict[word] != null_item:
            counter+=1
            sum_counter+=float(data_dict[word])
        else:
            if word in lemma_dict and lemma_dict[word] in data_dict and data_dict[lemma_dict[word]] != null_item:
                counter+=1
                sum_counter+=float(data_dict[lemma_dict[word]])
    index = safe_divide(sum_counter,counter)
    print('COCA_Academic_Frequency_AW: ', index, 'is calculated by summing the frequency scores from COCA: ', sum_counter, 'and divde by the # of words in text that also appeared in COCA: ', counter)
    index_list.append(index)
    header_list = ["Filename", "Word Count"]
    header_list.append(index_name)			
    #new in 2.0.6
    index_coverage = safe_divide(counter,len(in_text))
    index_coverage_list = []
    index_coverage_list.append(index_coverage)
    index_coverage_header_list = ["Filename", "cw_percentage","fw_percentage"]
    index_coverage_header_list.append(index_name)


# main command to obtain the index
ind_out_dict = {}
#self.ind_out = tk.Checkbutton(self.secondframe, text="Include Individual Item Output?", variable=self.ind_out_var,background = "#BFEAA3")
ind_out = []
index_list = []
DataDict_counter(coca_text,COCA_academic_uni_F,"aw","0",ind_out_dict,ind_out,index_list,"COCA_Academic_Frequency_AW")
