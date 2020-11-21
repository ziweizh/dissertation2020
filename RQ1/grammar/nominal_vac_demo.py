from collections import Counter
import xml.etree.cElementTree as ET
import glob
import math

def safe_divide(numerator, denominator):
		if denominator == 0:
			index = 0
		else: index = numerator/denominator
		return index

def dict_counter(dictionary, structure, denominator):
		n_structure = dictionary[structure]
		index_count = n_structure
		if isinstance(denominator, int) == True:
			denom_count = denominator
		else: denom_count = len(denominator)

		index = safe_divide(index_count, denom_count)
	
		return index

def dict_builder(database_file): #builds dictionaries from database files
    lemma_freq_dict ={}
    construction_freq_dict = {}
    contingency_dict ={}	
    for entries in database_file:  
        entries = entries.split("\t")
        if len(entries)<2:
            continue
        lemma_freq_dict[entries[0]]=entries[2]
        construction_freq_dict[entries[1]]=entries[3]
        combined_key = entries[0] + "\t" + entries[1]
        contingency_dict[combined_key] = entries[4:]

    return_list = [lemma_freq_dict,construction_freq_dict,contingency_dict]

    return return_list

def ratio_compiler(dict, list): #compiles ratios for collexeme attracted/repelled
    positive_count=0
    negative_count=0

    for items in list:
        if items in dict:
            if dict[items][1] == "Attracted": positive_count+=1
            if dict[items][1] == "Infinity": positive_count+=1
            if dict[items][1] == "Repelled": negative_count+=1
            if dict[items][1] == "Neg_Infinity": negative_count+=1
    return safe_divide(positive_count,negative_count)

def contingency_database_counter(dict, list, number, stdev = "no", log = "no"):
		var_list = []
		counter=0
		n_counter=0
		for items in list:
			#variable = items[0]+"\t"+items[1]
			if items in dict:
				if log == "yes":
					counter+= math.log10(float(dict[items][number]))
				else:
					counter+= float(dict[items][number])
				if stdev == "yes":
					var_list.append(float(dict[items][number]))
				n_counter+=1
		if stdev == "no":
			return safe_divide(counter,n_counter)
		if stdev == "yes":
			return var_list

def simple_database_counter_log(dict, list, number):
    counter=0
    n_counter=0
    for items in list:
        variable = items.split("\t")
        key = variable[number]
        if key in dict:
            ##print float(dict[key])
            counter+= math.log10(float(dict[key]))
            n_counter+=1
    return safe_divide(counter,n_counter)

# load the xml files
p_files_list = glob.glob("/Users/mingxi/Desktop/TEMP/DISS/Oral_Defense/feature_validation/taasc/demo/results_mod_parsed_small/*.xml") # Create a list of all files in target folder
for files in p_files_list: #iterate through files
    nwords = 0
    nsent = 0

    tree = ET.ElementTree(file=files) #The file is opened by the XML parser

    punctuation = ". , ? ! ) ( % / - _ -LRB- -RRB- SYM ".split(" ")
    noun_tags = "NN NNS NNP NNPS VBG".split(" ") #note that VBG is included because this list is only used for looking at dependents that will be a nominal
    verb_tags = "VB VBZ VBP VBD VBN VBG".split(" ") #This is a list of verb tags
    nominals = "NN NNP NNPS NNS PRP PRP$ CD DT".split(" ")
    adjectives = "JJ JJR JJS".split(" ")
    verbs = "VB VBZ VBP VBD VBN VBG".split(" ")
    other = "RB ".split(" ")
    noun_mod = ["amod", "appos", "det", "goeswith", "mwe", "nn", "num","poss", "cop", "advmod", "advcl", "rcmod","vmod"] #note: cop is thrown in for convenience; #advmod and advcl added in .8.5 , "advmod", "advcl"
    exclusions = "aux auxpass nsubj dobj iobj amod"
    single_quotes = "u\u2018 u\u2019 u'\u02BC'".split(" ")

    ### For Phrase Complexity ###
    nsent = 0
    all_nominal_deps = []
    all_nominal_deps_NN = []

    all_nominals = []
    all_nsubj = []
    all_nsubj_pass =[]
    all_agents = []
    all_dobj = []
    all_pobj = []
    all_iobj = []
    all_ncomps =[]

    all_nominals_NN = []
    all_nsubj_NN = []
    all_nsubj_pass_NN = []
    all_agents_NN = []
    all_dobj_NN = []
    all_pobj_NN = []
    all_iobj_NN = []
    all_ncomps_NN = []

    nsubj_deps_list = []
    nsubj_pass_deps_list = []
    agents_deps_list = []
    dobj_deps_list = []
    pobj_deps_list = []
    iobj_deps_list = []
    ncomp_deps_list = []

    nsubj_deps_list_NN = []
    nsubj_pass_deps_list_NN = []
    agents_deps_list_NN = []
    dobj_deps_list_NN = []
    pobj_deps_list_NN = []
    iobj_deps_list_NN = []
    ncomp_deps_list_NN = []

    all_nominals_stdev_list = []
    nsubj_stdev_list = []
    nsubj_pass_stdev_list = []
    agents_stdev_list = []
    dobj_stdev_list = []
    pobj_stdev_list = []
    iobj_stdev_list = []
    ncomp_stdev_list = []

    all_nominals_stdev_list_NN = []
    nsubj_stdev_list_NN = []
    nsubj_pass_stdev_list_NN = []
    agents_stdev_list_NN = []
    dobj_stdev_list_NN = []
    pobj_stdev_list_NN = []
    iobj_stdev_list_NN = []
    ncomp_stdev_list_NN = []

    all_PP = []
    nominal_PP = []
    nominal_PP_NN = []
    phrase_constructicon = []
###for Phrase Complexity

###for Clause Complexity and Sophistication

    constructicon = [] #holder for the context-free VACs
    prep_constructicon = []
    verb_constructicon = [] #holder for the verb-form sensitive VACs
    lemma_constructicon = [] #holder for the lemma-sensitive VACs
    lemma_constructicon_no_vcop = [] #holder for non-copular constructions
    lemma_constructicon_aux = []#
    prep_lemma_contructicon = []#
    constructicon_database = []#

#### THE NEXT SECTION CONVERTS THE TREE TO AN APPROXIMATION OF -makeCopulaHead #####
    for sentences in tree.iter("sentence"):
        nsent +=1
        phrase_sentence = []

        noun_list = []
        pronoun_list = []
        for tokens in sentences.iter("token"):
            phrase_sentence.append(tokens[0].text)
            if tokens[4].text in punctuation:
                continue
            #if tokens[4].text is not "punct":
            nwords += 1
            if tokens[4].text in noun_tags:
                noun_list.append(tokens.get("id"))
            if tokens[4].text == "PRP":
                pronoun_list.append(tokens.get("id"))
    
        cop_list = [] #list of copular dependency relationships in sentences (tuples)
        cop_list_simple = []
                
        for deps in sentences.iter("dependencies"): #iterates through dependencies
                                
            if deps.get('type') == "collapsed-ccprocessed-dependencies": #only iterate through cc-processed-dependencies
                                                                         # for cc-processed-dependecies: https://stackoverflow.com/questions/48058189/difference-between-dependenciesbasic-and-enhanced-from-stanford-corenlp
                for dependencies in deps.iter("dep"): # iterate through the dependencies

                    if dependencies.get("type") == "cop": #if the type is copular...

                        cop_list.append((dependencies[0].get("idx"),dependencies[0].text,dependencies[1].get("idx"),dependencies[1].text)) #this stores the governor idx and the governor text, and then the dep idx and dep text as a tuple
                        cop_list_simple.append(dependencies[1].get("idx")) #this should be the id for the copular_verb
                
            else: sentences.remove(deps) # this does not get rid of collapsed dependencies. Not sure why.

        
        for entries in cop_list:
            ##print entries
            comp_check = "no"
            for dependencies in deps.iter("dep"):
                if dependencies.get("type") == "cop" and dependencies[0].get("idx") == entries[0]: #if the dependency is copular and the item is the one we are concerned with in this iteration:
                    for word in sentences.iter("token"): #iterate through tokens to find the pos tag
                        ##print word[0].text
                        if word.get("id") == entries[0]:
                            pos = word[4].text
                            #nom_comp_position = word.get("id")
                            ##print pos

                    if pos in nominals: #set appropriate relationship (this may be problematic for finite versus non-finite complements)
                        dependencies.set("type", "ncomp")
                        comp_check = "yes"
                    if pos in adjectives:
                        dependencies.set("type", "acomp")
                        comp_check = "yes"
                    if pos in verbs:
                        dependencies.set("type", "vcomp")
                    if pos in other:
                        dependencies.set("type", "other")

                    dependencies[0].set("idx", entries[2]) #set the governor as the cop verb
                    dependencies[0].text = entries[3] #set the governor as the cop verb
                    dependencies[1].set("idx", entries[0]) #set the dependent as the complement
                    dependencies[1].text = entries[1] #set the dependent as the complement

                    continue # if this fixed the comp, continue to the next dependency

                if dependencies.get("type") not in noun_mod: #if the dependency isn't one that would only work for an nominal (this may need tweaking):

                    if dependencies.get("type") != "tmod" and comp_check == "yes":
                        continue
                
                    if dependencies[0].get("idx") == entries[0]: #if the governor is the previous cop governor - change to cop
                        dependencies[0].set("idx", entries[2]) #changes idx
                        dependencies[0].text = entries[3]	#changes text

                    if dependencies[1].get("idx") == entries[0]: # if the dependent is the previous cop governor - change to cop
                        dependencies[1].set("idx", entries[2]) #changes idx
                        dependencies[1].text = entries[3] #changes text
                    
### END COPULA CONVERSION SECTION ###

### Begin Clause Complexity Section ###
			
        token_store = [] # This will be a holder of tuples for id, word, lemma, pos
        sentence = [] # stores all of the words so sentence can be stored
        #pos = [] #stores POS information so that it can be easily retrieved later
        verbs = []
        excluded_verbs = []
        gerunds = []
        infinitives = []
        main_verbs = []

        if len(list(sentences.iter("token"))) > 100:
            ##print files, " Sentence_skipped"
            continue

        for tokens in sentences.iter("token"):
            token_store.append((tokens.get("id"),tokens[0].text.lower(), tokens[1].text, tokens[4].text))
            ##print token_store
            sentence.append(tokens[0].text) #this is word
            #pos.append(tokens[4].text) #this is POS
            #if tokens[4].text in verb_tags:
            #	verbs.append(tokens.get("id"))
        ##print verbs
        ##print token_store

        inf = "no"
        for items in token_store:
            if items[3] in verb_tags:
                for dependents in sentences.iter("dependencies"):
                    if dependents.get("type") == "collapsed-ccprocessed-dependencies":
                        for dep in dependents.iter("dep"):
                            if dep[1].get("idx") == items[0] and dep.get("type") in exclusions:
                                excluded_verbs.append(items[0]) #adds id to excluded verbs
            if items[3] == "VBG":
                gerunds.append(items[0]) #adds id to gerunds (actually any -ing verb)

            if items[3] == "VB" and inf =="yes":
                infinitives.append(items[0])

            if items[3] == "TO":
                inf = "yes"
            else: inf = "no"	

        for items in token_store:
            if items[0] in excluded_verbs:
                #print "excluded verb", items[0]
                continue
            if items[3] in verb_tags:
                main_verbs.append(items)
    
        #print main_verbs
        #print token_store
        for items in main_verbs:
            ##print "Main_Verb: ", items
            ###Differentiates between copular verbs and non-copular verbs###
            #print "cop list: ", cop_list_simple
            if items[0] in cop_list_simple:
                verb_type = "vcop"
            else: verb_type = "v" 

            ###cleans up some errors that were causing program to crash:##
            verb_form = items[2]
            if u'\xa0' in verb_form:
                verb_form = verb_form.replace(u'\xa0',' ')
            for apostrophe in single_quotes:
                if apostrophe in verb_form:
                    verb_form = verb_form.replace(apostrophe,"'")
            if "-" in verb_form:
                verb_form = verb_form.replace("-","_")


            VAC = [[int(items[0]), verb_type, verb_form]] #format ID, v or v_cop, lemma form

            for dependencies in sentences.iter("dependencies"):
                if dependencies.get("type") == "collapsed-ccprocessed-dependencies":
                    for dependents in dependencies.iter("dep"):
                        if dependents[0].get("idx") == items[0]:

                            dependent_type = dependents.get("type") #this allows the program to fix the copula error - nominal complements are now called "ncomp"
                            dependent_id = int(dependents[1].get("idx"))
                            dependent_form = dependents[1].text
                
                            if dependent_type == "punct":
                                continue
                
                            if dependent_type == "xcomp" and token_store[(int(dependents[1].get("idx"))-1)][3] in nominals:
                                dependent_type = "ncomp"

                            if dependent_type == "aux" and token_store[(int(dependents[1].get("idx"))-1)][3] == "MD":
                                dependent_type = "modal"

                            VAC.append([dependent_id,dependent_type, dependent_form])

            #print infinitives
            VAC = sorted(VAC, key = lambda x:int(x[0]))
            auxilliaries = ["aux", "auxpass", "modal"]
            pre_simple_VAC = []
            simple_VAC = []
            complex_VAC = []
            prep_VAC = []
            simple_VAC_aux = []
        
            #print VAC
        
            for item in VAC:
                simple_VAC_aux.append(item[1])
                if item[1] not in auxilliaries:
                    pre_simple_VAC.append(item)

            #print len(pre_simple_VAC), pre_simple_VAC

            if len(pre_simple_VAC) < 2 and str(pre_simple_VAC[0][0]) in gerunds:
                #print "g skip"
                continue

            if len(pre_simple_VAC) < 2 and str(pre_simple_VAC[0][0]) in infinitives:
                #print "skip"
                continue

            if len(pre_simple_VAC) < 2 and pre_simple_VAC[0][2] == "be":
                #print "be skip"
                continue

            for item in pre_simple_VAC:
                simple_VAC.append(item[1])
                complex_VAC.append("_".join([item[1],item[2]]))
                if "prep" in item[1] and "prepc" not in item[1]:
                    prep_VAC.append("prep")
                else:
                    prep_VAC.append(item[1])
            ####
            simple_VAC_string = "-".join(simple_VAC).lower()
        
            #print simple_VAC_string
        
            complex_VAC_string = "-".join(complex_VAC).lower()
            #print complex_VAC_string
            if "-v_be-" in complex_VAC_string:
                complex_VAC_string = complex_VAC_string.replace("-v_be-", "-vcop_be-")
                simple_VAC_string = simple_VAC_string.replace("-v-","-vcop-")
                #print complex_VAC_string
        
            lemm_entry = verb_form.lower() + "\t" + simple_VAC_string
            lemm_entry_aux = verb_form.lower() + "\t" + "-".join(simple_VAC_aux).lower()
            lemm_prep_entry = verb_form.lower() + "\t" + "-".join(prep_VAC).lower()
        
            prep_constructicon.append("-".join(prep_VAC).lower())
            constructicon.append("-".join(simple_VAC).lower())
            #lemm_entry = lemm_entry.lower()
            lemma_constructicon.append(lemm_entry)
            if "vcop" not in lemm_entry:
                lemma_constructicon_no_vcop.append(lemm_entry) # for contingency counts
            lemma_constructicon_aux.append(lemm_entry_aux)
            prep_lemma_contructicon.append(lemm_prep_entry)

    ###Clause Complexity Section ###


##### Phrase Complexity #######    
        nsubj_list = []
        nsubj_pass_list = []
        agents_list = []
        dobj_list = []
        pobj_list = []
        iobj_list = []
        ncomp_list = []

        all_nominals_stdev_dict = {}
        nsubj_stdev_dict = {}
        nsubj_pass_stdev_dict = {}
        agents_stdev_dict = {}
        dobj_stdev_dict = {}
        pobj_stdev_dict = {}
        iobj_stdev_dict = {}
        ncomp_stdev_dict = {}

        all_nominals_stdev_dict_NN = {}
        nsubj_stdev_dict_NN = {}
        nsubj_pass_stdev_dict_NN = {}
        agents_stdev_dict_NN = {}
        dobj_stdev_dict_NN = {}
        pobj_stdev_dict_NN = {}
        iobj_stdev_dict_NN = {}
        ncomp_stdev_dict_NN = {}

        ### This is the dictionary for the examples: ##
        nominals_const_dict = {}


        for dependencies in sentences.iter("dependencies"):
            if dependencies.get("type") == "collapsed-ccprocessed-dependencies":
                for dependents in dependencies.iter("dep"):
                    #if dependents.get("type") == "xcomp" and dependents[1].get("idx") in noun_list:

                    if dependents.get("type") == "nsubj":
                        ##print "dependent :", dependents.get("type"), " word: ", dependents[1].text
                        nsubj_list.append(dependents[1].get("idx")) #add idx of nsubj to list for later retrieval
                        nsubj_stdev_dict[dependents[1].get("idx")] = 0 #create dict for standard deviation
                        nominals_const_dict[dependents[1].get("idx")] = [[dependents[1].get("idx"),dependents.get("type"), dependents[1].text]] #create dict for construction example
                        all_nominals_stdev_dict[dependents[1].get("idx")] = 0
                        all_nominals.append(dependents[1].text)
                        all_nsubj.append(dependents[1].text)
                        if dependents[1].get("idx") not in pronoun_list:
                            nsubj_stdev_dict_NN[dependents[1].get("idx")] = 0
                            all_nominals_stdev_dict_NN[dependents[1].get("idx")] = 0
                            all_nominals_NN.append(dependents[1].text)
                            all_nsubj_NN.append(dependents[1].text)						
        
                    if dependents.get("type") == "nsubjpass":
                        ##print "dependent :", dependents.get("type"), " word: ", dependents[1].text
                        nsubj_pass_list.append(dependents[1].get("idx"))
                        nsubj_pass_stdev_dict[dependents[1].get("idx")] = 0
                        nominals_const_dict[dependents[1].get("idx")] = [[dependents[1].get("idx"),dependents.get("type"), dependents[1].text]] #create dict for construction example
                        all_nominals_stdev_dict[dependents[1].get("idx")] = 0
                        all_nominals.append(dependents[1].text)
                        all_nsubj_pass.append(dependents[1].text)
                        if dependents[1].get("idx") not in pronoun_list:
                            nsubj_pass_stdev_dict_NN[dependents[1].get("idx")] = 0
                            all_nominals_stdev_dict_NN[dependents[1].get("idx")] = 0
                            all_nsubj_pass_NN.append(dependents[1].get("idx"))
                            all_nominals_NN.append(dependents[1].text)

                    if dependents.get("type") == "agent":
                        ##print "dependent :", dependents.get("type"), " word: ", dependents[1].text
                        agents_list.append(dependents[1].get("idx"))
                        agents_stdev_dict[dependents[1].get("idx")] = 0
                        nominals_const_dict[dependents[1].get("idx")] = [[dependents[1].get("idx"),dependents.get("type"), dependents[1].text]] #create dict for construction example
                        all_nominals_stdev_dict[dependents[1].get("idx")] = 0
                        all_nominals.append(dependents[1].text)
                        all_agents.append(dependents[1].text)
                        if dependents[1].get("idx") not in pronoun_list:
                            agents_stdev_dict_NN[dependents[1].get("idx")] = 0
                            all_nominals_stdev_dict_NN[dependents[1].get("idx")] = 0
                            all_agents_NN.append(dependents[1].get("idx"))
                            all_nominals_NN.append(dependents[1].text)
                                    
                    if dependents.get("type") == "dobj":
                        ##print "dependent :", dependents.get("type"), " word: ", dependents[1].text
                        dobj_list.append(dependents[1].get("idx"))
                        dobj_stdev_dict[dependents[1].get("idx")] = 0
                        nominals_const_dict[dependents[1].get("idx")] = [[dependents[1].get("idx"),dependents.get("type"), dependents[1].text]] #create dict for construction example
                        all_nominals_stdev_dict[dependents[1].get("idx")] = 0
                        all_nominals.append(dependents[1].text)
                        all_dobj.append(dependents[1].text)
                        if dependents[1].get("idx") not in pronoun_list:
                            dobj_stdev_dict_NN[dependents[1].get("idx")] = 0
                            all_nominals_stdev_dict_NN[dependents[1].get("idx")] = 0
                            all_nominals_NN.append(dependents[1].text)
                            all_dobj_NN.append(dependents[1].text)

                    if dependents.get("type") == "pobj" or "prep" in dependents.get("type"):
                        ##print "dependent :", dependents.get("type"), " word: ", dependents[1].text
                        pobj_list.append(dependents[1].get("idx"))
                        pobj_stdev_dict[dependents[1].get("idx")] = 0
                        nominals_const_dict[dependents[1].get("idx")] = [[dependents[1].get("idx"),"pobj", dependents[1].text]] #create dict for construction example
                        all_nominals_stdev_dict[dependents[1].get("idx")] = 0
                        all_nominals.append(dependents[1].text)#check this. Problem here?
                        all_pobj.append(dependents[1].text)#check this. Problem here?
                        if dependents[1].get("idx") not in pronoun_list:
                            pobj_stdev_dict_NN[dependents[1].get("idx")] = 0
                            all_nominals_stdev_dict_NN[dependents[1].get("idx")] = 0
                            all_nominals_NN.append(dependents[1].text)
                            all_pobj_NN.append(dependents[1].text)


                    if dependents.get("type") == "iobj":
                        ##print "dependent :", dependents.get("type"), " word: ", dependents[1].text
                        iobj_list.append(dependents[1].get("idx"))
                        iobj_stdev_dict[dependents[1].get("idx")] = 0
                        nominals_const_dict[dependents[1].get("idx")] = [[dependents[1].get("idx"),dependents.get("type"), dependents[1].text]] #create dict for construction example
                        all_nominals_stdev_dict[dependents[1].get("idx")] = 0
                        all_nominals.append(dependents[1].text)
                        all_iobj.append(dependents[1].text)
                        if dependents[1].get("idx") not in pronoun_list:
                            iobj_stdev_dict_NN[dependents[1].get("idx")] = 0
                            all_nominals_stdev_dict_NN[dependents[1].get("idx")] = 0
                            all_nominals_NN.append(dependents[1].text)
                            all_iobj_NN.append(dependents[1].text)

                    if dependents.get("type") == "ncomp":
                        ##print "dependent :", dependents.get("type"), " word: ", dependents[1].text
                        ncomp_list.append(dependents[1].get("idx"))
                        ncomp_stdev_dict[dependents[1].get("idx")] = 0
                        nominals_const_dict[dependents[1].get("idx")] = [[dependents[1].get("idx"),dependents.get("type"), dependents[1].text]] #create dict for construction example
                        all_nominals_stdev_dict[dependents[1].get("idx")] = 0
                        all_nominals.append(dependents[1].text)
                        all_ncomps.append(dependents[1].text)
                        if dependents[1].get("idx") not in pronoun_list:
                            ncomp_stdev_dict_NN[dependents[1].get("idx")] = 0
                            all_nominals_stdev_dict_NN[dependents[1].get("idx")] = 0
                            all_nominals_NN.append(dependents[1].text)
                            all_ncomps_NN.append(dependents[1].text)

                    if "prep" in dependents.get("type"):
                        all_PP.append(dependents[1].text)
                        ##print "prep"
        
        for dependencies in sentences.iter("dependencies"):
            if dependencies.get("type") == "collapsed-ccprocessed-dependencies":
                for dependents in dependencies.iter("dep"):
                    if "prep" in dependents.get("type"):
                        type_dependent = "prep"
                    else: type_dependent = dependents.get("type")
                    ##print "type dependent: ", type_dependent
                    if type_dependent == "punct":
                        continue
            
                    gov_id = dependents[0].get("idx")
                    dep_id = dependents[1].get("idx")
                    dep_text = dependents[1].text
                
                    if gov_id  in nsubj_list:
                        #if type_dependent == "punct":
                            #print "Something is Fed with punct"
                        nsubj_deps_list.append(type_dependent)
                        nsubj_stdev_dict[gov_id] +=1
                        nominals_const_dict[gov_id].append([dep_id,type_dependent,dep_text]) #add id, dep type, word
                        all_nominals_stdev_dict[gov_id] +=1
                        all_nominal_deps.append(type_dependent)
                        ##print "nsubj: ", dependents.get("type")

                    if gov_id  in nsubj_pass_list:
                        nsubj_pass_deps_list.append(type_dependent)
                        nsubj_pass_stdev_dict[gov_id] +=1
                        nominals_const_dict[gov_id].append([dep_id,type_dependent,dep_text]) #add id, dep type, word
                        all_nominals_stdev_dict[gov_id] +=1
                        all_nominal_deps.append(type_dependent)
                        ##print "nsubj_pass: ", dependents.get("type")	

                    if gov_id  in agents_list:
                        agents_deps_list.append(type_dependent)
                        agents_stdev_dict[gov_id] +=1
                        nominals_const_dict[gov_id].append([dep_id,type_dependent,dep_text]) #add id, dep type, word
                        all_nominals_stdev_dict[gov_id] +=1
                        all_nominal_deps.append(type_dependent)
                        ##print "nsubj_pass: ", dependents.get("type")						
        
                    if gov_id  in dobj_list:
                        dobj_deps_list.append(type_dependent)
                        dobj_stdev_dict[gov_id] +=1
                        nominals_const_dict[gov_id].append([dep_id,type_dependent,dep_text]) #add id, dep type, word
                        all_nominals_stdev_dict[gov_id] +=1
                        all_nominal_deps.append(type_dependent)
                        ##print "dobj: ", dependents.get("type")
                        #if type_dependent == "advmod": #print "advmod in Sentence ", dependents[0].text, nsent
            
                    if gov_id  in pobj_list:
                        pobj_deps_list.append(type_dependent)
                        pobj_stdev_dict[gov_id] +=1
                        nominals_const_dict[gov_id].append([dep_id,type_dependent,dep_text]) #add id, dep type, word
                        all_nominals_stdev_dict[gov_id] +=1
                        all_nominal_deps.append(type_dependent)
                        ##print "pobj: ", dependents.get("type")

                    if gov_id  in iobj_list:
                        iobj_deps_list.append(type_dependent)
                        iobj_stdev_dict[gov_id] +=1
                        nominals_const_dict[gov_id].append([dep_id,type_dependent,dep_text]) #add id, dep type, word
                        all_nominals_stdev_dict[gov_id] +=1
                        all_nominal_deps.append(type_dependent)
                        ##print "iobj: ", dependents.get("type")
            
                    if gov_id  in ncomp_list:
                        ncomp_deps_list.append(type_dependent)
                        ncomp_stdev_dict[gov_id] +=1
                        nominals_const_dict[gov_id].append([dep_id,type_dependent,dep_text]) #add id, dep type, word
                        all_nominals_stdev_dict[gov_id] +=1
                        all_nominal_deps.append(type_dependent)
                        ##print "ncomp: ", dependents.get("type")
        
        
                    #####Only noun tags: this excludes cardinal numbers and pronouns#####
                    if gov_id  in nsubj_list and gov_id in noun_list:
                        nsubj_deps_list_NN.append(type_dependent)
                        nsubj_stdev_dict_NN[gov_id] +=1
                        all_nominals_stdev_dict_NN[gov_id] +=1
                        all_nominal_deps_NN.append(type_dependent)
                        ##print "nsubj: ", dependents.get("type")

                    if gov_id  in nsubj_pass_list and gov_id in noun_list:
                        nsubj_pass_deps_list_NN.append(type_dependent)
                        nsubj_pass_stdev_dict_NN[gov_id] +=1
                        all_nominals_stdev_dict_NN[gov_id] +=1
                        all_nominal_deps_NN.append(type_dependent)
                        ##print "nsubj_pass: ", dependents.get("type")							

                    if gov_id  in agents_list and gov_id in noun_list:
                        agents_deps_list_NN.append(type_dependent)
                        agents_stdev_dict_NN[gov_id] +=1
                        all_nominals_stdev_dict_NN[gov_id] +=1
                        all_nominal_deps_NN.append(type_dependent)
                        ##print "nsubj_pass: ", dependents.get("type")
                                
                    if gov_id  in dobj_list and gov_id in noun_list:
                        dobj_deps_list_NN.append(type_dependent)
                        dobj_stdev_dict_NN[gov_id] +=1
                        all_nominals_stdev_dict_NN[gov_id] +=1
                        all_nominal_deps_NN.append(type_dependent)
                        ##print "dobj: ", dependents.get("type")

                    if gov_id  in pobj_list and gov_id in noun_list:
                        pobj_deps_list_NN.append(type_dependent)
                        pobj_stdev_dict_NN[gov_id] +=1
                        all_nominals_stdev_dict_NN[gov_id] +=1
                        all_nominal_deps_NN.append(type_dependent)
                        ##print "pobj: ", dependents.get("type")

                    if gov_id  in iobj_list and gov_id in noun_list:
                        iobj_deps_list_NN.append(type_dependent)
                        iobj_stdev_dict_NN[gov_id] +=1
                        all_nominals_stdev_dict_NN[gov_id] +=1
                        all_nominal_deps_NN.append(type_dependent)
                        ##print "iobj: ", dependents.get("type")
            
                    if gov_id  in ncomp_list and gov_id in noun_list:
                        ncomp_deps_list_NN.append(type_dependent)
                        ncomp_stdev_dict_NN[gov_id] +=1
                        all_nominals_stdev_dict_NN[gov_id] +=1
                        all_nominal_deps_NN.append(type_dependent)
                        ##print "ncomp: ", dependents.get("type")

    # get nominal indices
    nsubj_deps_dict = Counter(nsubj_deps_list)

    n_all_nsubj_NN = len(all_nsubj_NN)

    det_nsubj_deps_NN_struct = dict_counter(nsubj_deps_dict, "det", n_all_nsubj_NN)
    rcmod_nsubj_deps_NN_struct = dict_counter(nsubj_deps_dict, "rcmod", n_all_nsubj_NN)

    # get VAC indices
    acad_database_file = open("/Users/mingxi/Desktop/TEMP/DISS/Oral_Defense/feature_validation/taasc/demo/acad_index_database_3.kdk", "rU").read().split("\n")
    acad_contingency_dict= dict_builder(acad_database_file)[2]
    acad_collexeme_ratio = ratio_compiler(acad_contingency_dict, lemma_constructicon_no_vcop)

    news_database_file = open("/Users/mingxi/Desktop/TEMP/DISS/Oral_Defense/feature_validation/taasc/demo/news_index_database_3.kdk", "rU").read().split("\n")
    news_contingency_dict= dict_builder(news_database_file)[2]
    news_av_faith_const_cue = contingency_database_counter(news_contingency_dict, lemma_constructicon_no_vcop, 5)

    news_lemma_freq_dict = dict_builder(news_database_file)[0]
    news_av_lemma_freq_log = simple_database_counter_log(news_lemma_freq_dict, lemma_constructicon, 0)

    print(files, det_nsubj_deps_NN_struct, rcmod_nsubj_deps_NN_struct, acad_collexeme_ratio, news_av_faith_const_cue, news_av_lemma_freq_log)
