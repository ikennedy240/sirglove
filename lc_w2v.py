# script for testing Gensim word2vec on CL corpus

import pandas as pd
import numpy as np
import logging
import os
import regex as re
from gensim.models import Word2Vec
from gensim.models.phrases import Phrases, Phraser
from gensim.similarities.index import AnnoyIndexer
%matplotlib inline
import matplotlib.pyplot as plt
from sklearn.manifold import TSNE
from sklearn.decomposition import PCA
from nltk.stem import PorterStemmer

%config InlineBackend.figure_format = "retina"

"""Start Logging"""
logging.basicConfig(format='%(asctime)s : %(levelname)s : %(message)s', level=logging.INFO)

"""Get ready to read some Sentences"""
class MySentences(object):
    def __init__(self, filename):
        self.filename = filename

    def __iter__(self):
        ps = PorterStemmer()
        punctuation_pattern = r"[#\w]+|[!?]+" # compile a regex that saves certain characters
        url_pattern = r'(http)?(www)?\S*(\.com|\.net|\.gov|\.be|\.org)\S*'
        short_pattern = r"(^| |')\b\w{1,3}\b"
        for line in open(self.filename, encoding='utf-8'):
            line = re.sub(url_pattern,' its_a_URL ', line.lower()) # clean URLS
            line = re.sub(r'\s\w*\d+\w*\s', ' its_a_number ', line)  # clean Numbers
            #line = re.sub(short_pattern, ' ', line)
            line = re.sub(r'\W',' ',line)
            line = re.sub(r'queen anne', 'queen_anne', line)
            line = re.sub(r'(u district)|(university district)', 'university_district', line)
            line = re.sub(r'courtesy patrol', 'courtesy_patrol', line)
            line = re.sub(r'gated community', 'gated_community', line)
            yield [ps.stem(x) for x in line.split()]

def most_similar_by_data(filep, word, topn, model_only=False):
    # pass the text file to the sentence iterator
    sentences = MySentences("data/text/"+filep) # a memory-friendly iterator

    # train the w2v model
    #phrases = Phrases(sentences, threshold = 1000, common_terms =  ["queen anne", "federal way", "university district"])
    #bigram_transformer = Phraser(phrases)
    model = Word2Vec(sentences, workers = 2)
    word_vectors = model.wv
    if model_only is True:
        return model
    return word_vectors.similar_by_word(word, topn = topn)

def display_keywords_PCAscatterplot(model, word, ref = False, key_words = None, fig = None, ax = None, title=None):

    arr = np.empty((0,100), dtype='f')
    word_labels = [word]
    word_vectors = model.wv

    # if there are no keywords, get the closest 20 from the model
    if key_words is None:
        key_words = [x[0] for x in word_vectors.similar_by_word(word, topn = 7)]

    # add the vector for each of the closest words to the array
    arr = np.append(arr, np.array([word_vectors[word]]), axis=0)
    for key_word in key_words:
        try:
            wrd_vector = word_vectors[key_word]
        except KeyError:
            print("KeyError: "+key_word+" not in this model")
            continue
        word_labels.append(key_word)
        arr = np.append(arr, np.array([wrd_vector]), axis=0)
    pca = PCA(n_components=2)
    result = pca.fit_transform(arr)
    # create a scatter plot of the projection
    if fig is None:
        fig, ax = plt.subplots(figsize = (12.8,9.6))
    if ref:
        ax.scatter(result[:, 0], result[:, 1], c = 'r')
        display_keywords_PCAscatterplot(models['sent_tokenized.txt'], word = word, key_words=  key_words, fig = fig, ax = ax)
    if not ref:
        ax.scatter(result[:, 0], result[:, 1], c = 'b')
    for i, word in enumerate(word_labels):
    	ax.annotate(word, xy=(result[i, 0]+.04, result[i, 1]+.04), fontsize = 16)
    ax.set_title(title)
    print(word_labels)
    return fig


################## START ##################

word = "crimin" # set a key word of interest
top_n = 30 # set how many words we'll focus on
fileps = [x for x in os.listdir('data/text') if ('seattle' in x)|('outside' in x)] # get filenames
fileps = fileps+['sent_tokenized.txt']
models = {} # initiate empty dictionary of models
for filep in fileps:
    models[filep] = most_similar_by_data(filep, word, top_n, model_only = True) # build w2v models for each file


fileps == list(models.keys())

similarities = pd.DataFrame()
for filep in fileps:
    word_vectors = models[filep].wv
    top_words = word_vectors.similar_by_word(word, topn = top_n)
    similarities[re.sub('.txt','',filep)+'_words'] = [x[0] for x in top_words]
    similarities[re.sub('.txt','',filep)+'_values'] = [x[1] for x in top_words]

similarities[[x for x in similarities.columns if 'seattle' in x]]
word_vectors.similar_by_word("courtesy_patrol", topn = top_n)

ps.stem('requested')
set(similarities.before_7_1_words).difference(similarities['7_1_to_2_19_words'])
set(similarities['7_1_to_2_19_words']).difference(similarities.before_7_1_words)
set(similarities.after2_19_words).difference(similarities['7_1_to_2_19_words'])
set(similarities.seattle_7_1_to_2_19_words).difference(similarities['outside_7_1_to_2_19_words'])

key_words = np.unique(similarities.iloc[:,similarities.columns.str.endswith('words')].values)
plots = {}
for filep in fileps:
    plots[filep] = display_keywords_PCAscatterplot(model = models[filep], word = word, key_words = similarities.sent_tokenized_words.head(10), ref= False, title=filep)

similarities.columns
ps.stem('controlled')

display_keywords_PCAscatterplot(model = models['sent_tokenized.txt'], word = 'sex', key_words = None, ref= False, title="full sample embeddings for 'sex'")
