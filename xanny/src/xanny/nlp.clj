(ns xanny.nlp
     (:use [opennlp.nlp]
           [opennlp.treebank]
           [clj-wordnet.core]
           ))

;==========================================================================================
;OPENNLP
;==========================================================================================

(def get-sentences (make-sentence-detector "NLP/models/en-sent.bin"))
(def tokenize (make-tokenizer "NLP/models/en-token.bin"))
(def detokenize (make-detokenizer "NLP/models/english-detokenizer.xml"))
(def pos-tag (make-pos-tagger "NLP/models/en-pos-maxent.bin"))
(def name-find (make-name-finder "NLP/models/namefind/en-ner-person.bin"))
(def chunker (make-treebank-chunker "NLP/models/en-chunker.bin"))

;==========================================================================================
;WORDNET
;==========================================================================================
(def wordnet (make-dictionary "NLP/WordNet-3.0"))



