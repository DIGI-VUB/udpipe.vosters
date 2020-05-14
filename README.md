# udpipe.vosters

This repository contains an R package for doing Parts of Speech tagging and Lemmatisation on 19th century Southern Dutch texts.

- The package contains an UDPipe model 
- The model was trained on the Vosters corpus
    - https://rikvosters.be/wp-content/uploads/2018/09/2014-NandU-19CDutch.pdf
    - http://homepages.vub.ac.be/~wvdbussc/2014AHS3b.pdf
- Code used to construct the training dataset and the code used to train the model is available in the inst/train/src folder. The data which was used to train the model is not distributed in this package.

### Installation

- For installing the package: `remotes::install_github("DIGI-VUB/udpipe.vosters", build_vignettes = TRUE)`

Look to the vignette and the documentation of the functions

```
vignette("vosters", package = "udpipe.vosters")
help(package = "udpipe.vosters")
```

### Example

```
library(udpipe.vosters)
x <- data.frame(
  doc_id = c("a", "b"), 
  text = c("beschuldigd van zich pligtig of ten minsten 
            door medewerking af verheeling medepligtig gemaakt te hebben 
            aan eenen diefstal van Kleedings",
           "eenen langen en mageren persoon eenen kantoenen mantel 
            te beleenen had gebragt"), 
  stringsAsFactors = FALSE)
anno <- udpipe_vosters(x, tokenizer = "basic", split = " ", trace = TRUE)
anno <- udpipe_vosters(x, tokenizer = "generic")
anno
 doc_id sentence_id       token        lemma  upos       xpos token_id term_id start end
      a           1 beschuldigd beschuldigen  VERB         ww        1       1     1  11
      a           1         van          van   ADP         vz        2       2    13  15
      a           1        zich         zich  PRON        vnw        3       3    17  20
      a           1     pligtig     plichtig   ADJ        bnw        4       4    22  28
      a           1          of           of  CONJ         vw        5       5    30  31
      a           1         ten    tenminste     X    vz,lidw        6       6    33  35
      a           1     minsten    tenminste  NOUN        znw        7       7    37  43
      a           1        door         door   ADP         vz        8       8    58  61
      a           1 medewerking  medewerking  NOUN        znw        9       9    63  73
      a           1          af           of  CONJ         vw       10      10    75  76
      a           1  verheeling    verheling  NOUN        znw       11      11    78  87
      a           1 medepligtig medeplichtig   ADJ        bnw       12      12    89  99
      a           1     gemaakt        maken  VERB         ww       13      13   101 107
      a           1          te           te   ADP         vz       14      14   109 110
      a           1      hebben       hebben  VERB         ww       15      15   112 117
      a           1         aan          aan   ADP         vz       16      16   132 134
      a           1       eenen          een   DET       lidw       17      17   136 140
      a           1    diefstal     diefstal  NOUN        znw       18      18   142 149
      a           1         van          van   ADP         vz       19      19   151 153
      a           1   Kleedings kledingsgoed  NOUN        znw       20      20   155 163
      b           1       eenen          een   DET       lidw        1       1     1   5
      b           1      langen         lang     X    znw,znw        2       2     7  12
      b           1          en           en  CONJ         vw        3       3    14  15
      b           1     mageren        mager PROPN znw(neloc)        4       4    17  23
      b           1     persoon      persoon  NOUN        znw        5       5    25  31
      b           1       eenen          een   DET       lidw        6       6    33  37
      b           1   kantoenen     katoenen  NOUN        znw        7       7    39  47
      b           1      mantel       mantel  NOUN        znw        8       8    49  54
      b           1          te           te   ADP         vz        9       9    69  70
      b           1    beleenen      belenen  VERB         ww       10      10    72  79
      b           1         had       hebben  VERB         ww       11      11    81  83
      b           1     gebragt      brengen  VERB         ww       12      12    85  91
```

### License

The package and model is distributed under the CC-BY-NC-SA license (https://creativecommons.org/licenses/by-nc-sa/4.0)

### Python

- If you prefer to use the model with Python
- Grab the model and use ufal.udpipe to do the tagging and lemmatisation

```
>>> from ufal.udpipe import Model, Pipeline, ProcessingError
>>> error = ProcessingError()
>>> model = Model.load('inst/models/dutch-vosters-20200514.udpipe')
>>> pipeline = Pipeline(model, 'vertical', Pipeline.DEFAULT, 'none', 'conllu')
>>> tokenized_sentence = '\n'.join(['eenen', 'langen', 'persoon', 'had', 'gebragt']) 
>>> print(pipeline.process(tokenized_sentence, error))
```

### Future

- Model is subject to possible change in the future when hyperparameters will be tuned further 


### DIGI

By DIGI: Brussels Platform for Digital Humanities: https://digi.research.vub.be

![](vignettes/logo.png)
