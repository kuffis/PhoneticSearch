Phonetic Search is a toy for finding similarly pronounced words or phrases.

![Example use case](doc/sieni.jpg)

Let's find out closest matches to sieni. The toy offers a command line interface
through Scala build tool (sbt) and takes the word as a parameter.

```
> sbt
> run sieni

...
sieni:/siːni/
Searching phonetically similar words ...
Word: seay knee, pronun:/s_iː _ni/, distance:0.0
Word: cee knee, pronun:/siː _ni/, distance:0.0
Word: sea knee, pronun:/siː _ni/, distance:0.0
Word: seay nee, pronun:/s_iː niː/, distance:0.04
...
```

Since "sieni" is not an English word, the toy used a machine learning model to
create an IPA pronunciations for the new word. The model has been trained 
with english pronunciations from Wiktionary.

It seems to disagree with the pun meme, outputting "sea knee" as the first
phonetic match which also is a proper English word, instead of "see any" as in the meme.
