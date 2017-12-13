import scala.io.Source
import scala.util.matching.Regex

/**
 * Reads (word, pronunciation) pairs from a file.
 * Since the training data was exctracted from Wiktionary,
 * it needs some notation unification & filtering.
 */
object TrainingItemReader {
  /** Empty list of training items. */
  private val empty = List[TrainingItem]()

  /** Reads the training items from a file like data/ipa.txt */
  def read(fileName : String) : List[TrainingItem] = {
    val lines = Source.fromFile(fileName).getLines
    lines.toList.map(parseTrainingItem).flatten
  }

  /** Unifies inconsistent IPA notations used in Wikipedia. */  
  def unifyIpa(pronun : String) : String = {
    // Replace nonstandard primary and secondary stress marks
    // with standard stress marks.
    pronun
      .replace("'","ˈ")
      .replace("ʹ","ˈ")
      .replace("ʼ","ˈ")
      .replace("‘","ˈ")
      .replace("ˡ","ˈ")

    // Replace nonstandard pause marks with the standard one.
      .replace(" ",".") // Thin space \u2009
      .replace(" ",".") // (normal) Space
      .replace("\u00a0", ".") // No-break space
      .replace("\u2003", ".") // Em Space
      .replace(".",".")
      .replace("—",".") // "-" in pronunciation, f.ex in pronun of "off-topic" 
      .replace("-",".")

    // To find out how similar two phonemes are, we use phonemes-to-features
    // data extracted from corpustools. Therefore we must convert some
    // Wikipedia phonemes to corpustools phonemes.
      .replace("d͡ʒ","d͡z")
      .replace("d​͡ʒ","d͡z")
      .replace("\u200c", "") // Zero-Width Non-joiner
      .replace("ˈ.",".ˈ")
      .replace("\u031a","")
      .replace("t͡ʃ","ʧ")
      .replace("ǝ","ə")
      .replace("ә","ə")
      .replace("e͡ɪ","eɪ")
      .replace("o͡ʊ","oʊ")
      .replace("ɛ͡ɹ","ɛɹ")
      .replace("ɔ͡ɪ","ɔɪ")
      .replace("a͡ɪ","aɪ")
      .replace("a͡ʊ","aʊ")
      .replace("ə͡l","əl")

      // Some rare phoneme features.
      .replace("ɜ","3")
      .replace("əʳ","ɚ")
      .replace("̪","̯")
      .replace("3˞","ɝ")
      .replace("ʰ","") // Remove aspiration (questionable, but under 1% of Wiktionary words have aspiration marks)
      .replace("ᵻ","I")
      .replace("ˑ","") // Remove half-long mark
      .replace("ĭ","ĭ")
    }

  /** Normalizes the word to make it easier to match letters to phonemes. */
  def normalizeWord(word : String) : String = {
    word
      .toLowerCase     // Germany -> germany
      .replace("-","") // able-bodies -> ablebodied
      .replace(" ","") // Costa Rica -> costarica
      .replace("'","") // don't -> dont
  }

  /**
   * Some letters are not pronounced, for example in
   * knee:/|n|ee/ the k realizes as an empty phoneme.
   *
   * Mark empty phonemes with "_"
   * knee:/_|n|ee/
   */
  def addEmptyPhonemes(pronun : String) : String = {
    ("#" + pronun + "#")
      .replace("||", "|_|")
      .replace("#|", "_|")
      .replace("|#", "|_")
      .replace("#","")
  }

  /**
   * Parses a single (word, pronunciation) pair. This may produce
   * 0-n training items, since some entries are unfit for our use
   * and some entries contain multiple alternative pronunciations.
   */
  def parseTrainingItem(word : String, pronun : String) : TrainingItem = {
    val wordParts = normalizeWord(word).split("\\|")
    
    val pronunParts =
      unifyIpa(
        addEmptyPhonemes(pronun)
      ).split("\\|")

    if (wordParts.length > 1 && pronunParts.length > 1) {
      new TrainingItem(wordParts zip pronunParts, Util.join(wordParts), Util.join(pronunParts), true)
    } else if (wordParts.length == 1 && wordParts(0).length == 1) {
      //new TrainingItem(wordParts zip pronunParts, wordParts(0), pronunParts(0), true)
      new TrainingItem(null, wordParts(0), pronunParts(0), false)
    } else {
      new TrainingItem(null, wordParts(0), pronunParts(0), false)      
    }
  }

  /** 
   * Returns a pronunciation vaariant with the optional phoneme.
   * @param s Prounciation with an optional phoneme, f.ex kis(s)a -> kissa
   */
  def optionalPresent(s : String) : String = {
    val _open = s.indexOf("(")
    val _close = s.indexOf(")")
    if (_open > -1 && _close > -1) {
      s.substring(0, _open) + s.substring(_open + 1, _close) + s.substring(_close + 1)
    } else {
      s
    }
  }
  /** 
   * Returns a pronunciation vaariant without the optional phoneme.
   * @param s Prounciation with an optional phoneme, f.ex kis(s)a -> kisa
   */
  def optionalAbsent(s : String) : String = {
    val _open = s.indexOf("(")
    val _close = s.indexOf(")")
    if (_open > -1 && _close > -1) {
      s.substring(0, _open) + s.substring(_close + 1)
    } else {
      s
    }
  }

  /** Maps a function to all elements of an array unless the array is null. */
  def maybeMap(a: Array[(String, String)], f: (String => String)) : Array[(String, String)] = {
    if (a == null)
      null
    else
      a.map(i => (i._1, f(i._2)))
  }

  /**
   * When the pronunciation contians an optional phoneme,
   * generates 2 pronunciation vaariants: with & without the optional phoneme.
   */
  def unpackOptionalPhoneme(item : TrainingItem) : List[TrainingItem] = {
    if (item.unsplitPronun.contains("(")) {
      val present = new TrainingItem(maybeMap(item.splitting, optionalPresent), item.unsplitWord, optionalPresent(item.unsplitPronun), item.isSplit)
      val absent  = new TrainingItem(maybeMap(item.splitting, optionalAbsent),  item.unsplitWord, optionalAbsent(item.unsplitPronun),  item.isSplit)
      
      unpackOptionalPhoneme(present) ++ unpackOptionalPhoneme(absent)
    } else {
      item :: empty
    }
  }

  /** Parses a training item line. */
  def parseTrainingItem(line : String) : List[TrainingItem] = {
    val format = """(.+):/(.+)/""".r
    line match {
      case format(word, pronun) => 
        if (word == word.toUpperCase)
          // Ignore abbreviations like CIA
          empty
        else {
          val item = parseTrainingItem(word, pronun)
          unpackOptionalPhoneme(item)
        }
      case _ => empty
    }
  }
}
