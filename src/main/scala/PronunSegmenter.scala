import scala.io.Source

/**
 * Phoneme comparison is based on features. For example,
 * distance("k","a") > distance("a","a:")
 * Segmenter splits a pronunciation into such phonemes, for which we can find
 * the features to compare distance.
 *
 * @param phonemesToFeatures List of phonemes for which we know the features.
 */
class PronunSegmenter(val phonemesToFeatures : Map[String, String]) {
  val phonemes = phonemesToFeatures.keys.toList.sortBy(a => -a.length)

  /** Splits the string into stress mark and the remaining string. */
  def splitStress(text : String) : (String, String) = {
    if (text.startsWith("ˈ") || text.startsWith("ˌ"))
      ("" + text(0), text.substring(1))
    else
      ("", text)
  }

  /** Splits vowel length mark or syllable pause away from the string. */
  def splitLongVowel(text : String) : (String, String) = {
    if      (text.startsWith("ː.")) ("ː.", text.substring(2))
    else if (text.startsWith("ː"))  ("ː",  text.substring(1))
    else if (text.startsWith("."))  (".",  text.substring(1))
    else if (text.startsWith("̆"))   ("̆",   text.substring(1))
    else                            ("",   text)
  }

  /** Segments a pronunciation into phonemes for which we can find features. */
  def segment(text : String) : List[String] = {
    if (text == "") return List[String]()
    else {
      val (stress, noStressText) = splitStress(text)
      val corePhoneme = phonemes.find(phoneme => noStressText.startsWith(phoneme))
      if (corePhoneme == None) {
        throw new IllegalArgumentException("Phoneme " + text + " can't be segmented.")
      } else {
        //println("Core phoneme:" + corePhoneme)
        val noPhonemeText = noStressText.substring(corePhoneme.get.length)
        val (longVowel, remainingText) = splitLongVowel(noPhonemeText)
        val result = stress + corePhoneme.get + longVowel
        return (result :: segment(remainingText))
      }
    }
  }

  /** Drops a character. */
  def drop1(segment : String) : String = {
    val l = segment.length
    if (l == 0) segment else segment.substring(0, l-1)
  }

  /** Removes the extra features marks from a phoneme. */
  def cleanExtraFeatures(segment : String) : String = {
    val (stress, noStress) = splitStress(segment)

    val noShortM    = (if (noStress.endsWith("̆")) drop1(noStress) else noStress)
    val noPause     = (if (noShortM.endsWith(".")) drop1(noShortM) else noShortM)
    val noLongVowel = (if (noPause.endsWith("ː"))  drop1(noPause)  else noPause)
    return noLongVowel
  }

  /**
   * For phonetic distance calculation, returns the extra features which
   * can be present or absent in any phoneme and are therefore not part of
   * phonemes-to-features table.
   */
  def extraFeatures(segment : String) : String = {
    val initials =
      if      (segment(0) == 'ˈ') "+-"
      else if (segment(0) == 'ˌ') "-+"
      else                        "--"

    val finals =
      if      (segment.endsWith("ː.")) "++"
      else if (segment.endsWith("."))  "+0"
      else if (segment.endsWith("ː"))  "-+"
      else if (segment.endsWith("̆"))   "--"
      else                             "-0"

    return (initials + finals)
  }

  /** Returns the list of features for a pronunciation. */
  def features (segment : String) : String = {
    phonemesToFeatures(cleanExtraFeatures(segment)) + extraFeatures(segment)
  }
}

/**
 * Factory method for creating a pronunciation segmenter
 * from a list of phonemes and their features.
 */ 
object PronunSegmenterFactory {
  def parseLine(line : String) : Option[(String, String)] = {
    val format = """(.+)\=([\+\-0]+)""".r
    line match {
      case format(phoneme, features) => Some((phoneme, features))
      case _ => None
    }
  }
  def create() : PronunSegmenter = {
    val lines = Source.fromFile("data/ipa_features.txt").getLines
    val phonemes = lines.map(parseLine).flatten.toMap
    return new PronunSegmenter(phonemes)
  }
}
