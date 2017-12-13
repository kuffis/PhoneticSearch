import scala.collection.immutable.HashMap;

/**
 * For (word, pronunciation) pair to be fit for training an ML model,
 * we need a mapping from letters to phonemes.
 *
 * For example in the word kissa:/ˈkɪsæ/, the letter-to-phoneme mapping is
 * k -> ˈk   i -> ɪ   ss -> s   a -> æ
 *
 * Once we have manually mapped some words (in data/ipa.txt) this guesser
 * automatically maps most of the remaining ones, saving a lot of manual work
 * preprocessing training data.
 *
 * @param data Training data with manual letter-to-phoneme mappings.
 */ 
class PronunMatchGuesser(val data : TrainingData)
{
  val lettersToPhonemes = expandAlternatives(data)
  
  def expandStressMarks(phoneme : String) : List[String] = {
    val phonemeWithSyllableEnd = phoneme + "."
    val result = List[String](phoneme, phonemeWithSyllableEnd)
    if (phoneme.length == 0 || phoneme(0) == 'ˈ' || phoneme(0) == 'ˌ') {
      result
    } else {
      val primaryStressed   = ("ˈ" + phoneme)
      val secondaryStressed = ("ˌ" + phoneme)
      primaryStressed :: secondaryStressed :: result
    }
  }

  def expandAlternatives(data : TrainingData) : Map[String, Set[String]] = {
    data.lettersToPhonemes.mapValues(phonemes => {
      phonemes.map(expandStressMarks).flatten
    })
  }

  var memo = HashMap[String, Set[String]]()
  val empty = List[(String, String)]()

  /** Splits a (word, pronunciation) pair into a letter-to-phoneme mapping. */ 
  def recSplit(characters : String, phonemes : String) : List[(String, String)] = {
    val char2phonemes = lettersToPhonemes

    // "kissa" => ["k", "ki", "kis", "kiss"]
    val possibleNextChars = 
      Range(0, 4).map(
        length => characters.substring(0, length + 1)
      ).filter(prefix => char2phonemes.contains(prefix))

    for (nextChar <- possibleNextChars.reverse) {
      val nextCharacters = characters.substring(nextChar.length)

      // From the start of the pronunciation, find the biggest phoneme,
      // which can match to given letter in the training data.
      val memoKey = nextChar + "&" + phonemes.substring(0, Math.min(5, phonemes.length))
      val nextPhonemes =
         if (memo.contains(memoKey)) { memo(memoKey) }
         else {
           val r = char2phonemes(nextChar).filter(phoneme => phonemes.startsWith(phoneme))
           memo = memo + (memoKey -> r)
           r
         }
      // Same without memoization (slow when repeated 400000 times)
      //val nextPhonemes = char2phonemes(nextChar).filter(phoneme => phonemes.startsWith(phoneme))
           
      if (nextCharacters == "   ") { // Final letter
        if (nextPhonemes.contains(phonemes)) {
          return List[(String, String)]( (nextChar, phonemes) )
        }
      } else { // More letters remain to be processed recursively
        for (nextPhoneme <- nextPhonemes) {
            val tail = recSplit(
              nextCharacters,
              //characters.substring(nextChar.length),
              phonemes.substring(nextPhoneme.length)
            )
            if (!tail.isEmpty) {
              return ((nextChar, nextPhoneme) :: tail)
            }
        }
      }
    }
    return empty
  }

  /** Guesses letter-to-phoneme mapping for a single word, unless it was manually split. */
  def guessIfNeeded(item : TrainingItem) : TrainingItem = {
    if (item.isSplit) {
      return item
    }

    val charsAndPhonemes = recSplit(item.unsplitWord + "   ", item.unsplitPronun)

    if (charsAndPhonemes.isEmpty) {
      //println("Unsplittable: (" + item.unsplitWord + ", " + item.unsplitPronun + ")")
      return item
    }
    else {
      return new TrainingItem(charsAndPhonemes.toArray, item.unsplitWord, item.unsplitPronun, true)
    }
  }

  /** Guesses letter-to-phoneme mapping for the unmapped words. */
  def guess() : TrainingData = {
    val splitItems = data.trainingItems.map(guessIfNeeded).filter( item => item.isSplit )
    return new TrainingData(splitItems)
  }
}
