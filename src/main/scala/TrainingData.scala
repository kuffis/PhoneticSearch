import scala.collection.immutable.HashMap
import java.io.PrintWriter

/**
 * A list of training items and common data structures
 * for processing them.
 */
class TrainingData(val trainingItems : List[TrainingItem]) {
    val START = "S"
    val END = "E"

    /** List of phonemes as which a main letter can realize. */
    val lettersToPhonemes = extractCharsAndPhonemes()
    /** List of main letters. */
    val letters = extractLetters()
    /** List of phonemes. */
    val phonemes = extractPhonemes()
    
    /** Extracts a list of phonemes as which a main letter can realize. */
    def extractCharsAndPhonemes() : HashMap[String, Set[String]] = {
        val emptyChars2Phonemes = new HashMap[String, Set[String]]()
        val splitItems = trainingItems.filter(i => i.isSplit)
        val charPhonemePairs = splitItems.map(item => item.splitting).flatten
      
        charPhonemePairs.foldLeft(emptyChars2Phonemes) { (chars2Phonemes, charPhonemePair) => {
            val (ch, ph) = charPhonemePair
            val previousPhonemes = chars2Phonemes.get(ch).getOrElse(Set[String]())
            chars2Phonemes + (ch -> (previousPhonemes + ph))
        }}
    }

    /** Extracts a list of letters. */ 
    def extractLetters() : Set[String] = {
        lettersToPhonemes.keys.toSet ++ Set(START, END)
    }
    
    /** Extracts a list of phonemes. */
    def extractPhonemes() : Set[String] = {
        lettersToPhonemes.values.toList.flatten.toSet
    }

    /** Writes the training data to a file. */
    def write(fileName : String) {
        val writer = new PrintWriter(fileName)
        for (trainingItem <- trainingItems) {
           val chars    = Util.join(trainingItem.splitting.map(a => a._1), "|")
           val phonemes = Util.join(trainingItem.splitting.map(a => a._2), "|").replace("_","")
           
           writer.println(chars + ":/" + phonemes + "/")
        }
        writer.close()
    }
}
