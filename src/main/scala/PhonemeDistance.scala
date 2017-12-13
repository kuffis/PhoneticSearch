import scala.collection.mutable.HashMap
import scala.io.Source

/**
 * Calculates the distance between two phonemes
 * based on phoneme feature table from corpustools.
 *
 * Weighed edit distance uses this to calculates how far two pronunciations
 * are from each others.
 *
 * @param dataFile Phoneme feature table (data/ipa_features.txt)
 */
class PhonemeDistance(val dataFile : String) {
  val phonemesToFeatures : HashMap[String, String] = createFeatureMap()
  val MAX_DISTANCE = 25f

  /**
   * Creates a mapping from phonemes to features.
   */
  def createFeatureMap() : HashMap[String, String] = {
    val format = """(.+)[=]([+-0]+)""".r
    val lines = Source.fromFile(dataFile).getLines.filter(l => l.contains("="))
    val phonemesAndFeatures = lines.map( line => {
      val format(phoneme, features) = line
      (phoneme, features)
    })
    HashMap[String, String]() ++ phonemesAndFeatures
  }

  val pronunSegmenter = PronunSegmenterFactory.create()
  
  /** Counts how many features are different between two phonemes. */
  def distance(phoneme1 : String, phoneme2 : String) : Float = {
    //val f1 = phonemesToFeatures(phoneme1)
    //val f2 = phonemesToFeatures(phoneme2)
    val f1 = pronunSegmenter.features(phoneme1)
    val f2 = pronunSegmenter.features(phoneme2)
    val distances =
      Range(0, f1.length).map({ i => {
        val distance =
          if      (f1(i) == 0)     { 0.25f } // Feature not applicable in phoneme 1
          else if (f2(i) == 0)     { 0.25f } // Feature not applicable in phoneme 2
          else if (f1(i) == f2(i)) { 0f }    // Feature identical
          else                     { 1f }    // Feature different
        distance
      }})
    return distances.sum
  }  
}
