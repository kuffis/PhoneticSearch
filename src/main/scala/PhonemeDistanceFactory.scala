/**
 * Creates phoneme distance measure for weighted edit distance calculation.
 */
object PhonemeDistanceFactory {
  /** Returns phoneme distance weigher with reasonable configuration. */
  def create() : ((String, String) => Float) = {
    val phonemeMeasure = new PhonemeDistance("data/ipa_features.txt")
    def result(phoneme1 : String, phoneme2 : String) : Float = {
      phonemeMeasure.distance(phoneme1, phoneme2) / phonemeMeasure.MAX_DISTANCE
    }
    result
  }
}
