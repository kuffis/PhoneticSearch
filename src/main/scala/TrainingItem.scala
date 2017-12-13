/**
 * Represents one English word and its pronunciation.
 * Used when training a machine learning model on how
 * unknown words are pronounced in English.
 *
 * @param splitting A pairing between letters and phonemes.
 * @param unsplitWord The letters of the word.
 * @param unspitPronun The phonemes of the pronunciation.
 * @param isSplit Could we match letters to phonemes?
 *                Only split words are useful for training a model.
 */
class TrainingItem(
  /**
   * A match between word characters and pronunciation phonemes.
   * These 2 arrays contain an equal number of characters and phonemes.
   */
  val splitting : Array[(String, String)],

  /** The English word */
  val unsplitWord : String,

  /** IPA pronunciation */
  val unsplitPronun : String,

  /** Were we able to match letters to phonemes? If not, the splitting is empty. */
  val isSplit : Boolean
) {
  /** Debugs the matching from letters to phonemes. */
  def debugSplit() : String = {
    if (isSplit)
      Util.join(splitting.map(a => a._1), "|") + " /" + Util.join(splitting.map(a => a._2), "|") + "/"
    else
      unsplitWord + "/" + unsplitPronun + "/"
  }
}
