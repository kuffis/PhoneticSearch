/**
 * Calculates edit distance from one main string
 * to many comparison strings. Designed to be used with
 * PronunTreeNode to efficiently measure edit distance while
 * traversing the tree depth-first.
 *
 * Example:
 *   // Traditional
 *   val kissa = segmenter.segment("kissa")
 *   val koira = segmenter.segment("koira")
 *   val distance = new EditDistance(weighter).distance(kissa, koira)
 *
 *   // Incremental
 *   val inc = new IncrementalEditDistance(weighter, kissa)
 *   for (segment <- koira)
 *     inc.push(segment)
 *   val distance = inc.distance()
 *
 * @param weighter Calculates distance between tw characters in alphabet.
 * @param segments The fixed main string which we compare to many others.
 */
class IncrementalEditDistance(
  val weighter : (String, String) => Float,
  val segments : List[String])
{
  // The dynamic programming matrix.
  private val matrix = initMatrixWithSegments()

  // How many characters have been pushed.
  // Tells how many rows in the matrix we have calculated.
  var prefixLength = 0

  /** Initializes the dynamic programming matrix. */
  def initMatrixWithSegments() = {
    if (segments.length == 0) {
       throw new IllegalArgumentException("The main string must not be empty!");
    }

    val matrix = Array.ofDim[Float](100, segments.length)    
    for (i <- Range(1, 100))
      matrix(i)(0) = i
    for (i <- Range(1, segments.length))
      matrix(0)(i) = i
    matrix
  }

  /** Adds a character to the changing string. */
  def push(prefix : String) {
    // Updates the dynamic programming matrix with one row.
    if (prefixLength == 0) {
      matrix(0)(0) = weighter(prefix, segments(0))
      for (i <- Range(1, segments.length)) {
        matrix(0)(i) = i //+ weighter(prefix, segments(0))
      }
    } else {
      for (i <- Range(1, segments.length)) {
        val insert_s1 = matrix(prefixLength - 1)(i  ) + 1
        val insert_s2 = matrix(prefixLength    )(i-1) + 1
        val replace   = matrix(prefixLength - 1)(i-1) + weighter(prefix, segments(i))
        matrix(prefixLength)(i) = List(insert_s1, insert_s2, replace).min
      }
    }
    prefixLength += 1
  }

  /** Removes a character from the changing string. */
  def pop() {
    prefixLength -= 1
  }

  /** 
   * Returns the edit distance between
   * the fixed string (given when instantiating)
   * and the changing string (given by push()ing characters)
   */
  def distance() : Float = {
    if (prefixLength == 0) return segments.length
    matrix(prefixLength - 1)(segments.length-1)
  }
  
  /** 
   * Returns the minimal distance, when the prefix
   * might still continue with more characters.
   */ 
  def minDistance() : Float = {
    if (prefixLength == 0) return 0
    matrix(prefixLength - 1).min
  }
}

