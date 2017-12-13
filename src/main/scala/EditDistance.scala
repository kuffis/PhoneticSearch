/**
 * Calculates edit distance between two strings.
 * https://en.wikipedia.org/wiki/Edit_distance
 *
 * @param weighter For weighted edit distance, calculates the distance
 *                 between characters.
 *                 EditDistance.unweighed ignores this.
 */
class EditDistance(val weighter : (String, String) => Float) {
  /** Calculates the edit distance between strings. */
  def distance(s1 : String, s2 : String) : Float = {
    // Empty string + insertions
    if (s1.length == 0) return s2.length
    if (s2.length == 0) return s1.length

    val matrix = Array.ofDim[Float](s1.length, s2.length)
    matrix(0)(0) = weighter("" + s1(0), "" + s2(0))
    for (i <- Range(1, s1.length))
      matrix(i)(0) = i
    for (i <- Range(1, s2.length))
      matrix(0)(i) = i

    for (i1 <- Range(1, s1.length)) {
      for (i2 <- Range(1, s2.length)) {
        val insert_s1 = matrix(i1-1)(i2  ) + 1
        val insert_s2 = matrix(i1  )(i2-1) + 1
        val replace   = matrix(i1-1)(i2-1) + weighter("" + s1(i1), "" + s2(i2))
        matrix(i1)(i2) = List(insert_s1, insert_s2, replace).min
      }
    }

    //for (i1 <- Range(0, s1.length)) {
    //  for (i2 <- Range(0, s2.length)) {
    //    print(" " + matrix(i1)(i2))
    //  }
    //  println("")
    //}


    return matrix(s1.length - 1)(s2.length - 1)
  }
  
  /** Returns the edit distance between the two segmented strings. */
  def distance(s1 : List[String], s2 : List[String]) : Float = {
    // Empty string + insertions
    if (s1.length == 0) return s2.length
    if (s2.length == 0) return s1.length

    matrix(s1, s2)(s1.length-1)(s2.length-1)
  }
  
  /** 
   * Returns the minimal distance, when the prefix
   * might still continue with more characters.
   *
   * @param s1 A whole string.
   * @param s2 A prefix, which might continue with more characters.
   * @return Lower bound for edit distance, whichever way prefix continues.
   */ 
  def minDistance(s1 : List[String], prefix : List[String]) : Float = {
    // Minimal distance is to add all characters in prefix.
    if (s1.length == 0) return prefix.length

    // If prefix continues with s1, the distance is 0 between identical strings.
    if (prefix.length == 0) return 0

    matrix(prefix, s1)(prefix.length-1).min
  }

  /** Calculates the dynamic programming matrix between
   *  between the two strings.
   *  https://en.wikipedia.org/wiki/Wagner%E2%80%93Fischer_algorithm
   */
  def matrix(s1 : List[String], s2 : List[String]) : Array[Array[Float]] = {

    val matrix = Array.ofDim[Float](s1.length, s2.length)
    matrix(0)(0) = weighter(s1(0), s2(0))
    for (i <- Range(1, s1.length))
      matrix(i)(0) = i
    for (i <- Range(1, s2.length))
      matrix(0)(i) = i

    for (i1 <- Range(1, s1.length)) {
      for (i2 <- Range(1, s2.length)) {
        val insert_s1 = matrix(i1-1)(i2  ) + 1 // Deletion
        val insert_s2 = matrix(i1  )(i2-1) + 1 // Insertion
        // Substitution (adds 0 for identical characters)
        val replace   = matrix(i1-1)(i2-1) + weighter(s1(i1), s2(i2))
        matrix(i1)(i2) = List(insert_s1, insert_s2, replace).min
      }
    }

    //for (i1 <- Range(0, s1.length)) {
    //  for (i2 <- Range(0, s2.length)) {
    //    print(" " + matrix(i1)(i2))
    //  }
    //  println("")
    //}

    return matrix
  }
}

/** Factory for edit distance measures. */
object EditDistance {
  /** Unweighted edit distance */
  val unweighed = new EditDistance(unitDistance)

  /** Returns unweight distance between two characters in alphabet. */
  def unitDistance(s1 : String, s2 : String) : Float = {
    if (s1 == s2) 0f else 1f
  }
}
