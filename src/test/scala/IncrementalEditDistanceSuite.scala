import org.scalatest.FunSuite

class IncrementalEditDistanceSuite extends FunSuite {
  val phonemeMeasure = new PhonemeDistance("data/ipa_features.txt")
  val segmenter = PronunSegmenterFactory.create()
  val seg = segmenter.segment _

  def distances(measure : IncrementalEditDistance, prefix : String) : (Float, Float) = {
    val segments = seg(prefix)
    for (segment <- segments) {
      measure.push(segment)
    }
    val result = (measure.distance(), measure.minDistance())
    for (segment <- segments) {
      measure.pop()
    }
    assert(result._2 <= result._1)
    return result
  }

  test("it calculates unweighed edit distance") {
    val word = segmenter.segment("kissa")  
        
    def unitDistances(s : String, prefix : String) : (Float, Float) = {
      val measure = new IncrementalEditDistance(EditDistance.unitDistance, seg(s))
      distances(measure, prefix)
    }
        
    // Identical strings
    assertThrows[IllegalArgumentException](unitDistances("",""))
    assert(unitDistances("aaaaaaa","aaaaaaa") == (0f, 0f))

    // Empty comparison
    assert(unitDistances("a","")    == (1f, 0f) )
    //assert(unitDistances("","aaaa") == (4f, 4f) )

    // Replacement
    assert(unitDistances("a","b") == (1f, 1f))
    assert(unitDistances("aaaaa","baaaa") == (1f, 1f))
    assert(unitDistances("aabaa","aaaaa") == (1f, 1f))
    assert(unitDistances("aaaaa","aaaab") == (1f, 1f))

    // Insertion & deletion
    assert(unitDistances("akissa","kissab")  == (2f, 2f))
    assert(unitDistances("kissa","kissha")   == (1f, 1f))
    assert(unitDistances("kissa","kiss")     == (1f, 0f))
    assert(unitDistances("akibssa","kissac") == (3f, 3f))
  }
    

  test("it calculates weighed edit distance") {
    //val phonemeMeasure = new PhonemeDistance("data/ipa_features.txt")
    def phonemeDistance( phoneme1 : String, phoneme2 : String ) : Float = {
      phonemeMeasure.distance( phoneme1, phoneme2 ) / phonemeMeasure.MAX_DISTANCE
    }
    //val segmenter = PronunSegmenterFactory.create()
    //val seg = segmenter.segment _
        
    def segMeasure(s1 : String, s2 : String) : Float = {
      val measure = new IncrementalEditDistance(phonemeDistance, seg(s1))
      distances(measure, s2)._1
    }
    def debMeasure(s1 : String, s2 : String) {
      val distance = segMeasure(s1, s2)
      println("The distance between /" + s1 + "/ and /" + s2 + " is " + distance)
    }
        
    debMeasure("ˈɛləfənt","ˈɛlɪfənt")
    debMeasure("deɪ","meɪ")
    debMeasure("ˈæntəˌnɪm","ˈsɪnənɪm")
    debMeasure("æbˈeɪl.jəˌneɪt","ˌkɑnəˈteɪʃən")
    assert(segMeasure("ˈæntəˌnɪm", "") == 7)
    //assert(segMeasure("", "ˈæntəˌnɪm") == 7)
  }
}
