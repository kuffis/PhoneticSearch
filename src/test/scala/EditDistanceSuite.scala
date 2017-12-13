import org.scalatest.FunSuite

class EditDistanceSuite extends FunSuite {
    val phonemeMeasure = new PhonemeDistance("data/ipa_features.txt")
    val segmenter = PronunSegmenterFactory.create()
    val seg = segmenter.segment _
    
    test("it calculates unweighed edit distance") {
        def unweighed(s1 : String, s2 : String) : Float = {
            if (s1 == s2) 0f else 1f
        }
        //val measure = new EditDistance( unweighed )
        val measure = EditDistance.unweighed
        // Identical strings
        assert(measure.distance("","") == 0)
        assert(measure.distance("a","a") == 0)
        assert(measure.distance("aaaaaaa","aaaaaaa") == 0)
        // Empty comparison
        assert(measure.distance("a","") == 1)
        assert(measure.distance("","aaaa") == 4)
        // Replacement
        assert(measure.distance("a","b") == 1)
        assert(measure.distance("aaaaa","baaaa") == 1)
        assert(measure.distance("aabaa","aaaaa") == 1)
        assert(measure.distance("aaaaa","aaaab") == 1)
        // Insertion & deletion
        assert(measure.distance("akissa","kissab") == 2)
        assert(measure.distance("kissa","kissha") == 1)
        assert(measure.distance("kissa","kiss") == 1)
        assert(measure.distance("AkiBssa","kissaC") == 3)
    }
    
    test("it calcualtes weighed edit distance") {
        //val phonemeMeasure = new PhonemeDistance("data/ipa_features.txt")
        val measure = new EditDistance( (phoneme1, phoneme2) => phonemeMeasure.distance(phoneme1, phoneme2) / phonemeMeasure.MAX_DISTANCE )
        //val segmenter = PronunSegmenterFactory.create()
        //val seg = segmenter.segment _
        
        def segMeasure(s1 : String, s2 : String) : Float = {
            measure.distance(seg(s1), seg(s2))
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
        assert(segMeasure("", "ˈæntəˌnɪm") == 7)
     }
    
    test("it calculates lower bound for edit distance between a string and a prefix") {
        val measure = new EditDistance( (phoneme1, phoneme2) => phonemeMeasure.distance(phoneme1, phoneme2) / phonemeMeasure.MAX_DISTANCE )
    
        def segMeasure(s1 : String, s2 : String) : Float = {
            measure.distance(seg(s1), seg(s2))
        }
        def minMeasure(s1 : String, s2 : String) : Float = {
            measure.minDistance(seg(s1), seg(s2))
        }
    
        assert(minMeasure("ˈæntəˌnɪm", "") == 0)
        assert(minMeasure("", "ˈæntəˌnɪm") == 7)

        assert(minMeasure("ˈɛləfənt","ˈɛlɪfənt")           <= segMeasure("ˈɛləfənt","ˈɛlɪfənt"))
        assert(minMeasure("deɪ","meɪ")                     <= segMeasure("deɪ","meɪ"))
        assert(minMeasure("ˈæntəˌnɪm","ˈsɪnənɪm")          <= segMeasure("ˈæntəˌnɪm","ˈsɪnənɪm"))
        assert(minMeasure("æbˈeɪl.jəˌneɪt","ˌkɑnəˈteɪʃən") <= segMeasure("æbˈeɪl.jəˌneɪt","ˌkɑnəˈteɪʃən"))
    }
}
