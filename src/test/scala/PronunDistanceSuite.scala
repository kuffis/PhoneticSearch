import org.scalatest.FunSuite
import scala.util.Random

class PhonemeDistanceSuite extends FunSuite {
    val measure = new PhonemeDistance("data/ipa_features.txt")

    test("distance between identical phonemes is zero") {
        assert(0.0f == measure.distance("ə","ə"))
    }

    test("distance between similar phonemes is smaller than distance between completely different phonemes") {
        val simDistance = measure.distance("æ","ə")
        val diffDistance = measure.distance("æ","k")
        println("SD:" + simDistance + ", " + diffDistance)
        assert(0 < simDistance)
        assert(simDistance < diffDistance)
        assert(3.0f == simDistance)
        assert(11.0f == diffDistance)
    }
    
    // Distance between empty and ɝ is 25.0
    test("Tries to find biggest possible") {
        val r = new Random()
        val phos = measure.phonemesToFeatures.keys.toList
        var maxDistance = 0f
        for (i <- Range(0, 1000)) {
            var p1 = phos(r.nextInt(phos.length))
            var p2 = phos(r.nextInt(phos.length))
            
            val distance = measure.distance(p1, p2)
            //print("Distance between " + p1 + " and " + p2 + " is " + distance)
            if (distance > maxDistance) {
              maxDistance = distance
              println("Distance between " + p1 + " and " + p2 + " is " + distance)
            }
        }
        assert(maxDistance <= 26f)
    }
}