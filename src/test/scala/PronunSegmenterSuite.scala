import org.scalatest.FunSuite
import scala.io.Source

class PronunSegmenterSuite extends FunSuite {
    def mainFeas(f : String) : String = {
        f.substring(0, f.length - 4)
    }
    def extraFeas(f : String) : String = {
        f.substring(f.length - 4)
    }

    test("it segments wikipedia pronunciations") {
        val segmenter = PronunSegmenterFactory.create()

        // In the beginning was the Word.
        assert(segmenter.segment("wɝd") == List("w","ɝ","d"))
    }

    test("it segments primary stress and adds it to features") {
        val segmenter = PronunSegmenterFactory.create()
        assert(segmenter.segment("tuˈniʒə") == List("t","u","ˈn","i","ʒ","ə"))

        val feas  = segmenter.features("ˈn")
        val feas2 = segmenter.features("n")
        assert(mainFeas(feas)  == mainFeas(feas2))
        assert(extraFeas(feas) != extraFeas(feas2))
        assert(extraFeas(feas) == "+--0")
    }

    test("it segments secondary stress and adds it to features") {
        val segmenter = PronunSegmenterFactory.create()
        assert(segmenter.segment("ˈɹeɪdiˌoʊ") == List("ˈɹ","e","ɪ","d","i","ˌo","ʊ"))

        val feas  = segmenter.features("ˌo")
        val feas2 = segmenter.features("o")
        assert(mainFeas(feas)  == mainFeas(feas2))
        assert(extraFeas(feas) != extraFeas(feas2))
        assert(extraFeas(feas) == "-+-0")
    }

    test("it segments syllable pause and adds it to features") {
        val segmenter = PronunSegmenterFactory.create()
        assert(segmenter.segment("ˈfi.dʒi") == List("ˈf","i.","d","ʒ","i"))

        val feas  = segmenter.features("i.")
        val feas2 = segmenter.features("i")
        assert(mainFeas(feas)  == mainFeas(feas2))
        assert(extraFeas(feas) != extraFeas(feas2))
        assert(extraFeas(feas) == "--+0")
    }

    test("it segments long vowel mark and adds it to features") {
        val segmenter = PronunSegmenterFactory.create()
        assert(segmenter.segment("fɔːls") == List("f","ɔː","l","s"))

        val feas  = segmenter.features("ɔː")
        val feas2 = segmenter.features("ɔ")
        assert(mainFeas(feas)  == mainFeas(feas2))
        assert(extraFeas(feas) != extraFeas(feas2))
        assert(extraFeas(feas) == "---+")
    }

    test("it segments handso") {
        //val segmenter = PronunSegmenterFactory.create()
        //segmenter.segment("ˈhaɪdɹədʒən") // Ensure that ə itself is not a problem. 
        //assert(segmenter.segment("ˈhɑn.dʒ‌ə") == List("ˈh","ɑ","n.","dʒ‌","ə"))
    }

    def myErrHandler(e : Exception, pronun : String) {
      //println("Error segmenting " + pronun + ": " + e.getMessage());
    }

    test("it segments all pronunciations in wiktionary") {
        var kissa  = List[String]()
        val segmenter = PronunSegmenterFactory.create()
        var skipCounter = 0
        val trainingData = TrainingItemReader.read("data/ipa.txt")
        //for (____line <- Source.fromFile("data/ipa.txt").getLines) {
        for (trainingItem <- trainingData) {
          var line = ":/" + trainingItem.unsplitPronun + "/"

          // Accents belong before the stressed phoneme, pauses after.
          // Remove misplaced accent and pause marks.
          line = line.replace("ˌ/","/").replace("/.","/")
          line = line.replace("..",".") // Remove double pauses.
          line = line.replace("_","")

          val pronun = line.substring(2, line.length - 1)
          val pronun2 = Util.join(pronun.split('|'))
          //println("Segmenting " + pronun2)

          try {
              segmenter.segment(pronun2)
              kissa = pronun2 :: kissa
          } catch {
              case e:IllegalArgumentException => myErrHandler(e, pronun2)
              skipCounter += 1
          }
        }
        println("Skipped lines:" + skipCounter)
        assert(skipCounter <= 160)
        //println(Util.join(kissa.take(100).toArray, "/ /"))
    }
}
