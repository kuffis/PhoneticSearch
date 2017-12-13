import scala.util.Random
import weka.core.Instances
import java.io.FileReader
import java.io.BufferedReader
import java.io.PrintWriter
import java.io.File
import scala.annotation.tailrec

/**
 * Tests the quality of the machine learning model. Compares IPA
 * pronunciations from Wikipedia to those created by the machine learning model.
 * Benchmark measures:
 *  - Deviation per word   (ML vs Wikipedia pronunciation)
 *  - Deviation per letter (ML vs Wikipedia pronunciation)
 */
class TrainingParamTester(val data : TrainingData) {
    val random = new Random()

    def test() {
        val (trainingSet, testSet) = data.trainingItems.partition(item => random.nextBoolean())
        val trainer = new WekaTrainer(new TrainingData(trainingSet))
        trainer.writeToArff()

        var distance = 0f;
        var letterCount = 0
        var wordCount = 0


        val unitMeasure = new PhonemeDistance("data/ipa_features.txt")
        val measure = new EditDistance(unitMeasure.distance)
        val segmenter = PronunSegmenterFactory.create()
        
        val words = testSet.map(_.unsplitWord)
        val pronuns = trainer.createIpaPronuns(words)
        for (index <- Range(0, words.length)) {
            try {
                val pronun = pronuns(index)
                val item = testSet(index)
                distance += measure.distance(segmenter.segment(pronun), segmenter.segment(item.unsplitPronun))
                letterCount += item.unsplitWord.length
                wordCount += 1
            } catch {
                case e:IllegalArgumentException => print("")
            }
        }

        println("Words   in the test set:" + wordCount)
        println("Letters in the test set:" + letterCount)
        println("Phonetic distance:" + distance)
        println("Phonetic distance per word:" + distance / wordCount)
        println("Phonetic distance per letter:" + distance / letterCount)
    }
}
