import org.scalatest.FunSuite
import java.io.File

class WekaTrainerSuite extends FunSuite {
  val segmenter = PronunSegmenterFactory.create()        
  val itemData = List[String]("a|b|a|t|o|r:/ə|ˈb|eɪ|t.|ɚ|/", "a|b|a|tt|oir:/ˈæ|b.|ə|ˌt|wɑɹ/", "a|b|a|t|u|r|e:/ˈæ|b.|ə.|t|jʊ|ɹ|/")
  val items = itemData.map(TrainingItemReader.parseTrainingItem).flatten
  val data = new TrainingData(items.map(testify))

  /** Add "test" prefix to all letters to avoid overwriting actual models. */ 
  def testify(item : TrainingItem) : TrainingItem = {
    def testifySplitting( letterAndPhoneme : (String, String) ) : (String, String) = {
      val (letter, phoneme) = letterAndPhoneme 
      ("test" + letter, phoneme)
    }
    val testifiedSplitting = item.splitting.map(testifySplitting)
    return new TrainingItem(testifiedSplitting, item.unsplitWord, item.unsplitPronun, item.isSplit)
  }

  test("it trains a model and generates pronunciations") {
    // Remove old test files
    new File("models").listFiles.map( (f : File) => {
      if (f.getName().contains(".test")) {
        f.delete()
      }
    })
    
    // Train pronunciation model
    val trainer = new WekaTrainer(data)
    trainer.writeToArff()
    // Test that it generates IPA correctly
    val result = trainer.createIpaPronun("testatestbtestatesttttestutestrteste")
    assert(result.startsWith("ˈæb.əˌtjʊɹ"))
  }
}
