import org.scalatest.FunSuite
import java.io.File

class TrainingParamTesterSuite extends FunSuite { 
  test("it measures various distances between generated and given pronunciations") {
    val items = TrainingItemReader.read("data/ipa.txt").filter(_.isSplit)
    val tester = new TrainingParamTester(new TrainingData(items))
    tester.test()

    // Remove test files so they don't disturb actual searches.
    new File("models").listFiles.map( (f : File) => {
      if (f.getName().contains("pronunciation")) {
        f.delete()
      }
    })
  }
}