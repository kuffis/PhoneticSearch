import org.scalatest.FunSuite
import java.io.File

class PhoneticSearchSuite extends FunSuite {
  test("it enriches word-pronunciation-pairs to be suitable ML training data") {
    
    var output = new File("data/test.after.txt")
    if (output.exists()) {
      output.delete()
    }

    val items = PhoneticSearchApp.loadTrainingItems("data/test.before.txt", "data/test.after.txt")
    assert(items.trainingItems.length > 100)
    assert(output.exists())

    val items2 = PhoneticSearchApp.loadTrainingItems("data/test.before.txt", "data/test.after.txt")
    assert(items.trainingItems.length == items2.trainingItems.length)
  }
  
  test("it validates words before searching") {
    assert(!PhoneticSearchApp.isSearchable("kïssä"))
    assert( PhoneticSearchApp.isSearchable("kissa"))
  }
}
