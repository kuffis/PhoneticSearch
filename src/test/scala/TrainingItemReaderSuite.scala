import org.scalatest.FunSuite

class TrainingItemReaderSuite extends FunSuite {
    test("it produces several outputs if some phoneme is optional") {
        val items = TrainingItemReader.parseTrainingItem("presupposition:/pɹiː.sʌ.pəˈzɪ.ʃ(ə)n/")
        assert(2 == items.length)
        assert("pɹiː.sʌ.pəˈzɪ.ʃən" == items(0).unsplitPronun) // (ə) present
        assert("pɹiː.sʌ.pəˈzɪ.ʃn"  == items(1).unsplitPronun) // (ə) absent

        val items2 = TrainingItemReader.parseTrainingItem("bargirl:/ˈbɑː(ɹ)ɡɜː(ɹ)l/")
        assert(4 == items2.length)
        assert("ˈbɑːɹɡ3ːɹl" == items2(0).unsplitPronun) // (ə) present
        assert("ˈbɑːɹɡ3ːl"  == items2(1).unsplitPronun) // (ə) absent
        assert("ˈbɑːɡ3ːɹl"  == items2(2).unsplitPronun) // (ə) present
        assert("ˈbɑːɡ3ːl"   == items2(3).unsplitPronun) // (ə) absent        
    }

    test("it normalizes the word") {
      val item = TrainingItemReader.parseTrainingItem("able-bodied:/ˈei.bl̩ˌbɑ.did/")
      assert(item(0).unsplitWord == "ablebodied")

      val item2 = TrainingItemReader.parseTrainingItem("Costa Rica:/ˌkoʊ.stə ˈɹi.kə/")
      assert(item2(0).unsplitWord == "costarica")
    }
}
