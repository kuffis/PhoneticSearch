import org.scalatest.FunSuite

class PronunTreeFactorySuite extends FunSuite {
    test("it constructs a tree") {
        val segmenter = PronunSegmenterFactory.create()
        
        val itemData = List[String]("a|b|a|t|o|r:/ə|ˈb|eɪ|t.|ɚ|/", "a|b|a|tt|oir:/ˈæ|b.|ə|ˌt|wɑɹ/", "a|b|a|t|u|r|e:/ˈæ|b.|ə.|t|jʊ|ɹ|/")
        val items = itemData.map(TrainingItemReader.parseTrainingItem).flatten
        val data = new TrainingData(items)
        
        val tree = PronunTreeFactory.create(data, segmenter)
        //tree.recDebug(1)
        assert(tree.subnodes.size == 2)
        assert(tree.subnodes("ə").subnodes.size == 1)
        assert(tree.subnodes("ˈæ").subnodes.size == 1)

        val abatureItem = tree.traverse("ˈæ|b.|ə.|t|j|ʊ|ɹ".split('|').toList)
        assert(abatureItem.length == 1)
        assert(abatureItem(0).unsplitWord == "abature")
    }
}
