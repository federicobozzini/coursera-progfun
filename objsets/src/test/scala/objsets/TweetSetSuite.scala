package objsets

import org.scalatest.FunSuite


import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TweetSetSuite extends FunSuite {
  trait TestSets {
    val set1 = new Empty
    val set2 = set1.incl(new Tweet("a", "a body", 20))
    val set3 = set2.incl(new Tweet("b", "b body", 20))
    val c = new Tweet("c", "c body", 27)
    val d = new Tweet("d", "d body", 29)
    val set4c = set3.incl(c)
    val set4d = set3.incl(d)
    val set5 = set4c.incl(d)

    val seta1 = set1.incl(c)
    val seta2 = seta1.incl(new Tweet("e", "e body", 9))
    val seta3 = seta2.incl(new Tweet("c", "c body", 4))
    val seta4 = seta3.incl(new Tweet("g", "g body", 5))
  }

  def asSet(tweets: TweetSet): Set[Tweet] = {
    var res = Set[Tweet]()
    tweets.foreach(res += _)
    res
  }

  def size(set: TweetSet): Int = asSet(set).size





  test("union: set1 with set1") {
    new TestSets {
      assert(size(set1.union(set1)) == 0)
    }
  }

  test("union: set1 with set2") {
    new TestSets {
      assert(size(set1.union(set2)) == 1)
    }
  }

  test("union: set2 with set1") {
    new TestSets {
      assert(size(set2.union(set1)) == 1)
    }
  }

  test("union: set4c with set4d") {
    new TestSets {
      assert(size(set4c.union(set4d)) == 4)
    }
  }

  test("union: set4c with set4c") {
    new TestSets {
      assert(size(set4c.union(set4c)) == 3)
    }
  }

  test("final exercise: gizmodo tweets") {
    new TestSets {
      val gizmodoData = TweetReader.ParseTweets.getTweetData("gizmodo", TweetData.gizmodo)
      val techCrunchData = TweetReader.ParseTweets.getTweetData("TechCrunch", TweetData.TechCrunch)
      val gizmodoTweets = TweetReader.toTweetSet(gizmodoData).filter(_.retweets > 15)
      val techCrunchTweets = TweetReader.toTweetSet(techCrunchData).filter(_.retweets > 15)
      assert(size(gizmodoTweets.union(techCrunchTweets)) != 0)
    }
  }

  test("filter: on empty set") {
    new TestSets {
      assert(size(set1.filter(tw => tw.user == "a")) === 0)
    }
  }

  test("filter: on set2 no results") {
    new TestSets {
      assert(size(set2.filter(tw => tw.user == "b")) === 0)
    }
  }

  test("filter: on set2 with results") {
    new TestSets {
      assert(size(set2.filter(tw => tw.user == "a")) === 1)
      assert(set2.filter(tw => tw.user == "a").contains(new Tweet("a", "a body", 20)))
    }
  }

  test("filter: a on set5") {
    new TestSets {
      assert(size(set5.filter(tw => tw.user == "a")) === 1)
    }
  }

  test("filter: a on set5 take all") {
    new TestSets {
      assert(size(set5.filter(tw => true)) === 4)
    }
  }

  test("filter: a on seta2") {
    new TestSets {
      assert(size(seta2.filter(tw => tw.user == "e")) === 1)
    }
  }

  test("filter: a on seta4") {
    new TestSets {
      assert(size(seta4.filter(tw => tw.user == "e")) === 1)
    }
  }

  test("filter: 20 on set5") {
    new TestSets {
      assert(size(set5.filter(tw => tw.retweets == 20)) === 2)
    }
  }

  test("union: set4c and set4d") {
    new TestSets {
      assert(size(set4c.union(set4d)) === 4)
    }
  }

  test("union: with empty set (1)") {
    new TestSets {
      assert(size(set5.union(set1)) === 4)
    }
  }

  test("union: with empty set (2)") {
    new TestSets {
      assert(size(set1.union(set5)) === 4)
    }
  }


  test("union: seta4 with set5") {
    new TestSets {
      assert(size(seta4 union set5) ==  6)
    }
  }


  test("union: set5 with seta4") {
    new TestSets {
      assert(size(set5 union seta4) ==  6)
    }
  }


  test("union: set5 with seta4 and set5") {
    new TestSets {
      assert(size(set5 union seta4 union set5) ==  6)
    }
  }

  test("mostRetweeted: with empty set") {
    new TestSets {
      intercept[java.util.NoSuchElementException] {
        set1.mostRetweeted
      }
    }
  }

  test("mostRetweeted: with non set4c") {
    new TestSets {
        set4c.mostRetweeted == c
    }
  }

  test("mostRetweeted: with set4d") {
    new TestSets {
      set4d.mostRetweeted == d
    }
  }

  test("mostRetweeted: with seta4") {
    new TestSets {
      seta4.mostRetweeted.retweets == 9
    }
  }

  test("mostRetweeted: bug") {
    new TestSets {
      val setbug = seta1.incl(d)
      setbug.mostRetweeted.retweets == 29
    }
  }

  test("descending: empty set") {
    new TestSets {
      val trends = set1.descendingByRetweet
      assert(trends.isEmpty)
    }
  }


  test("descending: set5") {
    new TestSets {
      val trends = set5.descendingByRetweet
      assert(!trends.isEmpty)
      assert(trends.head.user == "d")
    }
  }


  test("descending: seta4") {
    new TestSets {
      val trends = seta4.descendingByRetweet
      assert(!trends.isEmpty)
      assert(trends.head.user == "c")
    }
  }

  test("final exercise: all tweets") {
    new TestSets {
      assert(size(TweetReader.allTweets.filter(_ => true)) != 0)
    }
  }

  test("final exercise: google tweets") {
    new TestSets {
      assert(size(GoogleVsApple.googleTweets) != 0)
    }
  }

  }
