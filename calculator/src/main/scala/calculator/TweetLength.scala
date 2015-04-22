package calculator

object TweetLength {
  final val MaxTweetLength = 140

  def tweetRemainingCharsCount(tweetText: Signal[String]): Signal[Int] = {
    Signal[Int](MaxTweetLength - tweetLength(tweetText.apply()))
  }

  def colorForRemainingCharsCount(remainingCharsCount: Signal[Int]): Signal[String] = {
    println(remainingCharsCount.apply())
    remainingCharsCount.apply() match{
      case x if x >= 15 =>
        Signal[String]("green")
      case y if y <= 15 =>
        Signal[String]("orange")
      case z if z < 0 =>
        Signal[String]("red")
      case 140 =>
        println("This motherfucker")
        Signal[String]("yellow")
      case _ =>
        println("This actually fucking failed")
        Signal[String]("white")
    }
  }

  /** Computes the length of a tweet, given its text string.
   *  This is not equivalent to text.length, as tweet lengths count the number
   *  of Unicode *code points* in the string.
   *  Note that this is still a simplified view of the reality. Full details
   *  can be found at
   *  https://dev.twitter.com/overview/api/counting-characters
   */
  private def tweetLength(text: String): Int = {
    /* This should be simply text.codePointCount(0, text.length), but it
     * is not implemented in Scala.js 0.6.2.
     */
    if (text.isEmpty) 0
    else {
      text.length - text.init.zip(text.tail).count(
          (Character.isSurrogatePair _).tupled)
    }
  }
}
