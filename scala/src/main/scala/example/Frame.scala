package example

sealed trait Frame{
  def score(next: Option[Frame], next2: Option[Frame]): Int
}

case object Strike extends Frame {
  def score(next: Option[Frame], next2: Option[Frame]): Int = next match {
    case None => 10
    case Some(Strike) => next2 match {
      case None => 20
      case Some(Strike) => 30
      case Some(Spare(n)) => 20 + n
      case Some(Normal(n1, _)) => 20 + n1
    }
    case Some(Spare(n)) => 20
    case Some(Normal(n1, n2)) => 10 + n1 + n2
  }
}
case class Spare(first: Int) extends Frame {
  def score(next: Option[Frame], next2: Option[Frame]): Int = next match {
    case None => 10
    case Some(Strike) => 20
    case Some(Spare(n)) => 10 + n
    case Some(Normal(n1, _)) => 10 + n1
  }
}
case class Normal(first: Int, second: Int) extends Frame {
  def score(next: Option[Frame], next2: Option[Frame]): Int = first + second
}

object Frame {
  def fromString(str: String): Seq[Frame] = {
    def loop(buff: Seq[Frame], rest: String) = rest match {

    }
  }
}

sealed trait ScoreString
case class IntLikeString(num: Int) extends ScoreString
case object XString extends ScoreString
object ScoreString {
  def apply(c: Char): ScoreString = {
    if (c == 'X') XString
    else if (c >= '0' && c <= '9') IntLikeString(c.toString.toInt)
    else throw new Exception("エラーだよ")
  }
}
