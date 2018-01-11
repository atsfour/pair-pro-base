package example

import org.scalactic.TripleEqualsSupport.Spread
import org.scalatest.{DiagrammedAssertions, FunSpec}

class FrameSpec extends FunSpec with DiagrammedAssertions {

  describe("Frame") {

    describe("fromString") {
      it("fromString") {
        assert(Frame.fromString("11XX3X") == Seq(Normal(1, 1), Strike, Strike, Spare(3)))
        assert(Frame.fromString("0000") == Seq(Normal(0, 0), Normal(0, 0)))
        assert(Frame.fromString("XXXXX") == Seq.fill(5)(Strike))
      }
    }

    describe("score") {

      it("strikeの計算") {
        assert(Strike.score(next = Some(Strike), next2 = Some(Strike)) == 30)
        assert(Strike.score(next = Some(Spare(1)), next2 = Some(Strike)) == 20)
        assert(Strike.score(next = Some(Normal(1, 1)), next2 = None) == 12)
      }

      it("spareの計算") {
        assert(Spare(1).score(next = Some(Normal(5, 4)), next2 = Some(Strike)) == 15)
        assert(Spare(4).score(next = Some(Strike), next2 = None) == 20)
        assert(Spare(4).score(next = Some(Spare(1)), next2 = None) == 11)
        assert(Spare(4).score(next = None, next2 = None) == 10)
      }

      it("Normalの計算") {
        assert(Normal(1, 4).score(next = None, next2 = None) == 5)
        assert(Normal(0, 0).score(next = None, next2 = None) == 0)
      }


    }

  }

}
