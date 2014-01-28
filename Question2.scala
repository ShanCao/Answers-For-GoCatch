import org.specs2.mutable._
import org.specs2.ScalaCheck
import org.scalacheck._, Arbitrary._
import test.util.SpecUtil

object Question2 {
  val mc = new java.math.MathContext(1024)
  val zero = BigDecimal(0, mc)
  val one = BigDecimal(1, mc)

  //f(n)=f(n-1) + f(n-2)*f(1)+.....+ f(1)*f(n-2) + f(n-1)
  
  val bstCount: Int => BigDecimal = f
  
  def f(n:Int) : BigDecimal = 
  		if(n==1)1 else if(n==2) 2 else {
  		 var sum = 2*f(n-1)
  		 for (i <- 1 to n-2) {
  			 sum +=f(n-i-1)*f(i)
  		 }
  		 return sum
  		}
}

object Question2Properties extends Specification with SpecUtil {
  import Question2._

  case class CheckedDomain[A](value: A)

  // These are scalacheck properties.  Your implementations must satisfy these
  // properties.

  val bstCountSmall$ = {
    implicit val arbitraryCheckedDomain = Arbitrary {
      Gen.choose(1, 10).map(CheckedDomain(_))
    }

    Run { (n: CheckedDomain[Int]) =>
      bstCount(n.value) must_== Solution2.bstCount(n.value)
    }
  }

  val bstCountMedium$ = {
    implicit val arbitraryCheckedDomain = Arbitrary {
      Gen.choose(1, 100).map(CheckedDomain(_))
    }

    Run { (n: CheckedDomain[Int]) =>
      bstCount(n.value) must_== Solution2.bstCount(n.value)
    }
  }

  val bstCountLarge$ = {
    implicit val arbitraryCheckedDomain = Arbitrary {
      Gen.choose(1, 1000).map(CheckedDomain(_))
    }

    Run { (n: CheckedDomain[Int]) =>
      bstCount(n.value) must_== Solution2.bstCount(n.value)
    }
  }
}

class Question2Check extends org.specs2.Specification with ScalaCheck with SpecUtil {
  import Question2Properties._

  override def is = {
    "bstCount works for small values of n"             ! bstCountSmall$.check   ^
    "bstCount works for medium values of n"            ! bstCountMedium$.check  ^
    "bstCount works for large values of n"             ! bstCountLarge$.check   ^
    end
  }
}