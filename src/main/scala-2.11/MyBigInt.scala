import scala.annotation.tailrec

/**
  * Created by Admin on 08.01.2017.
  */
class MyBigInt extends Ordered[MyBigInt]{
  private var buf = Array[Byte]()
  private var sign: Boolean = true

  def this(str: String) = {
    this()
    if (str.head == '-') {
      sign = false
      buf = str.tail.map(ch => java.lang.Byte.parseByte(ch.toString)).toArray
    } else {
      buf = str.map(ch => java.lang.Byte.parseByte(ch.toString)).toArray
    }
  }

  private def this(buf: Array[Byte], sign: Boolean) = {
    this()
    this.buf = buf
    this.sign = sign
  }

  override def compare(that: MyBigInt): Int = {
    if (this.sign == that.sign) {
      compareOnlyDigits(that)
    } else if (this.sign) 1 else -1
  }

  private def compareOnlyDigits(thisBigInt: MyBigInt, that: MyBigInt): Int = {
    @scala.annotation.tailrec
    def helper(buffer: List[(Byte, Byte)]): Int = buffer match {
      case head::Nil =>
        val (a, b) = head
        if  (a > b) 1
        else if (a < b) -1
        else 0
      case head::tail =>
        val (a, b) = head
        if  (a > b) 1
        else if (a < b) -1
        else helper(tail)
    }
    if (thisBigInt.buf.length > that.buf.length) 1
    else if (thisBigInt.buf.length < that.buf.length) -1
    else helper((thisBigInt.buf zip that.buf).toList)
  }

  private def compareOnlyDigits(that: MyBigInt): Int = compareOnlyDigits(this, that)


  private def prepandZeroes(a: Array[Byte], b: Array[Byte]): Array[(Byte, Byte)] = {
    def helper(elementNumber: Int) = Array.fill[Byte](elementNumber)(0)

    val maxLength =scala.math.max(a.length, b.length)
    (helper(maxLength - a.length) ++ a)
      .zip(helper(maxLength - b.length) ++ b)
  }

  private def plus(buffer :Array[(Byte, Byte)], newSign: Boolean) = {
    val bufToPlus = buffer.reverse
    var div: Int = 0
    val result = bufToPlus.indices.map{i =>
      val (a,b) = bufToPlus(i)
      val newValue = a + b + div
      div = newValue / 10
      (newValue % 10).toByte
    }.reverse.toArray
    new MyBigInt(if (div == 1) (1:Byte) +: result else result, newSign)

  }
  private def minus(buffer :Array[(Byte, Byte)], newSign: Boolean): MyBigInt = {
    val bufToMinus = buffer.reverse
    var div: Int = 0
    val result = bufToMinus.indices.map { i =>
      val (a, b) = bufToMinus(i)
      val aMinusB = a - b
      aMinusB match {
        case num if num < 0 =>
          val add = if (div == 0) 10 else 9
          div = 1
          (num + add).toByte
        case num if (num - div) < 0 =>
          val res = (aMinusB - div + 10).toByte
          div = 1
          res
        case _ =>
          val res = (aMinusB - div).toByte
          div = 0
          res
      }
    }.reverse.toArray
    new MyBigInt(result.splitAt(result.indexWhere(_ != 0))._2, newSign)
  }

  private def multiplication(a: Array[Byte], b: Array[Byte], newSign: Boolean): MyBigInt = {
    val aReverse = a.reverse
    val bReverse = b.reverse
    val arrs = bReverse.par.zipWithIndex.map { case (bElem, index) =>
      var div: Int = 0
      val multi = aReverse.map { aElem =>
        val newValue = bElem * aElem
        val result = ((newValue + div) % 10).toByte
        div = newValue / 10
        result
      }
      if (div != 0) div.toByte +: (multi.reverse ++ Array.fill(index)(0: Byte)) else multi.reverse ++ Array.fill(index)(0: Byte)
    }.reverse
    arrs.foldLeft(new MyBigInt("0")) { case (element, arr) => element + new MyBigInt(arr, newSign)
    }
  }

  def + (that: MyBigInt): MyBigInt = {
    (sign, that.sign) match {
      case (false, false)  => plus(prepandZeroes(buf, that.buf), false)
      case (false, true)   => if (compareOnlyDigits(that) > 0) minus(prepandZeroes(buf, that.buf), false) else minus(prepandZeroes(that.buf, buf), true)
      case (true, false)   => if (compareOnlyDigits(that) > 0) minus(prepandZeroes(buf, that.buf), true) else minus(prepandZeroes(that.buf, buf), false)
      case (true, true)    => plus(prepandZeroes(buf, that.buf), true)
    }
  }

  def - (that: MyBigInt): MyBigInt = {
    (sign, that.sign) match {
      case (false, false) => if (compareOnlyDigits(that) > 0) minus(prepandZeroes(buf, that.buf), false) else minus(prepandZeroes(that.buf, buf), true)
      case (false, true) => plus(prepandZeroes(buf, that.buf), false)
      case (true, false) => plus(prepandZeroes(buf, that.buf), true)
      case (true, true) => if (compareOnlyDigits(that) > 0) {minus(prepandZeroes(buf, that.buf), true)} else minus(prepandZeroes(that.buf, buf), false)
    }
  }

  def * (that: MyBigInt): MyBigInt = {
    (sign, that.sign) match {
      case (false, false) => if (compareOnlyDigits(that) > 0) multiplication(buf,that.buf, true) else multiplication(that.buf, buf,  true)
      case (false, true)  => if (compareOnlyDigits(that) > 0) multiplication(buf,that.buf, false) else multiplication(that.buf, buf, false)
      case (true, false)  => if (compareOnlyDigits(that) > 0) multiplication(buf,that.buf, false) else multiplication(that.buf, buf, false)
      case (true, true)   => if (compareOnlyDigits(that) > 0) multiplication(buf,that.buf, true) else multiplication(that.buf, buf, true)
    }
  }

  def /% (that: MyBigInt): (MyBigInt, MyBigInt) = {
    val aUnsigned = new MyBigInt(this.buf, true)
    val bUnsigned = new MyBigInt(that.buf, true)
    @tailrec
    def helper(a: MyBigInt, b: MyBigInt, acc: MyBigInt):(MyBigInt, MyBigInt) = {
      val aMinusB = a - b
      if (aMinusB.sign) helper(aMinusB, b, acc + new MyBigInt("1")) else (acc, a)
    }
    helper(aUnsigned, bUnsigned, new MyBigInt("0"))
  }

  override def toString: String = if(sign) buf.mkString else "-" ++ buf.mkString
}

object MyBigInt extends App {
  val a  = new MyBigInt("1000000")
  val b = new MyBigInt("123123")
  val c = new MyBigInt("3")

  val x = a*b + c

  println(s"x is: $x")

  val res = x /% b
  val (div, mod) = (res._1, res._2)
  println(s"$x div $b -> $a == $div")
  println(s"$x mod $b -> $c == $mod")
}
