package com.yasvalidator

import java.lang.{Long => JLong}
import java.lang.{Double => JDouble}
import java.lang.{Boolean => JBoolean}
import java.text.SimpleDateFormat
import java.util.{ResourceBundle, Date}

import com.yasvalidator.Converters._

import scala.util.Try

/**
 * Data Validators and Converters. 
 *
 */
sealed trait ValidationResult[+T]

case class Valid[+T](value:T) extends ValidationResult[T]

case class Invalid[+T](messageKey: String, args:Any*) extends ValidationResult[T]{
  def message(fieldName:String)(implicit res: ResourceBundle):String={
    res.getString(messageKey).format(fieldName, args:_*)
  }
}

object Validators {

  /**
   * An alias for functions that validate and return the validated value or an value error
   * @param T
   */
  type InputValidator[T] = T => ValidationResult[T]


  /**
   * Validator function wrapper that allows combining with other validators
   */
  implicit class RichValidator[T](val validator: InputValidator[T]) extends AnyVal {
    def and(other: InputValidator[T]): InputValidator[T] = (in) => validator(in) match {
      case Valid(v) => other(v)
      case invalid => invalid
    }

    def or(other: InputValidator[T]): InputValidator[T] = in => validator(in) match {
      case r@Valid(_) => r
      case _ => other(in)
    }

    def as[R](converter: ValueConverter[R])(implicit ev1: String =:= T, ev2: T =:= String) = (in: String) => validator(in) match {
      case Valid(v) => converter(v)
      case Invalid(error) => Invalid(error)
    }
  }

  /**
   * A converter function wrapper that allows combining with other converters or validators
   */
  implicit class RichConverter[T](val converter: ValueConverter[T]) extends AnyVal {
    def validating(validator: InputValidator[T]) = converter.andThen {
      case Valid(v) => validator(v)
      case Invalid(e) => Invalid(e)
    }

    def or(other: ValueConverter[T]): ValueConverter[T] = in => converter(in) match {
      case Invalid(_) => other(in)
      case valid => valid
    }
  }

  /**
   * A simple validator that check if the given string is not empty or null
   */
  val mandatory: InputValidator[String] = (in) => if (in == null || in.trim().isEmpty) Invalid("mandatory") else Valid(in.trim())

  /**
   * A simple validator that checks that the given string is empty or null
   */
  val empty: InputValidator[String] = (in) => if (in == null || in.trim().isEmpty) Valid(null) else Invalid("empty")


  /**
   * A function that creates validator for checking values belong to a given list of values
   */
  def oneOf[T](values: T*): InputValidator[T] = (in) => if (values.contains(in)) Valid(in) else Invalid("one-of", in, values.mkString(","))


  /**
   * A validator that checks if the input is numeric
   */
  val numeric: InputValidator[String] = (in) => Try(BigDecimal(in)).map(_ => Valid(in)).getOrElse(Invalid("numeric", in))

  /**
   * A converter to non-numeric
   */
  val nonNumber: InputValidator[String] = in => Try(JDouble.valueOf(in)).map(_ => Invalid("not-numeric", in)).getOrElse(Valid(in))

  /**
   * A validator that checks if the input is alphanumeric
   */
  val alphanumeric: InputValidator[String] = in => if (in.matches("[0-9a-zA-Z]*")) Valid(in) else Invalid("alphanumeric")

  /**
   * A function that creates validators that check if value is in the given range
   */
  def inRange[T <: Comparable[T]](low: T, high: T): InputValidator[T] = d => if (d.compareTo(low) >= 0 && d.compareTo(high) <= 0) Valid(d)
  else Invalid("in-range",low, high)

  /**
   * A function that creates validators that check if input is under the specified length
   */
  def maxLen(size: Int): InputValidator[String] = (in) => if (in.trim.length <= size) Valid(in) else Invalid("max-len", size)

}


/**
 * Convert String input into other types
 */
object Converters{

  /**
   * An alias for functions that convert a string into a value of particular type 'R' or a value error if not convertible
   */
  type ValueConverter[R] = String => ValidationResult[R]

  /**
   * A converter to integer
   */
  val integer: ValueConverter[Integer] = (in) => Try(Integer.valueOf(in.trim)).map(Valid(_)).getOrElse(Invalid("to-integer", in))

  /**
   * A converter to Long
   */
  val long: ValueConverter[JLong] = (in) => Try(JLong.valueOf(in.trim)).map(Valid(_)).getOrElse(Invalid("to-long", in))

  /**
   * A converter to Double
   */
  val double: ValueConverter[JDouble] = in => Try(JDouble.valueOf(in.trim)).map(Valid(_)).getOrElse(Invalid("to-double", in))

  /**
   * A function that creates converters to Date using the specified date format.
   */
  def date(format: String): ValueConverter[Date] = in => Try(new SimpleDateFormat(format).parse(in)).map(Valid(_)).getOrElse(Invalid("to-date", in, format))

  /**
   * A function that creates a converter using the provided function
   */
  def convert[T](f: String => T, messageKey:String="general-convert"): ValueConverter[T] = in => Try(f(in)).map(Valid(_)).getOrElse(Invalid(messageKey, in))

  /**
   * A function that creates converters that map the input to values in the dictionary
   */
  def lookup[T](dictionary: Map[String, T]): ValueConverter[T] = in => dictionary.get(in) match {
    case Some(t) => Valid(t)
    case _ => Invalid("with-lookup", in)
  }

  /**
   * A function that creates converters that returns the provided value instead of converting the input
   */
  def const[T](value: T): ValueConverter[T] = _ => Valid(value)

  /**
   * A convert to boolean
   */
  val boolean: ValueConverter[JBoolean] = in =>
    if ("true".equalsIgnoreCase(in) || "yes".equalsIgnoreCase(in)) Valid(true)
    else if ("false".equalsIgnoreCase(in) || "No".equalsIgnoreCase(in)) Valid(false) else Invalid("to-boolean", in)

}