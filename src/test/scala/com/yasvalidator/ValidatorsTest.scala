package com.yasvalidator

import org.specs2.mutable.Specification
import Converters._
/**
 * Created by julio on 20/06/15.
 */
class ValidatorsTest extends Specification{

  import Validators.{empty => emptyValidator, _}

  "field validators" >> {
    "reject null or empty strings" >> {
      mandatory(null).isValid === false
      mandatory("  ").isValid === false
    }
    "accept non-empty values" >> {
      mandatory("a").isValid === true
    }
    "reject non-alphas" >> {
      alphanumeric("ab3d.").isValid === false
    }
    "accept alphas" >> {
      alphanumeric("abc1243a").isValid === true
    }
    "reject non numeric" >> {
      numeric("123,").isValid === false
    }
    "accept numeric" >> {
      numeric("342.03").isValid === true
    }
    "length validator" >> {
      maxLen(4)("12341").isValid === false
      maxLen(4)("1234").isValid === true
    }

    "chained validators" >> {
      mandatory.and(maxLen(3)).and(numeric)("").isValid === false
      mandatory.and(maxLen(3)).and(numeric)("qwex").isValid === false
      mandatory.and(maxLen(3)).and(numeric)("qwe ").isValid === false
      mandatory.and(maxLen(3)).and(numeric)("123 ") === Valid("123")
    }

    "disjunctive validators" >> {
      emptyValidator.as(const(null:Integer)).or(numeric.as(integer)).apply("") === Valid(null)
      emptyValidator.as(const(null:Integer)).or(numeric.as(integer)).apply("aaa").isValid === false
      emptyValidator.as(const(null:Integer)).or(numeric.as(integer)).apply("123") === Valid(123)

      mandatory.as(nonNumber.or(const(null:String))).apply("bbb") === Valid("bbb")
      mandatory.as(nonNumber.or(const(null:String))).apply("123") === Valid(null)
      mandatory.as(nonNumber.or(const(null:String))).apply("").isValid === false
    }

  }

  "field converters" >> {
    "convert numbers" >> {
      mandatory.as(integer).apply("123") === Valid(123)
      mandatory.as(long).apply("123") === Valid(123L)
      mandatory.as(double).apply("123") === Valid(123d)
      mandatory.as(double).apply("123.23") === Valid(123.23)
    }

    "boolean converter" >> {
      boolean("True") === Valid(true)
      boolean("abc").isValid === false
      boolean("Yes") === Valid(true)
    }

    "convert date" >> {
      mandatory.as(date("dd-MMM-yy")).apply("12-Oct-15").isValid === true
      date("dd-MMM-yy")("asdfasd").isValid === false
    }
  }


}
