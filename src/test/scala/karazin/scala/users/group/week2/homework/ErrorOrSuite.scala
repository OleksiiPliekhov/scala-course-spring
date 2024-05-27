package karazin.scala.users.group.week2.homework

import munit.ScalaCheckSuite
import org.scalacheck.Prop.*
import munit.ScalaCheckSuite
import org.scalacheck.Prop.*
import adt.ErrorOr
import adt.ErrorOr.Value
import adt.ErrorOr.Error
import org.scalacheck.Gen
import karazin.scala.users.group.model.DummyError
import java.lang

class ErrorOrSuite extends ScalaCheckSuite {

  property("applying pure value returns Value") {
    forAll { (v: String) =>
      ErrorOr(v) == Value(v)
    }
  }

  property("applying value which throws an exception returns Error") {
    forAll { (throwable: Throwable) =>
      val result = try {
        ErrorOr.apply(throw throwable)
      } catch {
        case e: Throwable => ErrorOr.Error(e)
      }
      result == ErrorOr.Error(throwable)
    }
  }

  // Check the property when ErrorOr represents some non-error case and function returns non-error case
  property("flatmap returns Value if `f` returns Value") {
    forAll { (v: Int, f: Int => String) =>
      ErrorOr(v).flatMap(v => ErrorOr(f(v))) == ErrorOr(f(v))
    }
  }

  // Check the property when ErrorOr represents some non-error case but function returns error case
  property("flatmap returns Error if `f` returns Error") {
    forAll { (value: Int, throwable: Throwable) =>
      ErrorOr(value).flatMap(_ => throw throwable) == Error(throwable)
    }
  }

  // Check property when ErrorOr represents some non-error case
  // and function returns non-error case but execution of function fails with some exception
  property("flatmap returns Error if `f` fails") {
    forAll { (v: Int) =>
      Value(v).flatMap(v => throw DummyError) == Error(DummyError)
    }
  }

  // Check the property when ErrorOr represents some error case
  property("flatmap returns Error immediately") {
    propBoolean {
      Error(DummyError).flatMap(_ => throw IllegalArgumentException()) == Error(DummyError)
    }
  }

  // Check the property when ErrorOr represents some non-error case and function returns non-error case
  property("map returns Value if `f` returns Value") {
    forAll { (v: Int, f: Int => String) =>
      Value(v).map(v => f(v)) == Value(f(v))
    }
  }

  // Check property when ErrorOr represents some non-error case
  // and function returns non-error case but execution of function fails with some exception
  property("map returns Error if `f` fails") {
    forAll { (v: Int) =>
      Value(v).map(v => throw DummyError) == Error(DummyError)
    }
  }

  // Check the property when ErrorOr represents some error case
  property("map returns Error immediately") {
    propBoolean {
      Error(DummyError).map(_ => throw DummyError) == Error(DummyError)
    }
  }

  // Fix ??? according to your naming
  // Check the property when ErrorOr represents some value case
  property("withFilter returns Value if applied to Value and predicate is true") {
    forAll { (v: Int) =>
      Value(v).withFilter(_ => true) == Value(v)
    }
  }

  // Fix ??? according to your naming
  // Check the property when ErrorOr represents some value case
  property("withFilter returns Error if applied to Value and predicate is false") {
    forAll { (v: Int) =>
      ErrorOr.Value(v).withFilter(_ => false) match {
        case ErrorOr.Error(e: NoSuchElementException) =>
          e.getMessage == s"Predicate does not hold for $v"
        case _ => false
      }
    }
  }

  // Fix ??? according to your naming
  // Check the property when ErrorOr represents some error case
  property("withFilter returns Error if applied to Error and predicate is true") {
    forAll { (e: Throwable) =>
      Error(e).withFilter(_ => true) == Error(e)
    }
  }

  // Fix ??? according to your naming
  // Check the property when ErrorOr represents some error case
  property("withFilter returns Error if applied to Error and predicate is false") {
    forAll { (e: Throwable) =>
      Error(e).withFilter(_ => false) == Error(e)
    }
  }

  // Fix ??? according to your naming
  // Check the property when a nested ErrorOr represents some value case
  property("flatten returns Value if applied to a nested Value") {
    forAll { (v: Int) =>
      Value(Value(v)).flatten == Value(v)
    }
  }

  // Fix ??? according to your naming
  // Check the property when a nested ErrorOr represents some error case
  property("flatten returns Error if applied to a nested Error") {
    forAll { (e: Throwable) =>
      Value(Error(e)).flatten == Error(e)
    }
  }

  // Fix ??? according to your naming
  // Check the property when ErrorOr represents some error case
  property("flatten returns Error if applied to Error") {
    forAll { (e: Throwable) =>
      Error(e).flatten == Error(e)
    }
  }

  // Fix ??? according to your naming
  // Check the property when a nested value is not an ErrorOr
  test("flatten compilation fails if applied to a non-ErrorOr nested value") {
    val errors = compileErrors("ErrorOr.Value(42).flatten")
    assert(errors.contains("Cannot prove that Int <:< karazin.scala.users.group.week2.homework.adt.ErrorOr[U]"))
  }

  // Fix ??? according to your naming
  // Check the property when ErrorOr represents some value case
  property("foreach applies side effect if applied to Value") {
    forAll { (v: String) =>
      var sideEffect = false
      Value(v).foreach(_ => sideEffect = true)
      sideEffect
    }
  }

  // Fix ??? according to your naming
  // Check the property when ErrorOr represents some error case
  property("foreach ignores side effect if applied to Error") {
    Error(DummyError).foreach(_ => throw IllegalArgumentException())
  }

  // Fix ??? according to your naming
  // Check the property when ErrorOr represents some value case
  property("fold returns the result of the function application if applied to Value") {
    forAll { (v: Int, ifEmpty: Int, f: Int => String) =>
      Value(v).fold(ifEmpty)(f) == f(v)
    }
  }

  // Fix ??? according to your naming
  // Check the property when ErrorOr represents some error case
  property("fold returns default if applied to Error") {
    forAll { (ifEmpty: Int) =>
      Error(DummyError).fold(ifEmpty)(_ => throw DummyError) == ifEmpty
    }
  }

  // Fix ??? according to your naming
  // Check the property when ErrorOr represents some value case
  property("foldLeft returns the result of the function application if applied to Value") {
    forAll { (v: Int, acc: Int, f: (Int, Int) => Int) =>
      Value(v).foldLeft(acc)(f) == f(acc, v)
    }
  }

  // Fix ??? according to your naming
  // Check the property when ErrorOr represents some error case
  property("foldLeft returns acc if applied to Error") {
    forAll { (acc: Int) =>
      Error(DummyError).foldLeft(acc)((_, _) => throw DummyError) == acc
    }
  }

}
