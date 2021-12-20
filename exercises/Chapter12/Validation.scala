package Chapter12

import java.util.Date

sealed trait Validation[+E, +A]

case class Failure[E](head: E, tail: Vector[E] = Vector()) extends Validation[E, Nothing]
case class Success[A](a: A) extends Validation[Nothing, A]

object Validation {
     def validationApplicative[E] = new Applicative[({type v[x] = Validation[E, x]})#v] {
        override def unit[A](a: => A): Validation[E, A] = Success(a)

        override def map2[A, B, C](fa: Validation[E, A], fb: Validation[E, B])(f: (A, B) => C): Validation[E, C] = (fa, fb) match {
            case (Success(a), Success(b)) => Success(f(a, b))
            case (Failure(ea, ta), Failure(eb, tb)) => Failure(ea, ta :+ eb :++ tb)
            case (f: Failure[E], _) => f
            case (_, f: Failure[E]) => f
        }
    }
}

case class WebForm(name: String, birthdate: Date, phoneNumber: String)

object WebForm {
    import Validation._

    def validName(name: String): Validation[String, String] =
        if (name != "") Success(name)
        else Failure("name cannot be empty")

    def validBirthdate(birthdate: String): Validation[String, Date] =
        try {
            import java.text._
            Success(new SimpleDateFormat("yyyy-MM-dd").parse(birthdate))
        } catch {
            case _: Throwable => Failure("Birthdate must be in \"yyyy-MM-dd\" format")
        }

    def validPhone(phoneNumber: String): Validation[String, String] =
        if (phoneNumber.matches("[0-9]{10}"))
            Success(phoneNumber)
        else
            Failure("Phone number must be 10 digits")

    def validWebForm(name: String, birthdate: String, phoneNumber: String): Validation[String, WebForm] =
        validationApplicative.map3(
            validName(name),
            validBirthdate(birthdate),
            validPhone(phoneNumber)
        )(WebForm.apply)
}
