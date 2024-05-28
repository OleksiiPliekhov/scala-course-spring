package karazin.scala.users.group.week4.homework

import cats.data.ValidatedNec
import cats.syntax.all.*
import karazin.scala.users.group.week4.homework.errors.{DomainError, PasswordDoesNotMeetCriteria, UsernameHasSpecialCharacters, InvalidEmailFormat, AgeNotInRange, UsernameLengthInvalid}
import karazin.scala.users.group.week4.homework.model.RegistrationData

object validation:

  type ValidationResult[A] = ValidatedNec[DomainError, A]

  def validateUsername(username: String): ValidationResult[String] =
    if username.matches("^[a-zA-Z0-9]+$") then 
      if username.length >= 5 && username.length <= 40 then username.validNec
      else UsernameLengthInvalid.invalidNec
    else UsernameHasSpecialCharacters.invalidNec

  def validatePassword(password: String): ValidationResult[String] =
    if password.matches("(?=^.{10,}$)((?=.*\\d)|(?=.*\\W+))(?![.\\n])(?=.*[A-Z])(?=.*[a-z]).*$") then password.validNec
    else PasswordDoesNotMeetCriteria.invalidNec

  def validateEmail(email: String): ValidationResult[String] =
    if email.matches("^[\\w-\\.]+@([\\w-]+\\.)+[\\w-]{2,4}$") then email.validNec
    else InvalidEmailFormat.invalidNec

  def validateAge(age: Int): ValidationResult[Int] =
    if age >= 18 && age <= 100 then age.validNec
    else AgeNotInRange.invalidNec

  def validateRegistrationForm(username: String, password: String, email: String, age: Int): Either[List[DomainError], RegistrationData] =
    (validateUsername(username),
      validatePassword(password),
      validateEmail(email),
      validateAge(age)).mapN(RegistrationData.apply).toEither.leftMap(_.toList)
