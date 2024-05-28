package karazin.scala.users.group.week4.homework

object errors:

  type DomainErrors = List[DomainError]
  sealed trait DomainError:
    def errorMessage: String

  case object FilterError extends DomainError:
    def errorMessage: String = "The object doesn't meet the filter condition"

  case object UsernameHasSpecialCharacters extends DomainError:
    def errorMessage: String = "Username cannot contain special characters."

  case object PasswordDoesNotMeetCriteria extends DomainError:
    def errorMessage: String =
      """
        |Password must be at least 10 characters long, including an uppercase and a lowercase letter,
        |one number and one special character.
        |""".stripMargin

  case object InvalidEmailFormat extends DomainError:
    def errorMessage: String = "Email format is invalid. Please enter a valid email address."

  case object AgeNotInRange extends DomainError:
    def errorMessage: String = "Age must be between 18 and 100."

  case object UsernameLengthInvalid extends DomainError:
    def errorMessage: String = "Username must be between 5 and 40 characters long."