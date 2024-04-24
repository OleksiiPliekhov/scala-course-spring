package karazin.scala.users.group.week1.homework

/* 
  Resources:
  * https://en.wikipedia.org/wiki/Algebraic_data_type
  * https://docs.scala-lang.org/scala3/book/types-adts-gadts.html
*/

object adt:
  
  enum ErrorOr[+V]:

    case Value(v: V)

    case Error(e: Throwable)
    /*
      Two case must be defined:
      * a case for a regular value
      * a case for an error (it should contain an actual throwable)
     */
    
  
    /* 
      The method is used for defining execution pipelines
      Provide a type parameter, an argument and a result type
      
      Make sure that if an internal function is failed with an exception
      the exception is not thrown but the case for an error is returned
    */ 
    def flatMap[Q](f : V => ErrorOr[Q]) : ErrorOr[Q] =
      this match
        case ErrorOr.Value(v) =>
          try
            f(v)
          catch
            case e: Throwable => ErrorOr.Error(e)

        case ErrorOr.Error(e) => ErrorOr.Error(e)
    /* 
      The method is used for changing the internal object
      Provide a type parameter, an argument and a result type
      
      Make sure that if an internal function is failed with an exception
      the exception is not thrown but the case for an error is returned
     */
    def map[Q](f : V => Q): ErrorOr[Q] =
      this match
        case ErrorOr.Value(v) =>
          try
            ErrorOr.Value(f(v))
          catch
            case e: Throwable => ErrorOr.Error(e)

        case ErrorOr.Error(e) => ErrorOr.Error(e)
      
  // Companion object to define constructor
  object ErrorOr:
    /* 
      Provide a type parameter, an argument and a result type
      
      Make sure that if an internal function is failed with an exception
      the exception is not thrown but the case for an error is returned
    */
    def apply[V](v: V): ErrorOr[V] =
      try
        ErrorOr.Value(v)
      catch
        case e: Throwable => ErrorOr.Error(e)

      
  