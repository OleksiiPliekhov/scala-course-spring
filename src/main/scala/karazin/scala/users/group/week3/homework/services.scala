package karazin.scala.users.group.week3.homework

import karazin.scala.users.group.week3.Monad
import karazin.scala.users.group.week3.homework.model.Post.PostId
import karazin.scala.users.group.week3.homework.model.User.UserId

import java.util.UUID
import scala.concurrent.{Future, ExecutionContext}

// Do not forget to import custom implementation
import karazin.scala.users.group.week3.homework.adt.*
import karazin.scala.users.group.week3.homework.model.*

//  Dummy services
//
//  The services need to be implemented in case of running the code
// */
object services:
//
//  // Implement and use ApiKey instead of String
//  // Replace `ErrorOr[_]` with ErrorOrT[Future, _]
  def getUserProfile(apiKey: String)
                    (using EC: ExecutionContext): ErrorOrT[Future, UserProfile] =
    ErrorOrT[Future, UserProfile](
      Future {
        println("Start getting user profile...")
        println(s"Current thread for `getUserProfile`: ${Thread.currentThread().getName}")
        Thread.sleep(2000)
        println("Got user profile...")
        ErrorOr.Value(UserProfile(UserId.generate))
      }(EC).recover { case e: Throwable => ErrorOr.Error(e) }
    )

  def getPosts(userId: UserId)
              (using EC: ExecutionContext): ErrorOrT[Future, List[Post]] =
    ErrorOrT[Future, List[Post]](
      Future {
        println("Start getting posts...")
        println(s"Current thread for `getPosts`: ${Thread.currentThread().getName}")
        Thread.sleep(2000)
        println("Got posts...")
        ErrorOr.Value(Post(UserId.generate, PostId.generate) :: Nil)
      }(EC).recover { case e: Throwable => ErrorOr.Error(e) }
    )

  def getComments(postId: PostId)
                 (using EC: ExecutionContext): ErrorOrT[Future, List[Comment]] =
    ErrorOrT[Future, List[Comment]](
      Future {
        println("Start getting comments...")
        println(s"Current thread for `getComments`: ${Thread.currentThread().getName}")
        Thread.sleep(2000)
        println("Got comments...")
        ErrorOr.Value(Comment(UserId.generate, PostId.generate) :: Nil)
      }(EC).recover { case e: Throwable => ErrorOr.Error(e) }
    )

  def getLikes(postId: PostId)
              (using EC: ExecutionContext): ErrorOrT[Future, List[Like]] =
    ErrorOrT[Future, List[Like]](
      Future {
        println("Start getting likes...")
        println(s"Current thread for `getLikes`: ${Thread.currentThread().getName}")
        Thread.sleep(2000)
        println("Got likes...")
        ErrorOr.Value(Like(UserId.generate, PostId.generate) :: Nil)
      }(EC).recover { case e: Throwable => ErrorOr.Error(e) }
    )

  def getShares(postId: PostId)
               (using EC: ExecutionContext): ErrorOrT[Future, List[Share]] =
    ErrorOrT[Future, List[Share]](
      Future {
        println("Start getting shares...")
        println(s"Current thread for `getShares`: ${Thread.currentThread().getName}")
        Thread.sleep(2000)
        println("Got shares...")
        ErrorOr.Value(Share(UserId.generate, PostId.generate) :: Nil)
      }(EC).recover { case e: Throwable => ErrorOr.Error(e) }
    )
