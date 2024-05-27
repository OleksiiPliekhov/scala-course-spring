package karazin.scala.users.group.week3.homework
//
// Do not forget to import custom implementation

import karazin.scala.users.group.week3.Functor
import karazin.scala.users.group.week3.future.given
import karazin.scala.users.group.week3.homework.adt.*
import karazin.scala.users.group.week3.homework.model.*
import karazin.scala.users.group.week3.homework.model.Post.PostId
import karazin.scala.users.group.week3.homework.model.User.UserId
import karazin.scala.users.group.week3.homework.services.*

import scala.util.{Failure, Success}
import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, ExecutionContextExecutorService, Future}
import scala.concurrent.ExecutionContext.global

object program extends App:

  val singleThreadPoolContext: ExecutionContextExecutorService =
    ExecutionContext.fromExecutorService(Executors.newSingleThreadExecutor())

  val fixedThreadPoolContext: ExecutionContextExecutorService =
    ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(10))

  given ExecutionContext = ExecutionContext.global
  /*
    Replace `ErrorOr[List[PostView]] result type with `ErrorOrT[Future, List[PostView]]
    Provide the implementation for the service
   */
  def getPostsViews(apiKey: String)(commentsFilter: List[Comment] => Boolean,
                                    likesFilter: List[Like] => Boolean): ErrorOr[List[PostView]] = ???

  /*
    Replace `ErrorOr[PostView] result type with `ErrorOrT[Future, PostView]
    Provide the implementation for the service
  */
  def getPostView(post: Post)(commentsFilter: List[Comment] => Boolean,
                              likesFilter: List[Like] => Boolean)
                 (using ExecutionContext): ErrorOrT[Future, PostView] =
    println(s"Main thread: ${Thread.currentThread().getName}")

    val getCommentsService = getComments(post.postId)(using fixedThreadPoolContext)
    val getLikesService = getLikes(post.postId)(using fixedThreadPoolContext)
    val getSharesService = getShares(post.postId)(using fixedThreadPoolContext)

    for
      comments <- getCommentsService
      if commentsFilter(comments)
      likes <- getLikesService
      if likesFilter(likes)
      shares <- getSharesService
    yield PostView(post, comments, likes, shares)

  getPostView(Post(UserId.generate, PostId.generate))(_ => true, _ => true).value.onComplete {
    case Success(value) => println(s"Completed with $value")
    case Failure(exception) => println(s"Failed with $exception")
  }

  Thread.sleep(10000)

  singleThreadPoolContext.shutdown()
  fixedThreadPoolContext.shutdown()




