// See LICENSE for license details.

package firrtl.options

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success}

class Speculative[A](
  identity: TransformLike[A] with IdentityLike[A],
  speculative: Seq[TransformLike[A]]) extends TransformLike[A] {

  override val name = s"speculating past ${identity.name}"

  final override def transform(a: A): A = {

    val aIdentity: Future[A] = Future {
      identity.transform(a)
    }

    aIdentity.onComplete{
      f => f match {
        case Success(a) => a
        case Failure(a) => throw a
      }
    }

    try {
      speculative.foldLeft(a: A){ case (acc, tx) => tx.transform(acc) }
    } catch {
      case t: Throwable => Await.result(aIdentity, Duration.Inf)
    }

  }

}
