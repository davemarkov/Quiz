package server.user

import akka.actor.{Actor, ActorRef}
import akka.io.Tcp
import akka.util.ByteString
import play.api.libs.json._
import server.quiz._
import server.storage._
import server.utils.ResponseObserverUtil

case class QuizResults(quizName: String, score: Int)
case class User(username: String, password: String, quizList: List[QuizResults]) extends StorageData

case class Forward(message: String)
case class Logout()
case class Login(username: String, password: String)
case class SignUp(username: String, password: String)
case class RegisterQuizResults()
case class ServiceResponse(message: String)

class UserSession(connection: ActorRef, discussions: ActorRef, dataBase: ActorRef) extends Actor {

  import Tcp._

  connection ! Register(self)
  var subscription: Option[ActorRef] = None
  var loggedInUserData: Option[User] = None
  val databaseResponseHandler: ResponseObserverUtil[DataBaseResponse] = new ResponseObserverUtil()

  def receive: Receive = loggedOutContext()

  def loggedOutContext(): Receive = {
    case Login(username: String, password: String) => login(username, password)
    case SignUp(username: String, password: String) => signUser(username, password)
    case Received(_) =>
      send("Not logged in. You must Log in or Sign in first.")
    case PeerClosed => context.stop(self)
  }

  def send(message: String): Unit = {
    connection ! Write(ByteString(message))
  }

  def login(username: String, password: String): Unit = {
    waitForDataBaseResponse((userData: List[_]) => {
      userData match {
        case List() => send("Invalid credentials.")
        case List(userData@User(_, _, _)) => setLoggedInContext(userData)
        case _ => send("Server Error.")
      }
    })

    dataBase ! Select(User(username, password, List()), MatchAll)
  }

  def loginContext(): Receive = {
    case PeerClosed => context.stop(self)
    case Received(message) => parseJsonMessage(message.decodeString("US-ASCII"))

  }

  def parseJsonMessage(jsonMessage: String): Unit = {

    val parsedMessage = "asda"
//    parsedMessage match {
//      case Logout() => logout()
//      case RegisterQuizResults() => // get the selected questions and compare
//      case Comment(_, _, _) => // send do user
//      case DiscussionHistory(_, history) => // send history to user
//      case Quiz(_, _, _) => // send back to client
//      case RequestHistory() => requestDiscussionHistory()
//      case GetQuiz(quizName) => requestQuiz(quizName)
//    }
  }

  def setLoggedInContext(userData: User): Unit = {
    loggedInUserData match {
      case Some(_) => send("Server Error")
      case None =>
        loggedInUserData = Option(userData)
        context.become(loginContext())
        send("Login successful.")
    }
  }

  def signUser(username: String, password: String): Unit = {
    waitForDataBaseResponse((userData: List[_]) => {
      userData match {
        case List() =>
          val newUser: User = User(username, password, List())
          dataBase ! Create(newUser)
          setLoggedInContext(newUser)
        case List(User(_, _, _)) =>
          send("User name already taken.")
        case _ => send("Server Error.")
      }
    })

    dataBase ! Select(User(username, "", List()), Has)
  }

  def logout(): Unit = {
    loggedInUserData match {
      case Some(value) =>
        unsubscribeFromDiscussion(value.username)
      case None => send("No user logged in.")
    }
    // stop connection
  }

  def waitForDataBaseResponse(completeAction: List[StorageData] => Unit): Unit = {
    context become {
      case PeerClosed => context.stop(self)
      case DataBaseResponse(data) =>
        completeAction(data)
        context.become(loginContext())
    }
  }

  def waitForDiscussionSupervisorResponse(completeAction: DiscussionData => Unit): Unit = {
    context become {
      case PeerClosed => context.stop(self)
      case DiscussionSupervisorResponse(data) =>
        completeAction(data)
        context.become(loginContext())
    }
  }

  def createQuiz(quiz: Quiz): Unit = {
    loggedInUserData match {
      case Some(user) =>
        waitForDataBaseResponse({
          case List() =>
            discussions ! CreateQuiz(user.username, quiz)
            waitForDiscussionSupervisorResponse({
              case DiscussionAddress(quizAddr) => subscription = Option(quizAddr)
              case _ =>
            })
          case _ => () // send to user quizname already exists
        })
        dataBase ! Select(Quiz(quiz.title, "", List()), Has)
      case None => () // error not logged in
    }
  }

  def requestQuiz(quizName: String): Unit = {
    waitForDataBaseResponse({
      case List(Quiz(_, _, _)) => // send only questions to user
      case _ => // no such quiz to user
    })
    dataBase ! Select(Quiz(quizName, "", List()), Has)
  }

  def subscribeToDiscussion(username: String, quizName: String): Unit = {
    subscription match {
      case Some(_) =>
        unsubscribeFromDiscussion(username)
      case None => ()
    }
    waitForDiscussionSupervisorResponse({
      case DiscussionAddress(quiz) => subscription = Option(quiz)
      case _ => ()
    })
    discussions ! GetQuiz(username, quizName)
  }

  def unsubscribeFromDiscussion(username: String): Unit = {
    subscription match {
      case Some(sub) => sub ! Unsubscribe(username)
      case None => () // send responce ... not subscribed
    }
  }

  def requestDiscussionHistory(): Unit = {
    loggedInUserData match {
      case Some(user) => subscription match {
        case Some(sub) =>
          sub ! RequestHistory(user.username)
        case None => send("No subscription found.")
      }
      case None => send("No user logged in.")
    }
  }

  def getAllQuiz(): Unit = {
    waitForDataBaseResponse(quizList => {
      // send all names to user
    })
    dataBase ! Select(Quiz("", "", List()), All)
  }

  def saveQuizResults(quizResults: QuizResults): Unit = {
    loggedInUserData match { // data in quiz users in discussion not updated, better remove it all together
      case Some(user) =>
        dataBase ! Update(User(user.username, "", List(quizResults)))
        loggedInUserData = Option(User(user.username, user.password, quizResults :: user.quizList))
      case None => ()
    }
  }
}
