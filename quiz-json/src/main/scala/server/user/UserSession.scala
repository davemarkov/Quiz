package server.user

import akka.actor.{Actor, ActorRef}
import akka.io.Tcp
import akka.util.ByteString
import play.api.libs.json._
import server.quiz._
import server.storage._
import server.utils.ResponseObserverUtil
import server.user.MessageHandler._

case class User(username: String, password: String, quizList: List[QuizResults]) extends StorageData

case class MessageType(messageType: String, message: String)
case class Logout()
case class Login(username: String, password: String)
case class SignUp(username: String, password: String)
case class QuizResults(quizName: String, score: Int)
case class Subscribe(quizName: String)
case class Notification(message: String)

class UserSession(parent: ActorRef, connection: ActorRef, discussions: ActorRef, dataBase: ActorRef) extends Actor {

  import Tcp._

  connection ! Register(self)
  var subscription: Option[ActorRef] = None
  var loggedInUserData: Option[User] = None
  val databaseResponseHandler: ResponseObserverUtil[DataBaseResponse] = new ResponseObserverUtil()

  def receive: Receive = defaultContext

  def defaultContext: Receive = {
    case PeerClosed => context.stop(self)
    case Received(message) => handleMessage(message.decodeString("US-ASCII"))
    case History(history) => // send history to user
  }

//  def loggedOutContext(): Receive = {
//    case Login(username: String, password: String) => login(username, password)
//    case SignUp(username: String, password: String) => signUser(username, password)
//    case Received(_) =>
//      send("Not logged in. You must Log in or Sign in first.")
//    case PeerClosed => context.stop(self)
//  }

  def send(message: String): Unit = {
    connection ! Write(ByteString(message))
  }

  def handleMessage(jsonMessage: String): Unit = {
    Json.parse(jsonMessage).as[MessageType] match {
      case MessageType(`LOGIN_MESSAGE`, message) => login(message)
      case MessageType(`LOGOUT_MESSAGE`, _) => logout()
      case MessageType(`SIGUP_MESSAGE`, message) => signUser(message)
      case MessageType(`QUIZ_RESULT_MESSAGE`, message) => saveQuizResults(message)
      case MessageType(`QUIZ_LIST_MESSAGE`, _) => getAllQuiz()
      case MessageType(`COMMENT_MESSAGE`, message) => postComment(message)
      case MessageType(`GET_HISTORY_MESSAGE`, _) => requestDiscussionHistory()
      case MessageType(`SUBSCRIBE_MESSAGE`, message) => subscribeToDiscussion(message)
      case MessageType(`UNSUBSCRIBE_MESSAGE`, _) => unsubscribeFromDiscussion()
      case MessageType(`CREATE_QUIZ_MESSAGE`, message) => createQuiz(message)
      case MessageType(`GET_QUIZ`, message) => requestQuiz(message)
    }
  }

  def login(message: String): Unit = {
    loggedInUserData match {
      case None =>
        val login: Login = Json.parse(message).as[Login]
        waitForDataBaseResponse((userData: List[_]) => {
          userData match {
            case List() => send("Invalid credentials.")
            case List(userData@User(_, _, _)) => setLoggedInContext(userData)
            case _ => send("Server Error.")
          }
        })

        dataBase ! Select(User(login.username, login.password, List()), MatchAll)
      case Some(_) => () // send notification user already logged in
    }
  }

  def signUser(message: String): Unit = {
    loggedInUserData match {
      case None =>
        val signup: SignUp = Json.parse(message).as[SignUp]
        waitForDataBaseResponse((userData: List[_]) => {
          userData match {
            case List() =>
              val newUser: User = User(signup.username, signup.password, List())
              dataBase ! Create(newUser)
              setLoggedInContext(newUser)
            case List(User(_, _, _)) =>
              send("User name already taken.")
            case _ => send("Server Error.")
          }
        })

        dataBase ! Select(User(signup.username, "", List()), Has)
      case Some(_) => () //  errro user logged in
    }
  }

  def setLoggedInContext(userData: User): Unit = {
    loggedInUserData match {
      case Some(_) => send("Server Error")
      case None =>
        loggedInUserData = Option(userData)
        send("Login successful.")
    }
  }

  def logout(): Unit = {
    loggedInUserData match {
      case Some(_) =>
        unsubscribeFromDiscussion()
      case None => send("No user logged in.")
    }
    parent ! RemoveSession(self)
    context.system.terminate()
  }

  def waitForDataBaseResponse(completeAction: List[StorageData] => Unit): Unit = {
    context become {
      case PeerClosed => context.stop(self)
      case DataBaseResponse(data) =>
        completeAction(data)
        context.become(defaultContext)
    }
  }

  def waitForDiscussionSupervisorResponse(completeAction: DiscussionData => Unit): Unit = {
    context become {
      case PeerClosed => context.stop(self)
      case DiscussionSupervisorResponse(data) =>
        completeAction(data)
        context.become(defaultContext)
    }
  }

  def createQuiz(message: String): Unit = {
    loggedInUserData match {
      case Some(user) =>
        val quiz: Quiz = Json.parse(message).as[Quiz]
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
    loggedInUserData match {
      case Some(_) =>
        waitForDataBaseResponse({
          case List(Quiz(_, _, _)) => // send only questions to user
          case _ => // no such quiz to user
        })
        dataBase ! Select(Quiz(quizName, "", List()), Has)
      case None => () // error user not logged in
    }
  }

  def subscribeToDiscussion(message: String): Unit = {
    loggedInUserData match {
      case Some(user) =>
        val subscribe: Subscribe = Json.parse(message).as[Subscribe]
        subscription match {
          case Some(_) =>
            unsubscribeFromDiscussion()
          case None => ()
        }
        waitForDiscussionSupervisorResponse({
          case DiscussionAddress(quiz) => subscription = Option(quiz)
          case _ => ()
        })
        discussions ! GetQuiz(user.username, subscribe.quizName)
    }
  }

  def unsubscribeFromDiscussion(): Unit = {
    loggedInUserData match {
      case Some(user) =>
        subscription match {
          case Some(sub) => sub ! Unsubscribe(user.username)
          case None => () // send responce ... not subscribed
        }
      case None => () // error not logged in
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
    loggedInUserData match {
      case Some(_) =>
        waitForDataBaseResponse(quizList => {
          // send all names to user
        })
        dataBase ! Select(Quiz("", "", List()), All)
      case None => () // user not logged in
    }
  }

  def saveQuizResults(message: String): Unit = {
    loggedInUserData match { // data in quiz users in discussion not updated, better remove it all together
      case Some(user) =>
        val quiz: Quiz = Json.parse(message).as[Quiz]
        waitForDataBaseResponse({
          case List(Quiz(_,_,questions)) =>
            val score: Int = evaluateQuiz(quiz.questions, questions)
            val newQuizResults = QuizResults(quiz.title, score)
            dataBase ! Update(User(user.username, "", List(newQuizResults)))
            loggedInUserData = Option(User(user.username, user.password, newQuizResults :: user.quizList))
          case _ => () // nothing found error

        })
      case None => ()
    }
  }

  def postComment(message: String): Unit = {
    loggedInUserData match {
      case Some(user) =>
        subscription match {
          case Some(sub) =>
            val comment: Comment = Json.parse(message).as[Comment]
            sub ! Comment(user.username, comment.timestamp, comment.text)
          case None => () // error , no subscription
        }
      case None => () // error user not logged in
    }
  }

  def evaluateQuiz(toEval: List[Question], original: List[Question]): Int = {
    37
  }


  def sendDiscussionHistory(discussionHistory: DiscussionHistory): Unit = {
    // parse
  }

  def sendQuiz(quiz: Quiz): Unit = {

  }

  def sendEmptyQuiz(): Unit = {

  }

  def sendQuizList(): Unit = {

  }

  def sendNotification(): Unit = {

  }
}
