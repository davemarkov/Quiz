package server.user

import play.api.libs.json._
import server.quiz.{Answer, Comment, DiscussionHistory, Question, Quiz, QuizList}

object MessageHandler {

  val LOGIN_MESSAGE = "login"
  val LOGOUT_MESSAGE = "logout"
  val SIGUP_MESSAGE = "signup"
  val NOTIFICATION_MESSAGE = "notification"
  val QUIZ_MESSAGE = "quiz"
  val QUIZ_RESULT_MESSAGE = "quizResult"
  val QUIZ_LIST_MESSAGE = "quizList"
  val COMMENT_MESSAGE = "comment"
  val DISCUSSION_HISTORY_MESSAGE = "discussionHistory"
  val GET_HISTORY_MESSAGE = "getHistory"
  val SUBSCRIBE_MESSAGE = "subscribe"
  val UNSUBSCRIBE_MESSAGE = "unsubscribe"
  val CREATE_QUIZ_MESSAGE = "createQuiz"
  val GET_QUIZ = "getQuiz"

  implicit object MessageTypeFormat extends Format[MessageType] {
    override def reads(json: JsValue): JsResult[MessageType] = for {
      messageType <- (json \ "messageType").validate[String]
      message <- (json \ "message").validate[String]
    } yield MessageType(messageType, message)

    def writes(o: MessageType): JsValue = Json.obj(
      "messageType" -> o.messageType,
      "message" -> o.message
    )
  }

  // Main
  implicit object LoginFormat extends Format[Login] {
    override def reads(json: JsValue): JsResult[Login] = for {
      username <- (json \ "username").validate[String]
      password <- (json \ "password").validate[String]
    } yield Login(username, password)

    def writes(o: Login): JsValue = Json.obj(
      "username" -> o.username,
      "password" -> o.password
    )
  }

  implicit object SignUpFormat extends Format[SignUp] {
    override def reads(json: JsValue): JsResult[SignUp] = for {
      username <- (json \ "username").validate[String]
      password <- (json \ "password").validate[String]
    } yield SignUp(username, password)

    def writes(o: SignUp): JsValue = Json.obj(
      "username" -> o.username,
      "password" -> o.password
    )
  }

  implicit object NotificationFormat extends Format[Notification] {
    override def reads(json: JsValue): JsResult[Notification] = for {
      message <- (json \ "message").validate[String]
    } yield Notification(message)

    def writes(o: Notification): JsValue = Json.obj(
      "message" -> o.message,
    )
  }

  implicit object SubscribeFormat extends Format[Subscribe] {
    override def reads(json: JsValue): JsResult[Subscribe] = for {
      quizName <- (json \ "quizName").validate[String]
    } yield Subscribe(quizName)

    def writes(o: Subscribe): JsValue = Json.obj(
      "quizName" -> o.quizName,
    )
  }


  // Quiz
  implicit object QuizFormat extends Format[Quiz] {
    override def reads(json: JsValue): JsResult[Quiz] = for {
      title <- (json \ "title").validate[String]
      owner <- (json \ "owner").validate[String]
      questions <- (json \ "questions").validate[List[Question]]
    } yield Quiz(title, owner, questions)


    def writes(o: Quiz): JsValue = Json.obj(
      "title" -> o.title,
      "owner" -> o.owner,
      "questions" -> o.questions
    )
  }

//  implicit object QuestionListFormat extends Format[List[Question]] {
//    override def reads(json: JsValue): JsResult[List[Question]] = for {
//      title <- (json \ "title").validate[String]
//      owner <- (json \ "owner").validate[String]
//      questions <- (json \ "questions").validate[List[Question]]
//    } yield Quiz(title, owner, questions)
//
//
//    def writes(o: List[Question]): JsValue = Json.obj(
//      "title" -> o.title,
//      "owner" -> o.owner,
//      "questions" -> o.questions
//    )
//  }

  implicit object QuestionFormat extends Format[Question] {
    override def reads(json: JsValue): JsResult[Question] = for {
      answers <- (json \ "answers").validate[List[Answer]]
    } yield Question(answers)

    override def writes(o: Question): JsValue = Json.obj(
      "answers" -> o.answers
    )
  }

  implicit object AnswerFormat extends Format[Answer] {
    override def reads(json: JsValue): JsResult[Answer] = for {
      text <- (json \ "text").validate[String]
      correct <- (json \ "correct").validate[Boolean]
    } yield Answer(text, correct)

    override def writes(o: Answer): JsValue = Json.obj(
      "text" -> o.text,
      "correct" -> o.correct
    )
  }

  implicit object QuizResultsFormat extends Format[QuizResults] {
    override def reads(json: JsValue): JsResult[QuizResults] = for {
      quizName <- (json \ "quizName").validate[String]
      score <- (json \ "score").validate[Int]
    } yield QuizResults(quizName, score)

    override def writes(o: QuizResults): JsValue = Json.obj(
      "quizName" -> o.quizName,
      "score" -> o.score
    )
  }

  implicit object QuizListFormat extends Format[QuizList] {
    override def reads(json: JsValue): JsResult[QuizList] = for {
      quizNames <- (json \ "quizNames").validate[List[String]]
    } yield QuizList(quizNames)


    def writes(o: QuizList): JsValue = Json.obj(
      "quizNames" -> o.quizNames,
    )
  }

  // History
  implicit object CommentFormat extends Format[Comment] {
    override def reads(json: JsValue): JsResult[Comment] = for {
      username <- (json \ "username").validate[String]
      timestamp <- (json \ "timestamp").validate[String]
      text <- (json \ "text").validate[String]
    } yield Comment(username, timestamp, text)


    def writes(o: Comment): JsValue = Json.obj(
      "username" -> o.username,
      "timestamp" -> o.timestamp,
      "text" -> o.text
    )
  }

  implicit object DiscussionHistoryFormat extends Format[DiscussionHistory] {
    override def reads(json: JsValue): JsResult[DiscussionHistory] = for {
      quizName <- (json \ "quizName").validate[String]
      history <- (json \ "history").validate[List[Comment]]
    } yield DiscussionHistory(quizName, history)


    def writes(o: DiscussionHistory): JsValue = Json.obj(
      "quizName" -> o.quizName,
      "history" -> o.history
    )
  }
}
