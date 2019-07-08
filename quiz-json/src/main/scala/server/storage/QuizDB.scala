package server.storage

import server.quiz.Quiz

object QuizDB extends DataBase[Quiz] {
  override var table: List[Quiz] = List()

  override def create(quiz: Quiz): Unit = {
    table = quiz :: table
  }
  override def select(quiz: Quiz, flag: Flag): List[Quiz] = {
    table.filter(quizEntry => {
      flag match {
        case MatchAll =>
          quizEntry.owner == quiz.owner &&
            quizEntry.title == quiz.title
        case Has =>
          quizEntry.owner == quiz.owner ||
            quizEntry.title == quiz.title
        case All => true
      }
    })
  }
  override def delete(quiz: Quiz): Unit = {}
  override def update(quiz: Quiz): Unit = {}
}