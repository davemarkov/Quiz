package server.quiz

import server.storage.StorageData

case class Quiz(title: String, owner: String, users: List[String]) extends StorageData

case class Question(answers: List[Answer])

case class Answer(test: String, correct: Boolean)