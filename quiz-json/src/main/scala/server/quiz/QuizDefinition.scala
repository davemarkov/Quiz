package server.quiz

import server.storage.StorageData

case class Quiz(title: String, owner: String, questions: List[Question]) extends StorageData
case class Question(answers: List[Answer])
case class Answer(text: String, correct: Boolean)

case class QuizList(quizNames: List[String])