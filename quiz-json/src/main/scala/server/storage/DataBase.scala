package server.storage

import akka.actor.{Actor, ActorRef}
import server.quiz.{DiscussionHistory, Quiz}
import server.user.User


trait DataBase[A] {
  protected var table: List[A]

  def create(data: A): Unit
  def select(data: A, flag: Flag): List[A]
  def delete(data: A): Unit
  def update(data: A): Unit
}

trait Flag
case object MatchAll extends Flag
case object All extends Flag
case object Has extends Flag

abstract class StorageData
case class Select(data: StorageData, flag: Flag)
case class Create(data: StorageData)
case class Update(data: StorageData)
case class Delete(data: StorageData, flag: Flag)
case class DataBaseResponse(data: List[StorageData])

class QuizDataBase extends Actor {

  def receive: Receive = {
    case Select(data: StorageData, flag: Flag) => select(data, flag, sender)
    case Create(data: StorageData) => create(data, sender)
    case Update(data: StorageData) => update(data)
    case Delete(data: StorageData, flag: Flag) => delete(data, flag, sender)
  }

  def select(data: StorageData, flag: Flag, sender: ActorRef): Unit = {
    data match {
      case user @ User(_, _, _) =>
        respond(sender, UsersDB.select(user, flag))
      case history @ DiscussionHistory(_, _) =>
        respond(sender, DiscussionDB.select(history, flag))
      case quiz @ Quiz(_, _, _) =>
        respond(sender, QuizDB.select(quiz, flag))
    }
  }

  def create(data: StorageData, sender: ActorRef): Unit = {
    data match {
      case user @ User(_, _, _) => UsersDB.create(user)
      case history @ DiscussionHistory(_, _) => DiscussionDB.create(history)
      case quiz @ Quiz(_, _, _) => QuizDB.create(quiz)
    }
  }

  def update(data: StorageData): Unit = {
    data match {
      case user @ User(_, _, _) => UsersDB.update(user)
      case history @ DiscussionHistory(_, _) => DiscussionDB.update(history)
      case quiz @ Quiz(_, _, _) => QuizDB.update(quiz)
    }
  }

  def delete(data: StorageData, flag: Flag, sender: ActorRef): Unit = {
    data match {
      case user @ User(_, _, _) => UsersDB.delete(user)
      case history @ DiscussionHistory(_, _) => DiscussionDB.delete(history)
      case quiz @ Quiz(_, _, _) => QuizDB.delete(quiz)
    }
  }

  def respond(to: ActorRef, message: List[StorageData]): Unit = {
    to ! message
  }
}