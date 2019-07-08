package server.storage

import server.quiz.DiscussionHistory

object DiscussionDB extends DataBase[DiscussionHistory] {
  override var table: List[DiscussionHistory] = List()

  override def create(history: DiscussionHistory): Unit = {
    table = history :: table
  }
  override def select(history: DiscussionHistory, flag: Flag): List[DiscussionHistory] = {
    table.filter(historyEntry => historyEntry.quizName == history.quizName)
  }
  override def delete(history: DiscussionHistory): Unit = {}
  override def update(history: DiscussionHistory): Unit = {
    table.map(historyEntry => {
      if (historyEntry.quizName == history.quizName) {
        val updatedHistory = history.history ++ history.history
        DiscussionHistory(historyEntry.quizName, updatedHistory)
      } else {
        historyEntry
      }
    })
  }
}
