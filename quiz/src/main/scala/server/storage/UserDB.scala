package server.storage

import server.user.User

object UsersDB extends DataBase[User] {
  override var table: List[User] = List()

  override def create(user: User): Unit = {
    table = user :: table
  }
  override def select(user: User, flag: Flag): List[User] = {
    table.filter(userEntry => {
      flag match {
        case MatchAll =>
          userEntry.username == user.username &&
            userEntry.password == user.password
        case Has =>
          userEntry.username == user.username ||
            userEntry.password == user.password
        case All => true
      }
    })
  }
  override def delete(user: User): Unit = {}
  override def update(user: User): Unit = {
    table.map(userEntry => {
      if (userEntry.username == user.username) {
        val updatedQuizList = user.quizList ++ userEntry.quizList
        User(userEntry.username, userEntry.password, updatedQuizList)
      } else {
        userEntry
      }
    })
  }
}
