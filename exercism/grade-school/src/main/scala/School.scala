class School {

  type DB = scala.collection.mutable.Map[Int, Seq[String]]

  // create a private database variable to prevent arbitrary modification
  private val school_db = scala.collection.mutable.Map.empty[Int, Seq[String]]

  // this is our "get" function
  def db: DB = school_db

  // this is our "set" function
  def add(name: String, g: Int): Unit = {

    if (school_db contains g) {
      school_db(g) = school_db(g) :+ name
    } else {
      school_db += (g -> Seq(name))
    }

  }

  def grade(g: Int): Seq[String] = {

    if (school_db contains g) {
      school_db(g)
    } else {
      Seq.empty[String]
    }

  }

  def sorted: DB = {

    school_db.keys.foreach(i => school_db(i) = school_db(i).sorted)

    scala.collection.mutable.Map(school_db.toSeq.sortBy(_._1):_*)

  }
}
