import java.io.File
import play.Play
import play.db.DB
import scala.io.Source

object DbSchema {

  def migrate {
    val currentDbVersion = {
      try {
        val rs = DB.executeQuery("select version from schema_version")
        if (!rs.first) 0 else rs.getInt(1)
      }
      catch {
        case ex: Exception => 0
      }
    }
    migrateToLatestVersion(currentDbVersion)
  }

  def migrateToLatestVersion(currentDbVersion:Int) {
    val mdir = new File(Play.applicationPath, Play.configuration.getProperty("brain.db.migrations_dir"))
    val files:Array[File] = mdir.listFiles.sortBy(_.getName)
    val maxVersion = (currentDbVersion /: files) (runMigrationIfNewer)
    DB.execute("update schema_version set version = " + maxVersion)
  }

  def runMigrationIfNewer(currentDbVersion:Int, f:File):Int = {
    val version = Integer.parseInt(f.getName)
    if (version < currentDbVersion)
      runMigration(f)
    version
  }

  def runMigration(f:File) {
    val lines = for (l <- Source.fromFile(f).getLines()
                     if !l.startsWith("--");
                     s = l.trim
                     if !s.isEmpty())
                yield(l)
    lines.mkString(" ").split(";").foreach(DB.execute)
  }

}
