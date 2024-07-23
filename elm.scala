/*  Author:     Fabian Huch, TU Muenchen

Elm module for Isabelle.
*/

package isabelle


import java.io.{File => JFile}


object Elm {
  private lazy val elm_home =
    proper_string(Isabelle_System.getenv("ISABELLE_ELM")).getOrElse(error("No elm component found"))

  private lazy val exec = Path.explode(elm_home) + Path.basic("elm")

  object Project {
    def apply(dir: Path, main: Path = Path.explode("src/Main.elm")): Project = {
      if (!dir.is_dir) error("Project directory does not exist: " + dir)
      val main_file = dir + main
      if (!main_file.is_file) error("Main elm file does not exist: " + main_file)
      new Project(dir, main)
    }
  }

  class Project private(dir: Path, main: Path = Path.explode("src/Main.elm")) {
    val definition = JSON.parse(File.read(dir + Path.basic("elm.json")))
    val src_dirs =
      JSON.strings(definition, "source-directories").getOrElse(
        error("Missing source directories in elm.json"))

    def sources: List[JFile] =
      for {
        src_dir <- src_dirs
        path = dir + Path.explode(src_dir)
        file <- File.find_files(path.file, _.getName.endsWith(".elm"))
      } yield file

    def sources_shasum: SHA1.Shasum = {
      val meta_info = SHA1.shasum_meta_info(SHA1.digest(JSON.Format(definition)))
      val source_digest =
        SHA1.shasum_sorted(for (file <- sources) yield SHA1.digest(file) -> file.getCanonicalPath)
      meta_info ::: source_digest
    }

    def build_html(): String = {
      val output = sources_shasum.digest.toString + ".html"
      val cmd =
        File.bash_path(exec) + " make " + File.bash_path(main) + " --optimize --output=" + output
      Isabelle_System.bash(cmd, cwd = dir).check
      val file = dir + Path.explode(output)
      val result = File.read(file)
      Isabelle_System.rm_tree(file)
      result
    }
  }
}