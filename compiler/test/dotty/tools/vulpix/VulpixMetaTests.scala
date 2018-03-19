package dotty.tools
package vulpix

import org.junit.Test
import org.junit.experimental.categories.Category
import scala.concurrent.duration._
import dotty.Properties
import TestConfiguration._

/** Meta tests for the Vulpix test suite. This test follows the structure of
 *  CompilationTests.scala. It is meant to be called from bash to diff with
 *  output againts an expected result.
 */
@Category(Array(classOf[dotty.VulpixMetaTests]))
class VulpixMetaTests extends ParallelTesting {
  def maxDuration = 1.seconds
  // Ensure maximum reproducibility.
  def numberOfSlaves = 1
  def safeMode = false // Don't fork separate VMs
  def isInteractive = false // Don't beautify output for interactive use.
  def testFilter = None // Run all the tests.

  implicit val summaryReport: SummaryReporting = new SummaryReport
  implicit def testGroup: TestGroup = TestGroup("VulpixMetaTests")

  @Test def compilePos: Unit = compileFilesInDir("tests/vulpix-tests/meta/pos", defaultOptions).checkCompile()
  @Test def compileNeg: Unit = compileFilesInDir("tests/vulpix-tests/meta/neg", defaultOptions).checkExpectedErrors()
  @Test def runAll: Unit     = compileFilesInDir("tests/vulpix-tests/meta/run", defaultOptions).checkRuns()
}
