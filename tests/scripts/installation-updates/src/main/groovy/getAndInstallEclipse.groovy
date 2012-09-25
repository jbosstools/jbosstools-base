import groovy.util.AntBuilder
import org.apache.tools.ant.taskdefs.Get;

File eclipseCacheDirectory = System.properties['eclipseCacheDirectory'] != null ? new File(System.properties['eclipseCacheDirectory']) : new File(".");
String eclipseFlavour = System.properties['eclipseFlavour'] != null ? System.properties['eclipseFlavour'] : "jee";
String releaseTrainId = System.properties['releaseTrainId'] != null ? System.properties['releaseTrainId'] : "juno";
String versionLabel = System.properties['versionLabel'] != null ? System.properties['versionLabel'] : "R";

String osLabel = System.properties['os.name'].toLowerCase();
String fileExtension = null;
if (osLabel.contains("windows")) {
	osLabel = "win32";
	fileExtension = "zip";
} else if (osLabel.contains("linux")) {
	osLabel = "linux-gtk";
	fileExtension = "tar.gz";
} else if (osLabel.contains("mac")) {
	osLabel = "macosx-cocoa";
	fileExtension = "tar.gz";
}
String archLabel = System.properties['os.arch'].contains("64") ? "-x86_64" : "";

String eclipseArchive = "eclipse-" + eclipseFlavour + "-" + releaseTrainId + "-" + osLabel + archLabel + "." + fileExtension;
String downloadURL = "http://www.eclipse.org/downloads/download.php?r=1&file=/technology/epp/downloads/release/" + releaseTrainId + "/" + versionLabel +"/" + eclipseArchive;
println("Will retrieve " + eclipseArchive)

File cachedFile = new File(eclipseCacheDirectory, eclipseArchive);
if (!cachedFile.isFile()) {
	new AntBuilder().get(
		src: downloadURL,
		dest: cachedFile);
	if (fileExtension.equals("tar.gz")) {
		new AntBuilder().gunzip(src: cachedFile.getAbsolutePath());
	}
}
// Unzip
if (fileExtension.equals("zip")) {
	new AntBuilder().unzip(
		src: cachedFile.getAbsolutePath(),
		dest: new File().getAbsolutePath());
} else if (fileExtension.equals("tar.gz")) {
	new AntBuilder().untar(
		src: cachedFile.getAbsolutePath()[0..- (".gz".length() + 1)],
		dest: new File(".").getAbsolutePath());
}