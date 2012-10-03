import org.apache.tools.ant.taskdefs.Java;
import java.text.SimpleDateFormat;

void usage() {
	println "Script to test installation";
	println "usage: groovy.sh testInstall.groovy <eclipse_home> <file_containing_list_of_sites|repository_url|CHECK_FOR_UPDATES>*";
	println "   <eclipse_home>: an eclipse installation will be performed on";
	println "   <file_containing_list_of_sites> a file containing a list of p2-friendly URLs of repositories";
	println "                                   separated by spaces or line breaks";
	println "   <repository_url>: URL of a p2 repo to install from";
	println "   CHECK_FOR_UPDATES: will trigger the check for updates";
}


// Takes a repo or a directory.xml URL single parameter
void installUrl(String repoUrl, File eclipseHome, String product) {
	if (repoUrl.endsWith(".xml")) {
		installFromCentral(repoUrl, eclipseHome, product);
	} else if (repoUrl.equals("CHECK_FOR_UPDATES")) {
		checkForUpdates(eclipseHome, product);
	} else {
		installRepo(repoUrl, eclipseHome, product);
	}
}


//Takes repo URL as single parameter
void installRepo(String repoUrl, File eclipseHome, String productName) {
	println("Installing content from " + repoUrl);
	String additionalVMArgs = "-DUPDATE_SITE=" + repoUrl;
	runSWTBotInstallRoutine(eclipseHome, productName, additionalVMArgs, "org.jboss.tools.tests.installation.InstallTest");
}

void runSWTBotInstallRoutine(File eclipseHome, String productName, String additionalVMArgs, String testClassName) {
	String report = "TEST-install-" + new SimpleDateFormat("yyyyMMddh-hmm").format(new Date()) + ".xml";
	File output = File.createTempFile("install", ".txt");
	output.deleteOnExit();
	// Invoke tests
	Java proc = new org.apache.tools.ant.taskdefs.Java();
	proc.setFork(true);
	proc.setDir(eclipseHome);
	proc.setOutput(output);
	proc.setJvmargs(additionalVMArgs + " " +
			"-Dorg.eclipse.swtbot.search.timeout=10000 " +
			"-Dusage_reporting_enabled=false " +
			"-Xms256M -Xmx768M -XX:MaxPermSize=512M");
	proc.setJar(new File(eclipseHome, "plugins").listFiles().find {it.getName().startsWith("org.eclipse.equinox.launcher_") && it.getName().endsWith(".jar")} );
	proc.setArgs("-application org.eclipse.swtbot.eclipse.junit4.headless.swtbottestapplication " +
			"-testApplication org.eclipse.ui.ide.workbench " +
			"-product " + productName + " " +
			"-data workspace " +
			"formatter=org.apache.tools.ant.taskdefs.optional.junit.XMLJUnitResultFormatter," + report + " " +
			"formatter=org.apache.tools.ant.taskdefs.optional.junit.PlainJUnitResultFormatter " +
			"-testPluginName org.jboss.tools.tests.installation " +
			"-className " + testClassName + " " +
			"-consoleLog -debug");
	proc.init();
	int returnCode = proc.executeJava();
	if (returnCode != 0) {
		println("An error occured. Most probably because of wrong configuration of environment.");
		System.exit(1);
	}

	output.eachLine { line ->
		if (line.contains("Failures:")) {
			if (line.contains("Failures: 0, Errors: 0")) {
				return;
			} else {
				println("Failed to install. Read " + report + " for details and see screenshots/")
				System.exit(1);
			}
		}
	}
}

// Takes a Central directory.xml URL single parameter
void installFromCentral(String discoveryUrl, File eclipseHome, String productName) {
		println("Installing content from " + discoveryUrl);
	String report = "TEST-install-" + new SimpleDateFormat("yyyyMMddh-hmm").format(new Date()) + ".xml";
	runSWTBotInstallRoutine(eclipseHome, productName, "-Djboss.discovery.directory.url" + discoveryUrl, "org.jboss.tools.tests.installation.InstallFromCentralTest");
}

// Check for updates
void checkForUpdates(File eclipseHome, String productName) {
	println("Check for updates");
	runSWTBotInstallRoutine(eclipseHome, productName, "", "org.jboss.tools.tests.installation.CheckForUpdatesTest");
}

if (args.length < 2) {
	usage();
	System.exit(2);
}

File eclipseHome = new File(args[0]);

if (!eclipseHome.isDirectory()) {
	usage();
	System.exit(2);
}

println "Preparing tests, installing framework";

// Install test framework
Java proc = new org.apache.tools.ant.taskdefs.Java();
proc.setDir(eclipseHome);
proc.setFork(true);
proc.setJar(new File(eclipseHome, "plugins").listFiles().find({it.getName().startsWith("org.eclipse.equinox.launcher_") && it.getName().endsWith(".jar")}).getAbsoluteFile());
proc.setArgs("-application org.eclipse.equinox.p2.director " +
		"-repository http://download.eclipse.org/technology/swtbot/helios/dev-build/update-site/," +
		"http://download.jboss.org/jbosstools/builds/staging/jbosstools-4.0_trunk.component--tests/all/repo/ " +
		"-installIU org.jboss.tools.tests.installation " +
		"-installIU org.eclipse.swtbot.eclipse.test.junit4.feature.group " +
		"-consolelog");
proc.init();
int returnCode = proc.executeJava();


File iniFile = eclipseHome.listFiles().find({it.getName().endsWith(".ini")});
iniLines = iniFile.readLines();
targetIndex = iniLines.findIndexOf {line -> line.startsWith("-product") };
String productName = iniLines[targetIndex + 1];
println ("Product is: " + productName);

args[1..-1].each {
	if (new File(it).isFile()) {
		new File(it).eachLine({ line ->
			installUrl(line, eclipseHome, productName);
		});
	} else {
		installUrl(it, eclipseHome, productName);
	}
}
System.exit(0)
