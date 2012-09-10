#!/bin/bash

usage() {
	echo "Script to test installation"
	echo "usage: $0 <eclipse_home> <file_containing_list_of_sites|repository_url>*"
	echo "   <eclipse_home>: an eclipse installation will be performed on"
	echo "   <file_containing_list_of_sites> a file containing a list of p2-friendly URLs of repositories"
	echo "                                   separated by spaces or line breaks"
	echo "   <repository_url>: URL of a p2 repo to install from"
}


# Takes a repo or a directory.xml URL single parameter
install_url() {
	repoName="$1"
	echo "$repoName" | grep ".xml$"
	if [ "$?" -eq 0 ]; then
		#Found .xml suffix
		install_central "$1"
	else
		install_repo "$1"
	fi 
}
	

# Takes repo URL as single parameter
install_repo() {
	echo "Installing content from " $1
		report=TEST-install-$(date +%Y%m%d%H%M).xml
			#Invoke tests
		output=$(java \
 -DUPDATE_SITE=$1 \
 -Dorg.eclipse.swtbot.search.timeout=10000 \
 -Dusage_reporting_enabled=false \
 -Xms256M -Xmx768M -XX:MaxPermSize=512M \
 -jar plugins/org.eclipse.equinox.launcher_*.jar \
 -application org.eclipse.swtbot.eclipse.junit4.headless.swtbottestapplication \
 -testApplication org.eclipse.ui.ide.workbench \
 -product $productName \
 -data workspace/ \
 formatter=org.apache.tools.ant.taskdefs.optional.junit.XMLJUnitResultFormatter,$report \
 formatter=org.apache.tools.ant.taskdefs.optional.junit.PlainJUnitResultFormatter \
 -testPluginName org.jboss.tools.tests.installation \
 -className org.jboss.tools.tests.installation.InstallTest \
 -consoleLog -debug)
		if [[ ! "$output" == *"Failures: 0, Errors: 0"* ]]; then
			echo "Error while installing from " $site ". Read $report for details and see screenshots/"
			popd
			exit 1
		fi
}

# Takes a Central directory.xml URL single parameter
install_central() {
	echo "Installing Central from " $1
		report=TEST-install-$(date +%Y%m%d%H%M).xml
			#Invoke tests
		output=$(java \
 -Djboss.discovery.directory.url=$1 \
 -Dorg.eclipse.swtbot.search.timeout=10000 \
 -Dusage_reporting_enabled=false \
 -Xms256M -Xmx768M -XX:MaxPermSize=512M \
 -jar plugins/org.eclipse.equinox.launcher_*.jar \
 -application org.eclipse.swtbot.eclipse.junit4.headless.swtbottestapplication \
 -testApplication org.eclipse.ui.ide.workbench \
 -product $productName \
 -data workspace/ \
 formatter=org.apache.tools.ant.taskdefs.optional.junit.XMLJUnitResultFormatter,$report \
 formatter=org.apache.tools.ant.taskdefs.optional.junit.PlainJUnitResultFormatter \
 -testPluginName org.jboss.tools.tests.installation \
 -className org.jboss.tools.tests.installation.InstallFromCentralTest \
 -consoleLog -debug)
		if [[ ! "$output" == *"Failures: 0, Errors: 0"* ]]; then
			echo "Error while installing from " $site ". Read $report for details and see screenshots/"
			popd
			exit 1
		fi
}

eclipse_home=$1
shift

if [ ! -d "$eclipse_home" -o ! -d "$eclipse_home/plugins" ]; then
	usage
	exit 2
fi
if (( $# <= 0)); then
	usage
	exit 2
fi

echo "Preparing tests, installing framework"
pushd $eclipse_home
iniFile=$(ls -1 *.ini)
productLineNumber=$(cat $iniFile | grep -n \\-product | cut -f 1 -d :)
let "productLineNumber = $productLineNumber + 1"
productName=$(sed -n ${productLineNumber}p $iniFile)

#install test framework
java -jar plugins/org.eclipse.equinox.launcher_*.jar \
-application org.eclipse.equinox.p2.director \
-repository http://download.eclipse.org/technology/swtbot/helios/dev-build/update-site/,\
http://download.jboss.org/jbosstools/builds/staging/jbosstools-4.0_trunk.component--tests/all/repo/ \
-installIU org.jboss.tools.tests.installation \
-installIU org.eclipse.swtbot.eclipse.test.junit4.feature.group \
-consoleLog


while (($#)); do
	if [ -f "$1" ]; then
		for repoUrl in $(cat "$1"); do
			install_url "$repoUrl"
		done
	else
		install_url "$1"
	fi
	shift
done
popd
exit 0
