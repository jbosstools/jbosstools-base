#!/bin/bash

# This script runs as http://hudson.qa.jboss.com/hudson/view/DevStudio/job/jbosstools-cbi-*/configure
# and is archived (for example) in https://pi4soa.svn.sourceforge.net/svnroot/pi4soa/trunk/releng/hudson/run.sh
# Build runs under ${WORKSPACE} == .../jbosstools-cbi-*/workspace

echo "[`date +%Y/%m/%d\ %H:%M`] Hudson job ${JOBNAME} build #${BUILD_NUMBER} (${BUILD_ID}) started." 

##############################################################################################

# BEGIN CONFIGURATION

buildTimestamp="`date +%Y%m%d%H%M`"
commonRelengBranch="HEAD"
basebuilderBranch="r35x_v20090811";
cvsProjectBaseDir=${WORKSPACE} # hudson.qa.jboss.com

# DONE CONFIGURATION

##############################################################################################

uname=$(uname -a); echo $uname
if [[ -f $HUDSON_CONFIG_DIR/scripts/common/common_bash.sh ]]; then
	. $HUDSON_CONFIG_DIR/scripts/common/common_bash.sh
	setant 171
elif [[ -f /opt/hudson/tools/apache-ant-1.7.1/bin/ant ]]; then
        export ANT_HOME=/opt/hudson/tools/apache-ant-1.7.1
elif [[ -f /opt/apache-ant-1.7.1/bin/ant ]]; then
        export ANT_HOME=/opt/apache-ant-1.7.1
fi

# collect Hudson's version of Java as defined globally for a given node 
MY_JAVA_HOME=${JAVA_HOME}

# cache of downloaded requirements and other binaries
downloadsDir="${WORKSPACE}/downloads"; if [[ ! -d $downloadsDir ]]; then mkdir -p $downloadsDir; fi 

# define where to do all the work; start with a fresh folder each time
writableBuildRoot="${WORKSPACE}/build"; if [[ -d ${writableBuildRoot} ]]; then rm -fr ${writableBuildRoot}; fi; mkdir -p $writableBuildRoot 

# need a place to store existing 3rd party jars, eg., ant-contrib.jar (if not in /usr/share/java/)
thirdPartyJarsDir="${WORKSPACE}/3rdPartyJars"; if [[ ! -d $thirdPartyJarsDir ]]; then mkdir -p $thirdPartyJarsDir; fi 

# get org.eclipse.dash.common.releng
if [[ ! -d $cvsProjectBaseDir/org.eclipse.dash.common.releng ]]; then 
	cd $cvsProjectBaseDir
	echo "Check out org.eclipse.dash.common.releng using HEAD"
	cvs -d :pserver:anonymous@dev.eclipse.org:/cvsroot/technology -Q co -r $commonRelengBranch -d org.eclipse.dash.common.releng org.eclipse.dash/athena/org.eclipse.dash.commonbuilder/org.eclipse.dash.commonbuilder.releng
fi 

# get org.eclipse.releng.basebuilder 
if [[ ! -d $cvsProjectBaseDir/org.eclipse.releng.basebuilder ]]; then 
	# network timeout when checking out files
	#echo "Export org.eclipse.releng.basebuilder using $basebuilderBranch"
	#cd $cvsProjectBaseDir; cvs -d :pserver:anonymous@dev.eclipse.org:/cvsroot/eclipse -Q ex -r $basebuilderBranch org.eclipse.releng.basebuilder

	#so get a zip and unpack it instead; zip stored here: http://anonsvn.jboss.org/repos/repository.jboss.org/eclipse/galileo/org.eclipse.releng.basebuilder_${basebuilderBranch}.zip
	if [[ ! -f $downloadsDir/org.eclipse.releng.basebuilder_${basebuilderBranch}.zip ]]; then 
		cd $downloadsDir; wget --no-clobber http://repository.jboss.org/eclipse/galileo/org.eclipse.releng.basebuilder_${basebuilderBranch}.zip
	fi
	cd $cvsProjectBaseDir; unzip -qq -d org.eclipse.releng.basebuilder $downloadsDir/org.eclipse.releng.basebuilder_${basebuilderBranch}.zip
	
	# reuse cached copy if available	
	if [[ -f $thirdPartyJarsDir/org.eclipse.pde.build.svn-1.0.1RC2.zip ]]; then 
		cp $thirdPartyJarsDir/org.eclipse.pde.build.svn-1.0.1RC2.zip $writableBuildRoot
	fi

	# get pde.build.svn plugin (http://sourceforge.net/projects/svn-pde-build/) and unpack into releng.basebuilder's root folder
	pushd $writableBuildRoot >/dev/null
	if [[ ! -f org.eclipse.pde.build.svn-1.0.1RC2.zip ]]; then
		wget --no-clobber http://downloads.sourceforge.net/svn-pde-build/org.eclipse.pde.build.svn-1.0.1RC2.zip
	fi
	unzip -qq org.eclipse.pde.build.svn-1.0.1RC2.zip -d org.eclipse.pde.build.svn
	pushd org.eclipse.pde.build.svn/org.eclipse.releng.basebuilder/ >/dev/null
	#mkdir -p $writableBuildRoot/org.eclipse.releng.basebuilder_${basebuilderBranch}/features 
	mkdir -p $writableBuildRoot/org.eclipse.releng.basebuilder_${basebuilderBranch}/plugins
	for f in $(find . -maxdepth 2 -mindepth 2 -type f); do # remove "-type f" to collect features too; 
		g=${f:2}; 
		if [[ -d $writableBuildRoot/org.eclipse.releng.basebuilder_${basebuilderBranch}/$g ]] || [[ -f $writableBuildRoot/org.eclipse.releng.basebuilder_${basebuilderBranch}/$g ]]; then
			rm -fr $writableBuildRoot/org.eclipse.releng.basebuilder_${basebuilderBranch}/$g;
		fi
		mv -f $g $writableBuildRoot/org.eclipse.releng.basebuilder_${basebuilderBranch}/$g
	done
	popd >/dev/null
	rm -fr org.eclipse.pde.build.svn
	popd >/dev/null
	echo "[start] SVN support added to basebuilder."
fi 

##############################################################################################

# exposed as a Hudson build parameter for convenience
if [[ $BUILDTYPE ]]; then
	buildType="$BUILDTYPE"
else
	buildType="N"
fi

##############################################################################################

# collect required variables 
# could also set them here as variable="some value" static variables, or use 
# export VARIABLE="some value" in Hudson script for convenience when copying/editing jobs

# if NOT hardcoded here, then this script is COMPLETELY boilerplate and need not be copied into the build's .releng folder.

haderror=0
if [[ $PROJECTID ]]; then 
	projectid="$PROJECTID" 
else
	echo "Error: Must set projectid in run.sh or PROJECTID in Hudson config." 
	haderror=1
fi

if [[ $VERSION ]]; then 
	version="$VERSION"
else
	echo "Error: Must set version in run.sh or VERSION in Hudson config." 
	haderror=1
fi

if [[ $PROJRELENGROOT ]]; then
	projRelengRoot="$PROJRELENGROOT"
else
	echo "Error: Must set projRelengRoot in run.sh or PROJRELENGROOT in Hudson config." 
	haderror=1
fi

if [[ $PROJRELENGPATH ]]; then
	projRelengPath="$PROJRELENGPATH"
else
	echo "Error: Must set projRelengPath in run.sh or PROJRELENGPATH in Hudson config." 
	haderror=1
fi

# optional override; assume HEAD or trunk if not specified
if [[ $PROJRELENGBRANCH ]]; then
	projRelengBranch="$PROJRELENGBRANCH"
else
	projRelengBranch="";
fi

# optional override
if [[ $PROJRELENGNAME ]]; then
	projRelengName="$PROJRELENGNAME"
else
	projRelengName="";
fi

if [[ $haderror -gt 0 ]]; then
	exit 1;
fi

# pass in additional flags like -buildAlias=1.0.0RC2 using the $EXTRAFLAGS Hudson parameter
# buildAlias will rename zips from foo-SDK-N200901011234.zip to foo-SDK-1.0.0RC2.zip

##############################################################################################

# define required folders
signingDir="${writableBuildRoot}/signing"; mkdir -p $signingDir 

# long form (default if omitted)
#   buildDir="${writableBuildRoot}/${projectid//.//}/downloads/drops/${version}/${buildType}${buildTimestamp}"
# short form (non-default)
if [[ $SNAPSHOT ]]; then # build in a path that will always be the same (so downstream projects can depend on Update zip)
  buildDir="${writableBuildRoot}/${buildType}-SNAPSHOT"
  EXTRAFLAGS="-buildAlias ${buildType}-SNAPSHOT ${EXTRAFLAGS}"
else
  buildDir="${writableBuildRoot}/${buildType}${buildTimestamp}"
fi

# create required folders & files (as symlinks is possible)
mkdir -p ${downloadsDir} ${signingDir} ${buildDir}

# create .cvspass file to shut up unnecessary warnings
touch ${writableBuildRoot}/.cvspass

#define symlinked required folders
relengBaseBuilderDir="${writableBuildRoot}/org.eclipse.releng.basebuilder"
relengCommonBuilderDir="${writableBuildRoot}/org.eclipse.dash.common.releng"

# symlink basebuilder and common.releng; alternatively, if you omit this, they'll be checked out in start.sh
ln -s ${cvsProjectBaseDir}/org.eclipse.releng.basebuilder ${writableBuildRoot}/
ln -s ${cvsProjectBaseDir}/org.eclipse.dash.common.releng ${writableBuildRoot}/

# symlink 3rdPartyJars (reuse existing content)
ln -s ${thirdPartyJarsDir} ${writableBuildRoot}/
thirdPartyJarsDir="${writableBuildRoot}/3rdPartyJars"

# clean up any *-SNAPSHOT.zip files in ${downloadsDir}
find ${downloadsDir} -maxdepth 1 -type f -name "*-SNAPSHOT.zip" -exec rm -f {} \;

# run a build - may have to pass in "-javaHome /usr/lib/jvm/java" or similar here if default JVM not found
cd ${writableBuildRoot}/org.eclipse.dash.common.releng/tools/scripts
./start.sh -projectid ${projectid} -version ${version} -buildType ${buildType} -buildTimestamp ${buildTimestamp} \
  -writableBuildRoot ${writableBuildRoot} -thirdPartyJarsDir ${thirdPartyJarsDir} -downloadsDir ${downloadsDir} -buildDir ${buildDir} \
  ${projRelengName} ${projRelengRoot} ${projRelengPath} ${projRelengBranch} \
  -thirdPartyDownloadLicenseAcceptance -javaHome ${JAVA16} ${EXTRAFLAGS} 2>&1

# remove file so workspace navigation is one click simpler
rm -f ${writableBuildRoot}/.cvspass

echo ""; echo $uname; echo ""
echo "[`date +%Y/%m/%d\ %H:%M`] Hudson job ${JOBNAME} build #${BUILD_NUMBER} (${BUILD_ID}) done." 
