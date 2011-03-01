package org.jboss.tools.tests.performance.startup;

//import org.eclipse.core.tests.runtime.perf.AllTests;

import org.eclipse.core.tests.session.UIPerformanceSessionTestSuite;
import org.jboss.tools.tests.performance.Activator;

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;

/*
 * http://wiki.eclipse.org/Session_Tests
 
  -Dosgi.requiredJavaVersion=1.5
  -XX:MaxPermSize=256m
  -Xms40m
  -Xmx512m
  -Dusage_reporting_enabled=false
  -Dsetup.override.vmArgs=Xms256m;Xmx384m;XX:-UseParallelGC;XX:+AggressiveOpts;XX:-UseConcMarkSweepGC
  -Dsetup.debug=true
  -Dsetup.override.systemProperties=Xverify=none,usage_reporting_enabled=false,eclipse.perf.dbloc=/tmp/db;create=true;dbname=perfdb;dbuser=guest;dbpasswd=guest,eclipse.perf.config=build=3.2_201009010244_201009010244;host=localhost;jvm=sun

   
   
           <property name="perf.baseBuildID" value="3.2_201009010246_201009010246"/><!--3.2_201009010244_201009010244-->
        <property name="perf.buildID" value="3.2_201009010250_201009010250"/>
        <property name="perf.memory" value="-Xms256M -Xmx512M -XX:MaxPermSize=256M"/>
        <property name="perf.db" value="-Declipse.perf.dbloc=${jbt.perf.db.loc};create=true;dbname=perfdb;dbuser=guest;dbpasswd=guest"/>
        <property name="perf.config" value="-Declipse.perf.config=build=${perf.buildID};host=localhost;jvm=sun"/>
        <condition property="perf.baseBuild" value="-Declipse.perf.assertAgainst=build=${perf.baseBuildID};host=localhost;jvm=sun" else="">
            <and>
                <isset property="perf.baseBuildID"/>
                <not>
                    <equals arg1="${perf.baseBuildID}" arg2="" trim="true"/>
                </not>
            </and>
        </condition>
<arg line="-Dvmargs='${perf.memory} ${perf.db} ${perf.config} ${perf.baseBuild}'"/>



 /space/java/sdk/jdk1.6.0_21/bin/java
 -agentlib:jdwp=transport=dt_socket,suspend=y,address=localhost:33469
 -Dosgi.requiredJavaVersion=1.5
 -XX:MaxPermSize=256m
 -Xms256m
 -Xmx384m
 -Dusage_reporting_enabled=false
 -Dsetup.override.vmArgs=Xms256m;Xmx512m;XX:-UseParallelGC;XX:+AggressiveOpts;XX:-UseConcMarkSweepGC
 -Dsetup.debug=true
 -DXXXsetup.override.systemProperties=usage_reporting_enabled=false;Xverify=none
 -Declipse.perf.dbloc=/tmp/db;create=true;dbname=perfdb;dbuser=guest;dbpasswd=guest
 -Declipse.perf.config=build=3.2_201009010247_201009010247;host=localhost;jvm=sun
 -Declipse.perf.assertAgainst=build=3.2_201009010240_201009010240;host=localhost;jvm=sun
 -Dsetup.override.systemProperties=usage_reporting_enabled=false
 -Declipse.pde.launch=true
 -Declipse.p2.data.area=@config.dir/p2
 -Dfile.encoding=UTF-8
 -classpath /home/lukas/latest/beta1/eclipse/plugins/org.eclipse.equinox.launcher_1.1.0.v20100507.jar
 org.eclipse.equinox.launcher.Main
 -os linux
 -ws gtk
 -arch x86
 -nl en_US
 -consoleLog
 -version 3
 -port 38219
 -testLoaderClass org.eclipse.jdt.internal.junit4.runner.JUnit4TestLoader
 -loaderpluginname org.eclipse.jdt.junit4.runtime
 -classNames org.jboss.tools.tests.perf.startup.JBDSStartupTest
 -application org.eclipse.pde.junit.runtime.uitestapplication
 -product com.jboss.jbds.product.product
 -data /home/lukas/work/aworkspace/../junit-workspace
 -configuration file:/home/lukas/work/aworkspace/.metadata/.plugins/org.eclipse.pde.core/pde-junit/
 -dev file:/home/lukas/work/aworkspace/.metadata/.plugins/org.eclipse.pde.core/pde-junit/dev.properties
 -testpluginname org.jboss.tools.tests.perf

 */
public class JBDSStartupTest extends TestCase {
	public static Test suite() {
		System.setProperty("setup.debug", "true");
		System.setProperty("setup.override.systemProperties", "usage_reporting_enabled=false");
		TestSuite ts = new TestSuite("sample");
		ts.addTest(new UIPerformanceSessionTestSuite(Activator.PLUGIN_ID, 4, UIStartupTest.class));
		return ts;
	}

}


/*
Command line: [
	/space/java/sdk/jdk1.6.0_22/jre/bin/java
	-classpath
	/home/lukas/latest/jbdevstudio/studio/plugins/org.eclipse.equinox.launcher_1.1.0.v20100507.jar
	-Dorg.eclipse.update.reconcile=false
	-Dorg.eclipse.ui.testsWaitForEarlyStartup=false
	-Declipse.consoleLog=true
	org.eclipse.core.launcher.Main
	-install
	/home/lukas/latest/jbdevstudio/studio/
	-port
	51776
	-vm
	/space/java/sdk/jdk1.6.0_22/jre/bin/java
	-os
	linux
	-testpluginname
	org.jboss.tools.tests.performance
	-application
	org.eclipse.pde.junit.runtime.uitestapplication
	-arch
	x86
	-dev
	file:/home/lukas/work/wsperf/.metadata/.plugins/org.eclipse.pde.core/pde-junit/dev.properties
	-test
	org.jboss.tools.tests.performance.startup.UIStartupTest:testUIApplicationStartup
	-data
	/tmp/workspace
	-ws
	gtk
	-version
	3
	-configuration
	file:/home/lukas/work/wsperf/.metadata/.plugins/org.eclipse.pde.core/pde-junit/
	-nl
	en_US
]
*/