package org.jboss.tools.common.model.test;

import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.util.List;

import junit.framework.TestCase;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Platform;
import org.eclipse.jdt.core.IClasspathEntry;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.core.JavaCore;
import org.jboss.tools.common.model.util.EclipseResourceUtil;
import org.jboss.tools.common.test.util.TestProjectProvider;
import org.jboss.tools.test.util.JobUtils;
import org.osgi.framework.Bundle;

/**
 * Automatic test for JBIDE-1811.
 * Checks that EclipseResourceUtil.getClassPath(IProject) 
 * returns list which includes paths for Eclipse class path entries:
 * 1. jars from the same project;
 * 2. jars from another project in Eclipse work space;
 * 3. external jars.
 *   
 * @author V.Kabanovich
 *
 */
public class ClassPathTest extends TestCase {
	static String BUNDLE_NAME = "org.jboss.tools.common.model.test";
	TestProjectProvider provider1 = null;
	IProject project1 = null;
	TestProjectProvider provider2 = null;
	IProject project2 = null;

	public ClassPathTest() {}

	public void setUp() throws Exception {
		provider1 = new TestProjectProvider(BUNDLE_NAME, null, "Test1", true); 
		project1 = provider1.getProject();

		provider2 = new TestProjectProvider(BUNDLE_NAME, null, "Test2", true); 
		project2 = provider2.getProject();

		project1.refreshLocal(IResource.DEPTH_INFINITE, new NullProgressMonitor());
		project2.refreshLocal(IResource.DEPTH_INFINITE, new NullProgressMonitor());
		
		JobUtils.waitForIdle();
		
		IJavaProject jp = JavaCore.create(project2);
		IClasspathEntry[] es = jp.getRawClasspath();
		
		String location = getLocation("projects/c.jar");
		assertTrue("Cannot find file " + location, new File(location).isFile());
		
		IPath path = new Path(location);
		IClasspathEntry e = JavaCore.newLibraryEntry(path, null, null);
		
		IClasspathEntry[] esn = new IClasspathEntry[es.length + 1];
		System.arraycopy(es, 0, esn, 0, es.length);
		esn[es.length] = e;
		
		jp.setRawClasspath(esn, new NullProgressMonitor());
		
		JobUtils.waitForIdle();
	}

	public void testGetClassPath() throws CoreException, IOException {
		List<String> list = EclipseResourceUtil.getClassPath(project2);
		
		String[] testNames = {
			"/Test2/lib/b.jar",	//1. jar from this project
			"/Test1/lib/a.jar", //2. jar from another project
			"/projects/c.jar"   //3. external jar
		};
		for (int i = 0; i < testNames.length; i++) {
			assertTrue("Cannot find classpath entry " + testNames[i], contains(list, testNames[i]));
		}

	}

	private String getLocation(String relativeInBundle) throws IOException {
		Bundle bundle = Platform.getBundle(BUNDLE_NAME);
		URL url = FileLocator.resolve(bundle.getEntry(relativeInBundle));
		String location = url.getFile();
		return location;
	}

	private boolean contains(List<String> list, String name) {
		for (String s: list) {
			if(s.replace('\\', '/').endsWith(name.replace('\\', '/'))) {
				return true;
			}
		}
		return false;
	}	

}
