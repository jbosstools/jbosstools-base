package org.jboss.tools.common.model.test;

import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.util.Collection;

import junit.framework.TestCase;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Platform;
import org.eclipse.jdt.core.IClasspathEntry;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.core.JavaCore;
import org.jboss.tools.common.model.XModelObject;
import org.jboss.tools.common.model.filesystems.FileSystemsHelper;
import org.jboss.tools.common.model.util.EclipseResourceUtil;
import org.jboss.tools.test.util.JobUtils;
import org.jboss.tools.test.util.TestProjectProvider;
import org.osgi.framework.Bundle;

/**
 * Automatic test for JBIDE-1811 & 9906.
 * Checks that EclipseResourceUtil.getAllVisibleLibraries(IProject) 
 * returns list which includes paths for Eclipse class path entries:
 * 1. jars from the same project;
 * 2. jars from another project in Eclipse work space;
 * 3. external jars.
 * 4. exported jars from a parent project
 * 5. exported jars from a project exported by parent project
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
	TestProjectProvider provider4 = null;
	IProject project4 = null;
	TestProjectProvider provider5 = null;
	IProject project5 = null;

	public ClassPathTest() {}

	public void setUp() throws Exception {
		provider1 = new TestProjectProvider(BUNDLE_NAME, null, "Test1", true); 
		project1 = provider1.getProject();

		provider2 = new TestProjectProvider(BUNDLE_NAME, null, "Test2", true); 
		project2 = provider2.getProject();

		provider4 = new TestProjectProvider(BUNDLE_NAME, null, "Test4", true); 
		project4 = provider4.getProject();

		provider5 = new TestProjectProvider(BUNDLE_NAME, null, "Test5", true); 
		project5 = provider5.getProject();

		project1.refreshLocal(IResource.DEPTH_INFINITE, new NullProgressMonitor());
		project2.refreshLocal(IResource.DEPTH_INFINITE, new NullProgressMonitor());
		project4.refreshLocal(IResource.DEPTH_INFINITE, new NullProgressMonitor());
		project5.refreshLocal(IResource.DEPTH_INFINITE, new NullProgressMonitor());
		
		JobUtils.waitForIdle();
		
		IJavaProject jp = JavaCore.create(project2);
		IClasspathEntry[] es = jp.getRawClasspath();
		
		TestProjectProvider provider3 = new TestProjectProvider(BUNDLE_NAME, null, "Test3", true); 
		provider3.getProject();
		
		String location = ResourcesPlugin.getWorkspace().getRoot().getFile(new Path("/Test3/lib/c.jar")).getLocation().toFile().getAbsolutePath();
		assertTrue("Cannot find file " + location, new File(location).isFile());
		
		IPath path = new Path(location);
		IClasspathEntry e = JavaCore.newLibraryEntry(path, null, null);
		
		IClasspathEntry[] esn = new IClasspathEntry[es.length + 1];
		System.arraycopy(es, 0, esn, 0, es.length);
		esn[es.length] = e;
		
		jp.setRawClasspath(esn, new NullProgressMonitor());
		
		JobUtils.waitForIdle();
	}

	public void testSharingJarContent() throws Exception {
		XModelObject fs1 = EclipseResourceUtil.createObjectForResource(project1);
		XModelObject fs2 = EclipseResourceUtil.createObjectForResource(project2);
		
		XModelObject a1 = FileSystemsHelper.getFileSystem(fs1.getModel(), "lib-a.jar");
		XModelObject a2 = FileSystemsHelper.getFileSystem(fs2.getModel(), "lib-a.jar");
		assertTrue(a1.hasChildren());
		assertTrue(a2.hasChildren());
		assertFalse(a1 == a2);
		XModelObject[] c1 = a1.getChildren();
		XModelObject[] c2 = a2.getChildren();
		assertTrue(c1[0] == c2[0]);
		System.out.println(a1.toString() + a2.toString());
	}

	public void testGetClassPath() throws CoreException, IOException {
		Collection<String> list = EclipseResourceUtil.getAllVisibleLibraries(project2);
		
		String[] testNames = {
			"/Test2/lib/b.jar",	//1. jar from this project
			"/Test1/lib/a.jar", //2. jar from another project
			"/Test3/lib/c.jar"  //3. external jar
		};
		for (int i = 0; i < testNames.length; i++) {
			assertTrue("Cannot find classpath entry " + testNames[i], contains(list, testNames[i]));
		}

		//4. exported jars from a parent project
		String testName = "/Test1/lib/a.jar";
		list = EclipseResourceUtil.getAllVisibleLibraries(project4);
		assertTrue("Cannot find classpath entry " + testName, contains(list, testName));

		//5. exported jars from a project exported by parent project
		testName = "/Test1/lib/a.jar";
		list = EclipseResourceUtil.getAllVisibleLibraries(project5);
		assertTrue("Cannot find classpath entry " + testName, contains(list, testName));
	}

	private String getLocation(String relativeInBundle) throws IOException {
		Bundle bundle = Platform.getBundle(BUNDLE_NAME);
		URL url = FileLocator.resolve(bundle.getEntry(relativeInBundle));
		String location = url.getFile();
		return location;
	}

	public void tearDown() {
		provider5.dispose();
		provider4.dispose();
		provider2.dispose();
		provider1.dispose();
	}

	private boolean contains(Collection<String> list, String name) {
		for (String s: list) {
			if(s.replace('\\', '/').endsWith(name.replace('\\', '/'))) {
				return true;
			}
		}
		return false;
	}	

}
