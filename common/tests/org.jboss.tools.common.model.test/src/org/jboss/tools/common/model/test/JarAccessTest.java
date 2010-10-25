package org.jboss.tools.common.model.test;

import java.io.File;
import java.util.List;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.jboss.tools.common.model.filesystems.impl.TestJarAccess;
import org.jboss.tools.test.util.JobUtils;
import org.jboss.tools.test.util.TestProjectProvider;

import junit.framework.TestCase;

public class JarAccessTest extends TestCase {
	static String BUNDLE_NAME = "org.jboss.tools.common.model.test";
	TestProjectProvider provider1 = null;
	IProject project1 = null;

	public void setUp() throws Exception {
		provider1 = new TestProjectProvider(BUNDLE_NAME, null, "TestJar", true); 
		project1 = provider1.getProject();

		project1.refreshLocal(IResource.DEPTH_INFINITE, new NullProgressMonitor());
		
		JobUtils.waitForIdle();
	}

	public void testJarAccess() throws Exception {
		IFile jar = project1.getFile(new Path("lib/standard.jar"));
		assertTrue(jar.exists());
		String file = jar.getLocation().toFile().getAbsolutePath();
		assertTrue(new File(file).isFile());
		TestJarAccess test = new TestJarAccess(file);
		test.runAll();
		List<String> errors = test.getErrors();
		String message = "There were errors:";
		for (String error: errors) {
			message += "\n\t" + error;
		}
		assertTrue(message, errors.isEmpty());
	}
}
