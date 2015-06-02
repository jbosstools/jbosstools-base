package org.jboss.tools.common.core.test;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.jboss.jandex.DotName;
import org.jboss.jandex.Index;
import org.jboss.jandex.Indexer;
import org.jboss.tools.common.core.jandex.JandexUtil;
import org.jboss.tools.test.util.ResourcesUtils;
import org.junit.Before;
import org.junit.Test;

public class JandexTest {
	
	IProject project;

	@Before
	public void setUp() throws Exception {
		if(project == null) {
			project = importProject("JarProject");
		}
	}

	private static IProject importProject(String name) throws Exception {
		IProject project = ResourcesUtils.importProject("org.jboss.tools.common.core.test", "projects/" + name);
		assertNotNull(project);
		assertTrue(project.exists());
		return project;
	}

	@Test
	public void testJar() throws Exception {
		IFile f = project.getFile("lib/weld-se-1.1.10.Final.jar");
		assertTrue(f.exists());
		Index index = JandexUtil.createJarIndex(f.getLocation().toFile(), new Indexer());
		assertNotNull(index);
		int classes = index.getAllKnownSubclasses(DotName.createSimple("java.lang.Object")).size();
		assertTrue("More than 2000 classes are expected in weld-se-1.1.10.Final.jar. Found are only " + classes + ".", classes > 1000);
	}
}
