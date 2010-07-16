package org.jboss.tools.common.model.test;

import java.io.IOException;

import junit.framework.TestCase;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.jboss.tools.common.model.XModelObject;
import org.jboss.tools.common.model.util.EclipseResourceUtil;
import org.jboss.tools.test.util.JobUtils;
import org.jboss.tools.test.util.TestProjectProvider;

/**
 *   
 * @author V.Kabanovich
 *
 */
public class PropertiesLoaderTest extends TestCase {
	static String BUNDLE_NAME = "org.jboss.tools.common.model.test";
	TestProjectProvider provider = null;
	IProject project = null;

	public PropertiesLoaderTest() {}

	public void setUp() throws Exception {
		provider = new TestProjectProvider(BUNDLE_NAME, null, "Test1", true); 
		project = provider.getProject();

		project.refreshLocal(IResource.DEPTH_INFINITE, new NullProgressMonitor());
		
		JobUtils.waitForIdle();
	}

	public void testMalformedPropertiesFile() throws CoreException, IOException {
		IFile f = project.getFile(new Path("src/x.properties"));
		assertNotNull(f);
		XModelObject p = EclipseResourceUtil.createObjectForResource(f);
		assertNotNull(p);
		XModelObject a = p.getChildByPath("a");
		assertNotNull(a);
		assertEquals(a.getAttributeValue("value"), "valueA");
		XModelObject b = p.getChildByPath("b");
		assertNull(b);
		XModelObject c = p.getChildByPath("c");
		assertNotNull(c);
		assertEquals(c.getAttributeValue("value"), "valueC");
	}

}
