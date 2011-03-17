package org.jboss.tools.common.model.test;

import java.io.IOException;

import junit.framework.TestCase;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.eclipse.ui.ide.ResourceUtil;
import org.jboss.tools.common.model.XModelObject;
import org.jboss.tools.common.model.util.EclipseResourceUtil;
import org.jboss.tools.test.util.JobUtils;
import org.jboss.tools.test.util.TestProjectProvider;

/**
 *   
 * @author V.Kabanovich
 *
 */
public class ResourceAdapterTest extends TestCase {
	static String BUNDLE_NAME = "org.jboss.tools.common.model.test";
	TestProjectProvider provider = null;
	IProject project = null;

	public ResourceAdapterTest() {}

	public void setUp() throws Exception {
		provider = new TestProjectProvider(BUNDLE_NAME, null, "Test1", true); 
		project = provider.getProject();

		project.refreshLocal(IResource.DEPTH_INFINITE, new NullProgressMonitor());
		
		JobUtils.waitForIdle();
	}

	public void testResourceAdapter() throws CoreException, IOException {
		IFile f = project.getFile(new Path("src/x.properties"));
		assertNotNull(f);
		XModelObject p = EclipseResourceUtil.createObjectForResource(f);
		assertNotNull(p);
		XModelObject a = p.getChildByPath("a");

		// XModelObject should not directly adapt to IResource - 
		IResource r = (IResource)a.getAdapter(IResource.class);
		assertNull(r);

		// There is adapter factory to adapt XModelObject to IResource
		r = (IResource)ResourceUtil.getAdapter(a, IResource.class, true);
		assertEquals(f, r);

		// At present, direct adapting to IFile rises no problem.
		IFile fa = (IFile)a.getAdapter(IFile.class);
		assertEquals(f, fa);

	}

}
