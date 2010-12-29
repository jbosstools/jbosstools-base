package org.jboss.tools.common.model.util.test;

import java.util.HashMap;
import java.util.Map;

import junit.framework.TestCase;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.core.IMethod;
import org.eclipse.jdt.core.IType;
import org.jboss.tools.common.model.util.EclipseResourceUtil;
import org.jboss.tools.common.util.BeanUtil;
import org.jboss.tools.common.util.EclipseJavaUtil;
import org.jboss.tools.test.util.JobUtils;
import org.jboss.tools.test.util.TestProjectProvider;

public class EclipseJavaUtilTest extends TestCase {

	static String BUNDLE_NAME = "org.jboss.tools.common.model.test";
	TestProjectProvider provider1 = null;
	IProject project1 = null;

	public void setUp() throws Exception {
		provider1 = new TestProjectProvider(BUNDLE_NAME, null, "Test1", true); 
		project1 = provider1.getProject();

		project1.refreshLocal(IResource.DEPTH_INFINITE, new NullProgressMonitor());
	
		JobUtils.waitForIdle();
	}

	public void testGetters() throws Exception {
		IJavaProject jp = EclipseResourceUtil.getJavaProject(project1);
		assertNotNull(jp);

		IType user = EclipseJavaUtil.findType(jp, "demo.User");
		assertNotNull(user);
		
		IMethod[] ms = user.getMethods();
		Map<String, IMethod> methods = new HashMap<String, IMethod>();
		for (IMethod m: ms) methods.put(m.getElementName(), m);
		
		IMethod m = methods.get("isBooleanValue1");
		assertNotNull(m);
		assertEquals("boolean", EclipseJavaUtil.getMemberTypeAsString(m));
		assertTrue("Method isBooleanValue1() is not recognized as getter", BeanUtil.isGetter(m));
		
		m = methods.get("isBooleanValue2");
		assertNotNull(m);
		assertEquals("java.lang.Boolean", EclipseJavaUtil.getMemberTypeAsString(m));
		assertTrue("Method isBooleanValue2() is not recognized as getter", BeanUtil.isGetter(m));
		
		m = methods.get("isBooleanValue3");
		assertNotNull(m);
		assertEquals("int", EclipseJavaUtil.getMemberTypeAsString(m));
		assertFalse("Method isBooleanValue3() is misrecognized as getter", BeanUtil.isGetter(m));
		
		m = methods.get("getVoid");
		assertNotNull(m);
		assertEquals("void", EclipseJavaUtil.getMemberTypeAsString(m));
		assertFalse("Method getVoid() is misrecognized as getter", BeanUtil.isGetter(m));
		
		m = methods.get("getIntValue");
		assertNotNull(m);
		assertEquals("int", EclipseJavaUtil.getMemberTypeAsString(m));
		assertTrue("Method getIntValue() is not recognized as getter", BeanUtil.isGetter(m));
		
	}

}
