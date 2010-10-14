/*******************************************************************************
  * Copyright (c) 2010 Red Hat, Inc.
  * Distributed under license by Red Hat, Inc. All rights reserved.
  * This program is made available under the terms of the
  * Eclipse Public License v1.0 which accompanies this distribution,
  * and is available at http://www.eclipse.org/legal/epl-v10.html
  *
  * Contributors:
  *     Red Hat, Inc. - initial API and implementation
  ******************************************************************************/
package org.jboss.tools.common.el.core.test;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IncrementalProjectBuilder;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jdt.core.IField;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.core.IMethod;
import org.eclipse.jdt.core.IType;
import org.eclipse.jdt.core.JavaCore;
import org.jboss.tools.common.el.core.ca.DefaultJavaRelevanceCheck;
import org.jboss.tools.test.util.ResourcesUtils;
import org.jboss.tools.test.util.TestProjectProvider;

import junit.framework.TestCase;

public class RelevanceCheckTest extends TestCase {
	public static final String TEST_PROJECT_NAME = "JavaProject";
	
	public static final String TEST_PROJECT_PATH = "/projects/" + TEST_PROJECT_NAME;

	TestProjectProvider prjProvider = null;
	IProject project = null;

	protected void setUp() throws Exception {
		boolean saveAutoBuild = ResourcesUtils.setBuildAutomatically(false);
		project = (IProject)ResourcesPlugin.getWorkspace().getRoot().findMember(TEST_PROJECT_NAME);
		if(project==null) {
			prjProvider = new TestProjectProvider("org.jboss.tools.common.el.core.test", TEST_PROJECT_PATH, TEST_PROJECT_NAME, true);
			project = prjProvider.getProject();
		}
		
		project.build(IncrementalProjectBuilder.FULL_BUILD, null);
		ResourcesUtils.setBuildAutomatically(saveAutoBuild);
	}

	public void testRelevanceCheck() throws CoreException {
		IJavaProject jp = JavaCore.create(project);
		IType bean = jp.findType("test.Bean");

		IField f = bean.getField("myField");
		assertTrue("Field myField is not found", f != null && f.exists());
		DefaultJavaRelevanceCheck check = new DefaultJavaRelevanceCheck(f);
		
		assertTrue("'myField' should be relevant", check.isRelevant("myField"));
		assertTrue("'myField2' should be relevant", check.isRelevant("myField2"));
		assertFalse("'myFiel' should not be relevant", check.isRelevant("myFiel"));
		
		IMethod m = bean.getMethod("getProp1", new String[0]);
		assertTrue("Method getProp1 is not found", m != null && m.exists());
		check = new DefaultJavaRelevanceCheck(m);
		
		assertTrue("'prop1' should be relevant", check.isRelevant("prop1"));
		assertTrue("'getProp1' should be relevant", check.isRelevant("getProp1"));
		assertFalse("'getProp2' should not be relevant", check.isRelevant("getProp2"));

		m = bean.getMethod("isProp2", new String[0]);
		assertTrue("Method isProp2 is not found", m != null && m.exists());
		check = new DefaultJavaRelevanceCheck(m);
		
		assertTrue("'prop1' should be relevant", check.isRelevant("prop2"));
		assertTrue("'isProp2' should be relevant", check.isRelevant("isProp2"));
		assertFalse("'getProp3' should not be relevant", check.isRelevant("isProp3"));

		m = bean.getMethod("setProp3", new String[]{"QString;"});
		assertTrue("Method setProp3 is not found", m != null && m.exists());
		check = new DefaultJavaRelevanceCheck(m);
		
		assertTrue("'prop1' should be relevant", check.isRelevant("prop3"));
		assertTrue("'setProp3' should be relevant", check.isRelevant("setProp3"));
		assertFalse("'setProp4' should not be relevant", check.isRelevant("setProp4"));
	}

	@Override
	protected void tearDown() throws Exception {
		if(prjProvider!=null) {
			prjProvider.dispose();
		}
	}

}
