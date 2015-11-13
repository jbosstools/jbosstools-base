/******************************************************************************* 
 * Copyright (c) 2015 Red Hat, Inc. 
 * Distributed under license by Red Hat, Inc. All rights reserved. 
 * This program is made available under the terms of the 
 * Eclipse Public License v1.0 which accompanies this distribution, 
 * and is available at http://www.eclipse.org/legal/epl-v10.html 
 * 
 * Contributors: 
 * Red Hat, Inc. - initial API and implementation 
 ******************************************************************************/
package org.jboss.tools.common.core.test;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import org.eclipse.core.resources.IProject;
import org.eclipse.jdt.core.IField;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.core.IType;
import org.eclipse.jdt.core.JavaCore;
import org.jboss.tools.common.java.IParametedType;
import org.jboss.tools.common.java.ParametedType;
import org.jboss.tools.common.java.ParametedTypeFactory;
import org.jboss.tools.common.util.EclipseJavaUtil;
import org.jboss.tools.test.util.ResourcesUtils;
import org.junit.Before;
import org.junit.Test;

public class ParametedTypeTest {
	
	IProject project;

	@Before
	public void setUp() throws Exception {
		if(project == null) {
			project = importProject("JavaProject");
		}
	}

	private static IProject importProject(String name) throws Exception {
		IProject project = ResourcesUtils.importProject("org.jboss.tools.common.core.test", "projects/" + name);
		assertNotNull(project);
		assertTrue(project.exists());
		return project;
	}

	@Test
	public void testEquals() throws Exception {
		IJavaProject javaProject = JavaCore.create(project);
		assertNotNull(javaProject);

		IType dependencies = EclipseJavaUtil.findType(javaProject, "parameters.Dependencies");
		assertNotNull(dependencies);
		IType type4 = EclipseJavaUtil.findType(javaProject, "parameters.Dependencies.Type4");
		assertNotNull(type4);
		IType type5 = EclipseJavaUtil.findType(javaProject, "parameters.Dependencies.Type4.Type5");
		assertNotNull(type5);
		IField f2 = type4.getField("f2");
		assertNotNull(f2);
		
		ParametedTypeFactory factory = new ParametedTypeFactory();
		IParametedType ptype5 = factory.newParametedType(type5);
		assertNotNull(ptype5);
		System.out.println(f2.getTypeSignature());
		
		IParametedType pf2 = factory.getParametedType(type4, f2.getTypeSignature());
		
		try {
			assertTrue(pf2.equals(ptype5));
			assertTrue(ptype5.equals(pf2));
		} catch (StackOverflowError e) {
			fail("Stack overflow in ParametedType.equals().");
		}
	}

	@Test
	public void testIsAssignableTo() throws Exception {
		IJavaProject javaProject = JavaCore.create(project);
		assertNotNull(javaProject);

		IType dependencies = EclipseJavaUtil.findType(javaProject, "parameters.Dependencies");
		assertNotNull(dependencies);
		IType type1 = EclipseJavaUtil.findType(javaProject, "parameters.Dependencies.Type1");
		assertNotNull(type1);
		IType type2 = EclipseJavaUtil.findType(javaProject, "parameters.Dependencies.Type2");
		assertNotNull(type2);
		IType type3 = EclipseJavaUtil.findType(javaProject, "parameters.Dependencies.Type3");
		assertNotNull(type3);
		IField f1 = dependencies.getField("f1");
		assertNotNull(f1);
		
		ParametedTypeFactory factory = new ParametedTypeFactory();
		IParametedType pdependencies = factory.newParametedType(dependencies);
		ParametedType ptype1 = (ParametedType)factory.newParametedType(type1);
		assertNotNull(ptype1);
		ParametedType ptype2 = (ParametedType)factory.newParametedType(type2);
		assertNotNull(ptype2);
		ParametedType ptype3 = (ParametedType)factory.newParametedType(type3);
		assertNotNull(ptype3);
		System.out.println(f1.getTypeSignature());
		
		ParametedType pf1 = (ParametedType)factory.getParametedType(dependencies, f1.getTypeSignature());
		
		try {
			assertTrue(ptype1.isAssignableTo(pf1, true));
			assertTrue(ptype2.isAssignableTo(pf1, true));
			assertTrue(ptype3.isAssignableTo(pf1, true));
		} catch (StackOverflowError e) {
			fail("Stack overflow in ParametedType.isAssignableTo().");
		}
	}

}
