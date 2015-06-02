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

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
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
	public void testSubtypesInJar() throws Exception {
		IFile f = project.getFile("lib/weld-se-1.1.10.Final.jar");
		assertTrue(f.exists());
		List<String> classes = new ArrayList<String>();
		classes.add("java.lang.Object");
		List<String> interfaces = new ArrayList<String>();
		boolean b = JandexUtil.hasSubtypes(f.getLocation().toFile(), classes, interfaces);
		assertTrue(b);
	}

	@Test
	public void testAnnotationInJar() throws Exception {
		IFile f = project.getFile("lib/weld-se-1.1.10.Final.jar");
		assertTrue(f.exists());
		JandexUtil.IAnnotationCheck check = new JandexUtil.IAnnotationCheck() {
			@Override
			public boolean isRelevant(String annotationType) {
				return "java.lang.Deprecated".equals(annotationType);
			}
		};
		boolean b =  JandexUtil.hasAnnotation(f.getLocation().toFile(), check);
		assertTrue(b);
	}
}
