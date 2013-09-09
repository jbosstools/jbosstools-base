/*******************************************************************************
 * Copyright (c) 2013 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributor:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/
package org.jboss.tools.common.model.test;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.jdt.core.IAnnotation;
import org.eclipse.jdt.core.IField;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.core.IType;
import org.jboss.tools.common.EclipseUtil;
import org.jboss.tools.common.java.IJavaAnnotation;
import org.jboss.tools.common.java.impl.AnnotationDeclaration;
import org.jboss.tools.common.java.impl.JavaAnnotation;
import org.jboss.tools.common.util.EclipseJavaUtil;

import junit.framework.TestCase;

public class AnnotationTest extends TestCase {
	protected static String PLUGIN_ID = "org.jboss.tools.cdi.core.test";

	static String ATTR_VALUE = "value";
	static String ATTR_NAME = "name";
	static String ATTR_AGE = "age";

	IProject project = null;
	IJavaProject javaProject;
	IType type;
	String fileName = "src/test/a/Test.java";

	public AnnotationTest() {
	}

	@Override
	public void setUp() throws Exception {
		project = ResourcesPlugin.getWorkspace().getRoot().getProject("AnnotationTest");
		javaProject = EclipseUtil.getJavaProject(project);
		type = EclipseJavaUtil.findType(javaProject, "test.Test");
	}

	public void testDefaults() throws Exception {
		AnnotationDeclaration d = getAnnotationDeclaration("f1");
		assertNull(d.getMemberConstantValue(ATTR_VALUE));
		assertEquals(0, d.getMemberDefaultValue(ATTR_VALUE));
		assertNull(d.getMemberConstantValue(ATTR_NAME));
		assertEquals("xy", d.getMemberDefaultValue(ATTR_NAME));
		assertNull(d.getMemberConstantValue(ATTR_AGE));
		assertEquals(5, d.getMemberDefaultValue(ATTR_AGE));
	}

	public void testSum() throws Exception {
		AnnotationDeclaration d = getAnnotationDeclaration("f3");
		assertEquals(38, d.getMemberConstantValue(ATTR_VALUE));
	}

	public void testSumWithReference() throws Exception {
		AnnotationDeclaration d = getAnnotationDeclaration("f4");
		assertEquals(28, d.getMemberConstantValue(ATTR_VALUE));
	}

	public void testSumOfReferences() throws Exception {
		AnnotationDeclaration d = getAnnotationDeclaration("f5");
		assertEquals(12, d.getMemberConstantValue(ATTR_VALUE));
	}

	public void testManyPairs() throws Exception {
		AnnotationDeclaration d = getAnnotationDeclaration("f7");
		assertEquals("Ac1", d.getMemberConstantValue(ATTR_NAME));
		assertEquals(15, d.getMemberConstantValue(ATTR_AGE));
	}

	public void testComplexExpression() throws Exception {
		AnnotationDeclaration d = getAnnotationDeclaration("f8");
		assertEquals(17, d.getMemberConstantValue(ATTR_AGE));
	}

	AnnotationDeclaration getAnnotationDeclaration(String fieldName) {
		return getAnnotationDeclaration(fieldName, "MyAnnotation");
	}

	AnnotationDeclaration getAnnotationDeclaration(String fieldName, String annotationTypeName) {
		IAnnotation a = findAnnotation(fieldName, annotationTypeName);
		IJavaAnnotation ja = new JavaAnnotation(a, type);
		AnnotationDeclaration d = new AnnotationDeclaration();
		d.setDeclaration(ja);
		return d;
	}

	IAnnotation findAnnotation(String fieldName, String annotationTypeName) {
		IField field = type.getField(fieldName);
		assertTrue(field.exists());
		IAnnotation annotation = field.getAnnotation(annotationTypeName);
		assertTrue(annotation.exists());
		return annotation;
	}

}
