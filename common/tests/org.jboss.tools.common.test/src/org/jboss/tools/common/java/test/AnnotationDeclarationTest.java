/******************************************************************************* 
 * Copyright (c) 2011 Red Hat, Inc. 
 * Distributed under license by Red Hat, Inc. All rights reserved. 
 * This program is made available under the terms of the 
 * Eclipse Public License v1.0 which accompanies this distribution, 
 * and is available at http://www.eclipse.org/legal/epl-v10.html 
 * 
 * Contributors: 
 * Red Hat, Inc. - initial API and implementation 
 ******************************************************************************/ 
package org.jboss.tools.common.java.test;

import org.jboss.tools.common.java.IJavaAnnotation;
import org.jboss.tools.common.java.impl.AnnotationDeclaration;
import org.jboss.tools.common.java.impl.JavaAnnotation;

import junit.framework.TestCase;

/**
 * 
 * @author Viacheslav Kabanovich
 *
 */
public class AnnotationDeclarationTest extends TestCase {
	
	public void testNullValue() {
		AnnotationDeclaration d = new AnnotationDeclaration();
		IJavaAnnotation a = new JavaAnnotation(new FakeAnnotationImpl(), null);
		d.setDeclaration(a);
		Object o = d.getMemberValue("value");
		System.out.println(o);
	}

}
