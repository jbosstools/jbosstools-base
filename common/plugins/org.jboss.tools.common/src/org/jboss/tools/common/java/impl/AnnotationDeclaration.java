/******************************************************************************* 
 * Copyright (c) 2009-2011 Red Hat, Inc. 
 * Distributed under license by Red Hat, Inc. All rights reserved. 
 * This program is made available under the terms of the 
 * Eclipse Public License v1.0 which accompanies this distribution, 
 * and is available at http://www.eclipse.org/legal/epl-v10.html 
 * 
 * Contributors: 
 * Red Hat, Inc. - initial API and implementation 
 ******************************************************************************/ 
package org.jboss.tools.common.java.impl;

import org.eclipse.core.resources.IResource;
import org.eclipse.jdt.core.IAnnotation;
import org.eclipse.jdt.core.IMember;
import org.eclipse.jdt.core.IMemberValuePair;
import org.eclipse.jdt.core.IType;
import org.jboss.tools.common.java.IAnnotationDeclaration;
import org.jboss.tools.common.java.IAnnotationType;
import org.jboss.tools.common.java.IJavaAnnotation;
import org.jboss.tools.common.java.impl.JavaAnnotation;

/**
 * 
 * @author Viacheslav Kabanovich
 *
 */
public class AnnotationDeclaration implements IAnnotationDeclaration {
	protected IJavaAnnotation annotation;

	public AnnotationDeclaration() {}

	public void setDeclaration(IJavaAnnotation annotation) {
		this.annotation = annotation;
	}

	public IJavaAnnotation getDeclaration() {
		return annotation;
	}

	public IResource getResource() {
		return annotation.getResource();
	}

	public IMemberValuePair[] getMemberValuePairs() {
		return annotation.getMemberValuePairs();
	}

	public Object getMemberValue(String name) {
		if(name == null) name = "value";
		IMemberValuePair[] pairs = getMemberValuePairs();
		if(pairs != null) {
			for (IMemberValuePair pair: pairs) {
				if(name.equals(pair.getMemberName())) {
					return pair.getValue();
				}
			}
		}
		return null;
	}

	public IMember getParentMember() {
		return annotation.getParentMember();
	}

	public String getTypeName() {
		return annotation.getTypeName();
	}

	public IType getType() {
		return annotation.getType();
	}

	public int getLength() {
		return annotation.getLength();
	}

	public int getStartPosition() {
		return annotation.getStartPosition();
	}

	public IAnnotationType getAnnotation() {
		return null;
	}

	public IAnnotation getJavaAnnotation() {
		if(annotation instanceof JavaAnnotation) {
			return ((JavaAnnotation) annotation).getAnnotation();
		}
		return null;
	}
}