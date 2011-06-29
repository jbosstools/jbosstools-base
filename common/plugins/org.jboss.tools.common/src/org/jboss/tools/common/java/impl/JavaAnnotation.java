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
package org.jboss.tools.common.java.impl;

import org.eclipse.core.resources.IResource;
import org.eclipse.jdt.core.IAnnotation;
import org.eclipse.jdt.core.IMember;
import org.eclipse.jdt.core.IMemberValuePair;
import org.eclipse.jdt.core.ISourceRange;
import org.eclipse.jdt.core.IType;
import org.eclipse.jdt.core.JavaModelException;
import org.jboss.tools.common.CommonPlugin;
import org.jboss.tools.common.java.IJavaAnnotation;
import org.jboss.tools.common.util.EclipseJavaUtil;

public class JavaAnnotation implements IJavaAnnotation {
	IAnnotation annotation;

	String annotationTypeName;
	IType type;

	public JavaAnnotation(IAnnotation annotation, IType declaringType) {
		this.annotation = annotation;
		try {
			String name = annotation.getElementName();
			annotationTypeName = EclipseJavaUtil.resolveType(declaringType, name);
			type = EclipseJavaUtil.findType(annotation.getJavaProject(), annotationTypeName);
		} catch (JavaModelException e) {
			CommonPlugin.getDefault().logError(e);
		}
	}

	public IResource getResource() {
		return annotation.getResource();
	}

	public String getTypeName() {
		return annotationTypeName;
	}

	public IType getType() {
		return type;
	}

	public int getLength() {
		try {
			ISourceRange range = annotation.getSourceRange();
			if(range != null) {
				return range.getLength();
			}
		} catch (JavaModelException e) {
			CommonPlugin.getDefault().logError(e);
		}
		return 0;
	}

	public int getStartPosition() {
		try {
			ISourceRange range = annotation.getSourceRange();
			if(range != null) {
				return range.getOffset();
			}
		} catch (JavaModelException e) {
			CommonPlugin.getDefault().logError(e);
		}
		return 0;
	}

	public IMember getParentMember() {
		return (IMember)annotation.getParent();
	}

	public IMemberValuePair[] getMemberValuePairs() {
		try {
			return annotation.getMemberValuePairs();
		} catch (JavaModelException e) {
			CommonPlugin.getDefault().logError(e);
		}
		return new IMemberValuePair[0];
	}

	public IAnnotation getAnnotation() {
		return annotation;
	}

}
