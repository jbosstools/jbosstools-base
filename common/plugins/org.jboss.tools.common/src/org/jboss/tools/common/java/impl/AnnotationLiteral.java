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
import org.eclipse.jdt.core.IMember;
import org.eclipse.jdt.core.IMemberValuePair;
import org.eclipse.jdt.core.ISourceRange;
import org.eclipse.jdt.core.IType;
import org.eclipse.jdt.core.SourceRange;
import org.eclipse.jdt.internal.core.MemberValuePair;
import org.jboss.tools.common.java.IJavaAnnotation;

public class AnnotationLiteral implements IJavaAnnotation {
	IResource declaringResource;
	IType annotationType;

	String source;
	ISourceRange range;
	IMemberValuePair[] memberValues = new IMemberValuePair[0];

	public AnnotationLiteral(IResource declaringResource, String source, ISourceRange range, IMemberValuePair[] memberValues, IType annotationType) {
		this.declaringResource = declaringResource;
		this.source = source;
		this.range = range;
		this.memberValues = memberValues;
		this.annotationType = annotationType;
	}

	public AnnotationLiteral(IResource declaringResource, int offset, int length, Object value, int valueType, IType annotationType) {
		this.declaringResource = declaringResource;
		this.range = new SourceRange(offset, length);
		if(value != null) {
			this.memberValues = new IMemberValuePair[]{
				createPair("value", value, valueType)	
			};
		}
		this.annotationType = annotationType;
	}

	public void addMemberValuePair(String name, Object value, int valueType) {
		IMemberValuePair[] pairs = new IMemberValuePair[memberValues.length + 1];
		System.arraycopy(memberValues, 0, pairs, 0, memberValues.length);
		pairs[memberValues.length] = createPair(name, value, valueType);
		memberValues = pairs;
	}

	private IMemberValuePair createPair(String name, Object value, int valueType) {
		return new MemberValuePair(name, value, valueType);
	}

	public int getStartPosition() {
		return range == null ? -1 : range.getOffset();
	}

	public int getLength() {
		return range == null ? -1 : range.getLength();
	}

	public IResource getResource() {
		return declaringResource;
	}

	public String getTypeName() {
		if(annotationType != null) return annotationType.getFullyQualifiedName();
		return null;
	}

	public IType getType() {
		return annotationType;
	}

	public IMember getParentMember() {
		//Do we need it?
		return null;
	}

	public IMemberValuePair[] getMemberValuePairs() {
		return memberValues;
	}
	
}
