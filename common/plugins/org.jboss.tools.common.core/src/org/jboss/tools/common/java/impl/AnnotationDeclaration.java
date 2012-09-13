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

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jdt.core.Flags;
import org.eclipse.jdt.core.IAnnotation;
import org.eclipse.jdt.core.ICompilationUnit;
import org.eclipse.jdt.core.IField;
import org.eclipse.jdt.core.IImportDeclaration;
import org.eclipse.jdt.core.IJavaElement;
import org.eclipse.jdt.core.IMember;
import org.eclipse.jdt.core.IMemberValuePair;
import org.eclipse.jdt.core.IType;
import org.jboss.tools.common.core.CommonCorePlugin;
import org.jboss.tools.common.java.IAnnotationDeclaration;
import org.jboss.tools.common.java.IAnnotationType;
import org.jboss.tools.common.java.IJavaAnnotation;
import org.jboss.tools.common.util.EclipseJavaUtil;

/**
 * 
 * @author Viacheslav Kabanovich
 *
 */
public class AnnotationDeclaration implements IAnnotationDeclaration {
	protected IJavaAnnotation annotation;
	protected IValues values = EmptyValues.instance;

	public AnnotationDeclaration() {}

	public void setDeclaration(IJavaAnnotation annotation) {
		this.annotation = annotation;
		IMemberValuePair[] pairs = getMemberValuePairs();
		if(pairs.length > 0) {
			for (IMemberValuePair pair: pairs) {
				String name = pair.getMemberName();
				Object value = resolveMemberValue(pair);
				if(value != null) {
					values = values.put(name, value);
				}
			}
		}
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
		if(name == null) name = "value"; //$NON-NLS-1$
//		IMemberValuePair[] pairs = getMemberValuePairs();
//		for (IMemberValuePair pair: pairs) {
//			if(name.equals(pair.getMemberName())) {
//				return resolveMemberValue(pair);
//			}
//		}
		return values == null ? null : values.get(name);
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

	public Object resolveMemberValue(IMemberValuePair pair) {
		Object value = pair.getValue();
		int k = pair.getValueKind();
		if(k == IMemberValuePair.K_QUALIFIED_NAME || k == IMemberValuePair.K_SIMPLE_NAME
			|| (value instanceof Object[] && k == IMemberValuePair.K_UNKNOWN)) {
			IAnnotation a = getJavaAnnotation();
			if(a != null && a.getAncestor(IJavaElement.COMPILATION_UNIT) instanceof ICompilationUnit) {
				value = validateNamedValue(value, a);
			}
		}
		return value;
	}

	private Object validateNamedValue(Object value, IAnnotation a) {
		if(value instanceof Object[]) {
			Object[] vs = (Object[])value;
			for (int i = 0; i < vs.length; i++) {
				vs[i] = validateNamedValue(vs[i], a);
			}
		} else {
			ICompilationUnit u = (ICompilationUnit)a.getAncestor(IJavaElement.COMPILATION_UNIT);
			IType type = (IType)a.getAncestor(IJavaElement.TYPE);
			try {
				IImportDeclaration[] is = u.getImports();
				String stringValue = value.toString();
				int lastDot = stringValue.lastIndexOf('.');
				String lastToken = stringValue.substring(lastDot + 1);
				if(lastDot < 0) {
					IField f = (a.getParent() == type) ? type.getField(lastToken) : EclipseJavaUtil.findField(type, lastToken);
					if(f != null && f.exists()) {
						value = f.getDeclaringType().getFullyQualifiedName() + "." + lastToken;
					} else {
						String v = getFullName(type, is, lastToken);
						if(v != null) {
							value = v;
						}
					}
					return value;
				}
				String prefix = stringValue.substring(0, lastDot);
				String t = EclipseJavaUtil.resolveType(type, prefix);
				if(t != null) {
					IType q = EclipseJavaUtil.findType(type.getJavaProject(), t);
					if(q != null && q.getField(lastToken).exists()) {
						value = t + "." + lastToken;
					}
				}
				
			} catch (CoreException e) {
				CommonCorePlugin.getDefault().logError(e);
			}
		}
		
		return value;
	}

	private String getFullName(IType type, IImportDeclaration[] is, String name) throws CoreException {
		for (IImportDeclaration d: is) {
			String n = d.getElementName();
			if(n.equals(name) || n.endsWith("." + name)) {
				return n;
			}
			if(Flags.isStatic(d.getFlags()) && n.endsWith(".*")) {
				String typename = n.substring(0, n.length() - 2);
				IType t = EclipseJavaUtil.findType(type.getJavaProject(), typename);
				if(t != null && t.exists()) {
					IField f = EclipseJavaUtil.findField(t, name);
					if(f != null) {
						return f.getDeclaringType().getFullyQualifiedName() + "." + name;
					}
				}
				
			}
		}
		return null;
	}

	@Override
	public IMember getSourceMember() {
		return getParentMember();
	}

	@Override
	public IJavaElement getSourceElement() {
		return annotation.getParentElement();
	}
}

interface IValues {
	Object get(String name);
	IValues put(String name, Object value);
}

class EmptyValues implements IValues {
	static EmptyValues instance = new EmptyValues();
	public Object get(String name) {
		return null;
	}
	public IValues put(String name, Object value) {
		return ("value".equals(name)) ? new Value(value) : new Values(name, value);
	}
}

class Value implements IValues {
	Object value;
	Value(Object value) {
		this.value = value;
	}

	public Object get(String name) {
		return "value".equals(name) ? value : null;
	}

	public IValues put(String name, Object value) {
		Values values = new Values("value", this.value);
		values.put(name, value);
		return values;
	}
}

class Values implements IValues {
	List<String> names = new ArrayList<String>(2);
	List<Object> values = new ArrayList<Object>(2);

	public Values(String name, Object value) {
		names.add(name);
		values.add(value);
	}
	public Object get(String name) {
		for (int i = 0; i < names.size(); i++) {
			if(name.equals(names.get(i))) return values.get(i);
		}
		return null;
	}

	public IValues put(String name, Object value) {
		names.add(name);
		values.add(value);
		return this;
	}
}