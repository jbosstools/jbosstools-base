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
import org.eclipse.jdt.core.IAnnotation;
import org.eclipse.jdt.core.IJavaElement;
import org.eclipse.jdt.core.IMember;
import org.eclipse.jdt.core.IMemberValuePair;
import org.eclipse.jdt.core.IMethod;
import org.eclipse.jdt.core.ISourceReference;
import org.eclipse.jdt.core.IType;
import org.jboss.tools.common.core.CommonCorePlugin;
import org.jboss.tools.common.java.IAnnotationDeclaration;
import org.jboss.tools.common.java.IAnnotationType;
import org.jboss.tools.common.java.IJavaAnnotation;

/**
 * 
 * @author Viacheslav Kabanovich
 *
 */
public class AnnotationDeclaration implements IAnnotationDeclaration {
	public static final String VALUE = "value"; //$NON-NLS-1$

	protected IJavaAnnotation annotation;
	protected IValues values = EmptyValues.instance;
	protected IValues constants = EmptyValues.instance;
	protected IValues defaults = EmptyValues.instance;

	public AnnotationDeclaration() {}

	public void setDeclaration(IJavaAnnotation annotation) {
		this.annotation = annotation;
		IMemberValuePair[] pairs = getMemberValuePairs();
		if(pairs.length > 0) {
			ValueResolver r = new ValueResolver(getJavaAnnotation());
			for (IMemberValuePair pair: pairs) {
				String name = pair.getMemberName();
				Object value = r.resolvePair(pair);
				if(value != null) {
					values = values.put(name, value);
				}
				if(r.getConstant() != null) {
					constants = constants.put(name, r.getConstant());
				}
			}
			r.dispose();
		}
		try {
			loadDefaults();
		} catch (CoreException e) {
			CommonCorePlugin.getDefault().logError(e);
		}
	}

	private void loadDefaults() throws CoreException {
		IType type = getType();
		if(type == null) {
			return;
		}
		IMethod[] ms = type.getMethods();
		for (IMethod m: ms) {
			String n = m.getElementName();
			IMemberValuePair p = m.getDefaultValue();
			if (p != null) {
				n = p.getMemberName();
				Object o = p.getValue();
				// Default value can be null since JDT does not computes complex values
				// E.g. values (char)7 or (2 + 3) will be resolved to null.
				if(o == null) {
					String expression = null;
					String s = ((ISourceReference)m).getSource();
					if(s != null) {
						int i = s.indexOf("default");
						int j = s.lastIndexOf(';');
						if(i > 0 && j > i) {
							expression = s.substring(i + 7, j).trim();
						}
					}
					if(expression != null) {
						ValueResolver r = new ValueResolver(m);
						o = r.resolveExpression(expression);
						r.dispose();
					}
				}
				if(o != null) {
					defaults = defaults.put(n, o);
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

	/**
	 * Returns value 'as is', it may be a qualified name without resolving its value.
	 */
	public Object getMemberValue(String name) {
		if(name == null) name = VALUE;
		return values.get(name);
	}

	public Object getMemberConstantValue(String name) {
		if(name == null) name = VALUE;
		return constants.get(name);
	}

	/**
	 * For a value resolved to a field reference, tries to get its value.
	 * If it cannot be resolved, 'as is' value is returned.
	 * @param name
	 * @param resolve
	 * @return
	 */
	public Object getMemberValue(String name, boolean resolve) {
		Object result = null;
		if(resolve) {
			result = getMemberConstantValue(name);
		}
		if(result == null) {
			result = getMemberValue(name);
		}
		return result;
	}

	public Object getMemberDefaultValue(String name) {
		if(name == null) name = VALUE;
		return defaults.get(name);
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